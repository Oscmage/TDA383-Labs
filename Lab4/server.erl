-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{serverName = ServerName}.


%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, {connect, Nick, Pid}) ->
    UserConnected = lists:keymember(Pid, 2, St#server_st.cUsers),
    if UserConnected == true ->
        {reply, {error, user_already_connected, "User is already connected"}, St};
      true -> % else
        NickTaken = lists:keymember(Nick, 1, St#server_st.cUsers),
        case NickTaken of
            true -> % Nick taken by other user
                {reply, nick_taken, St};
            false -> % Nick not taken free to take
                NewState = St#server_st{cUsers = [{Nick, Pid}] ++ St#server_st.cUsers},
                {reply, user_is_connected, NewState}
        end
    end;

handle(St, {disconnect,Nick,Pid}) ->
    NewState = St#server_st{cUsers = lists:delete({Nick,Pid}, St#server_st.cUsers)}, % rmove the user from the users list
    {reply,ok, NewState}; % Send ok and use the updated state


handle(St, {join,Channel,PID}) ->
    ChannelAtom = list_to_atom(St#server_st.serverName ++ Channel),
    ChannelPID = whereis(ChannelAtom),
    % Does a process with this atom exists?
    case ChannelPID of
        undefined ->
            % Start a new process with a combination of the server name and the channel name
            genserver:start(ChannelAtom, channel:initial_state(Channel), fun channel:handle/2);
        _ ->
            ok
    end,
    {reply, genserver:request(ChannelAtom,{join, PID}), St}; % Is okay since client does request in try catch block.

handle(St, get_all_users) ->
  io:fwrite("Get all users: ~p~n", [St#server_st.cUsers]),
  {reply, [Pid || {_, Pid} <- St#server_st.cUsers], St};

handle(St, Request) ->
    io:fwrite("In server.erl, Shouldn't have gotten here, this is a debugg message: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, Response, St}.
