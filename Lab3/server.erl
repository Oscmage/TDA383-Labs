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
    Connected = lists:keymember(Pid, 2, St#server_st.cUsers),
    if Connected == true ->
        {reply, {error,user_already_connected,"User is already connected"}, St};
      true -> % else
        NickTaken = lists:keymember(Nick, 1, St#server_st.cUsers),
        case NickTaken of
            true -> % Nick taken by other user
                {reply, nick_taken, St};
            false -> % Nick not taken free to take
                NewState = St#server_st{cUsers=[{Nick, Pid}]++St#server_st.cUsers},
                {reply, user_is_connected, NewState}
        end
    end;

handle(St, {disconnect,Nick,Pid}) ->
    {reply,ok, St#server_st{cUsers = lists:delete({Nick,Pid}, St#server_st.cUsers)}}; % Send ok, and remove the nick from the users.


handle(St, {join,Channel,PID}) ->
    io:fwrite("Someone wants to join a channel: ~p~n", [St]),
    ChannelAtom = list_to_atom(St#server_st.serverName ++ Channel),
    ChannelPID = whereis(ChannelAtom),
    case ChannelPID of
        undefined ->
            genserver:start(ChannelAtom, channel:initial_state(Channel), fun channel:handle/2);
        _ ->
            ok
    end,
    {reply, genserver:request(ChannelAtom,{join, PID}), St};

handle(St, Request) ->
    io:fwrite("In server.erl, Shouldn't have gotten here, derp: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, Response, St}.
