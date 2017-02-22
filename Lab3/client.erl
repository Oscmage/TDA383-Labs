-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { gui = GUIName, nick = list_to_atom(Nick), server='', chatrooms=[]}.
%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
  try
    Response = genserver:request(list_to_atom(Server), {connect, St#client_st.nick, self()}),
    case Response of
        user_already_connected ->
          {reply, {error,user_already_connected,"User is already connected"}, St};
        nick_taken ->
            {reply,{error,nick_taken,"Someone with this nick is already connected"},St};
        user_is_connected ->
            io:fwrite("Connected to server: ~p~n", [St]),
            {reply, ok, St#client_st{server = list_to_atom(Server)}}
    end
  catch
     _:_ -> {reply,{error,server_not_reached,"Server unreachable4"},St}
  end;


%% Disconnect from server NOT WORKING AT CURRENT STAGE
handle(St, disconnect) ->
    case St#client_st.server =:= '' of
        true -> {reply,{error,user_not_connected,"Can't disconnect from a server that you aren't connected to"}, St};
        false ->
            case St#client_st.chatrooms =:= [] of
                true ->
                    try
                        case genserver:request(St#client_st.server,{disconnect, St#client_st.nick, self()}) of
                            ok ->
                                {reply,ok,St#client_st{server = ''}}
                        end
                    catch
                          _:_ -> {reply,{error,server_not_reached,"Server unreachable1"},St}
                    end;
                false -> {reply,{error,leave_channels_first,"Should leave channels first"},St}
            end
    end;

% Join channel
handle(St, {join, Channel}) ->
    AlreadyJoined = lists:member(Channel,St#client_st.chatrooms),
    if
        St#client_st.server == '' ->
            {reply, {error, not_implemented, "Server is disconnected"}, St};
        AlreadyJoined == true ->
            io:fwrite("Already connect to this channel: ~p~n", [St]),
            {reply, {error, user_already_joined, "User has already joined this chat room"}, St};
        true ->
            try
                Response = genserver:request(St#client_st.server, {join, Channel, self()}),
                case Response of
                    joined ->
                        NewSt = St#client_st{chatrooms = St#client_st.chatrooms ++ [Channel]},
                        {reply, ok, NewSt}
                end
            catch
                _:_ -> {reply,{error,server_not_reached,"Server unreachable2"},St}
            end
    end;

%% Leave channel
handle(St, {leave, Channel}) ->
    case St#client_st.server of
        '' ->
            {reply, {error, not_implemented, "Server is disconnected"}, St};
        _ ->
            ChannelAtom = list_to_atom(atom_to_list(St#client_st.server) ++ Channel),
            Response = genserver:request(ChannelAtom, {leave, self()}),
            case Response of
                user_not_joined ->
                    {reply, {error, user_not_joined, "Not possible to leave from other chatrooms but the current one"}, St};
                left ->
                    NewSt = St#client_st{chatrooms = lists:delete(Channel, St#client_st.chatrooms)},
                    {reply, ok, NewSt}
            end
    end;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    IsMember = lists:member(Channel,St#client_st.chatrooms),
    if
        IsMember == false ->
            {reply, {error, user_not_joined, "Can't send a message in a channel you're not in"},St};
        true ->
            ChannelAtom = list_to_atom(atom_to_list(St#client_st.server) ++ Channel),
            try
                genserver:request(ChannelAtom, {message, Msg, St#client_st.nick, self()}),
                {reply,ok,St}
            catch
                _:_ -> {reply,{error,server_not_reached,"Server unreachable3"},St}
            end
    end;

%% Get current nick
handle(St, whoami) ->
    Nick = atom_to_list(St#client_st.nick),
    {reply, Nick, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
  if St#client_st.server == '' ->
      {reply, ok,St#client_st{nick=list_to_atom(Nick)}};
    true -> % else
      {reply, {error, user_already_connected, "Not possible to change nick when connected to the server"}, St}
  end;


%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.

%% Retrieves the channel PID by convention from the server.erl
