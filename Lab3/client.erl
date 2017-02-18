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
    case St#client_st.server == '' of
        false -> 
            {reply, {error,user_already_connected,"User is already connected"},St};
        true -> 
            try 
                case genserver:request(list_to_atom(Server),{connect, St#client_st.nick}) of
                    user_already_connected -> 
                        {reply,{error,user_already_connected,"Someone with this nick is already connected"},St};
                    ok -> 
                        {reply,ok,St#client_st{server = list_to_atom(Server)}}
                end
            catch
                _ -> {reply,{error,server_not_reached,"Server unreachable"},St}
            end
    end;

%% Disconnect from server NOT WORKING AT CURRENT STAGE
handle(St, disconnect) ->
    case St#client_st.server =:= '' of
        true -> {reply,user_not_connected, St};
        false -> 
            case St#client_st.chatrooms =:= [] of
                true -> 
                    try 
                        case genserver:request(St#client_st.server,{disconnect, St#client_st.nick}) of
                            ok -> 
                                {reply,ok,St#client_st{server = ''}}
                        end
                    catch
                          _ -> {reply,{error,server_not_reached,"Server unreachable"},St}
                    end;
                false -> {reply,leave_channels_first,St}
            end
    end;

% Join channel
handle(St, {join, Channel}) ->
    AlreadyJoined = lists:member(Channel,St#client_st.chatrooms),
  % Not done and tested, have to add things to server.erl
    if
        St#client_st.server == '' -> 
            {reply, {error, not_implemented, "Server is disconnected"}, St};
        AlreadyJoined == true -> 
            io:fwrite("Already connect to this channel: ~p~n", [St]),
            {reply, {error, user_already_joined, "User has already joined this chat room"}, St};
        true -> 
            try 
                Response = genserver:request(St#client_st.server, {join, Channel, St#client_st.nick, self()}),
                case Response of
                    joined ->
                        NewSt = St#client_st{chatrooms = St#client_st.chatrooms ++ [Channel]},
                        {reply, ok, NewSt}
                end
            catch
                _ -> {reply,{error,server_not_reached,"Server unreachable"},St}
            end
    end;

%% Leave channel
handle(St, {leave, Channel}) ->
    case St#client_st.server of
        '' ->
            {reply, {error, not_implemented, "Server is disconnected"}, St};
        _ ->
            Response = genserver:request(St#client_st.server, {leave, Channel, St#client_st.nick, self()}),
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
            ChannelAtom = getChannelPid(St,Channel),
            try 
                genserver:request(ChannelAtom, {message, St#client_st.nick, self()}),{reply,ok,St}
            catch
                _ -> {reply,{error,server_not_reached,"Server unreachable"},St}
            end
    end;

%% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
  if St#client_st.server == '' ->
      {reply, ok,St#client_st{nick=list_to_atom(Nick)}};
    true -> % else
      {reply, {error, not_implemented, "Not possible to change nick when connected to the server"}, St}
  end;


%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.

%% Retrieves the channel PID by convention from the server.erl 
getChannelPid(St,Channel) ->
    list_to_atom(St#client_st.server ++ Channel).
