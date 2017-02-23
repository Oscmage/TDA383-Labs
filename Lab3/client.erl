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
%  Connect a user to a specific server by requesting it from genserver
%  Response is a atom that receives the request data
%  Different Response sent different messages and states to the requesting process
handle(St, {connect, Server}) ->
  try
    Response = genserver:request(list_to_atom(Server), {connect, St#client_st.nick, self()}),
    case Response of
        user_already_connected -> %Requesting data states that the user is already connected to this server
          {reply, {error,user_already_connected,"User is already connected"}, St};
        nick_taken -> %Requesting data/Response states that the nick is already taken, therefore not able to connect with the same nick
            {reply,{error,nick_taken,"Someone with this nick is already connected"},St};
        user_is_connected -> %Response states that the user has been allowed to connect to the server
            io:fwrite("Connected to server: ~p~n", [St]),
            {reply, ok, St#client_st{server = list_to_atom(Server)}}
    end
  catch
     _:_ -> {reply,{error,server_not_reached,"Server unreachable in connect-process"},St}
  end;


%% Disconnect from server
% Disconnet a user to a server by first making sure that the user is connected to this server
% Then it ensures that the user is not a member of any chatrooms
handle(St, disconnect) ->
    case St#client_st.server =:= '' of %Checks if the user is connected to this server
        true -> {reply,{error,user_not_connected,"Can't disconnect from a server that you aren't connected to"}, St};
        false ->
            case St#client_st.chatrooms =:= [] of %Checks if the user has left all the channels
                true ->
                    try
                        case genserver:request(St#client_st.server,{disconnect, St#client_st.nick, self()}) of
                            ok ->
                                {reply,ok,St#client_st{server = ''}}
                        end
                    catch
                          _:_ -> {reply,{error,server_not_reached,"Server unreachable in disconnect-process"},St}
                    end;
                false -> {reply,{error,leave_channels_first,"Should leave channels first"},St}
            end
    end;

%% Join channel
% Join a user to a channel by first checking if the user is connected to a server
% Then it receives request data in order to determine if the user can join or not
handle(St, {join, Channel}) ->
    if
        St#client_st.server == '' ->
            {reply, {error, server_not_reached, "Server is disconnected"}, St};
        true ->
            try
                Response = genserver:request(St#client_st.server, {join, Channel, self()}),
                case Response of
                    joined ->
                        NewSt = St#client_st{chatrooms = St#client_st.chatrooms ++ [Channel]},
                        {reply, ok, NewSt};
                    user_already_joined ->
                        {reply, {error, user_already_joined, "User has already joined this chat room"}, St}
                end
            catch
                _:_ -> {reply,{error,server_not_reached,"Server unreachable in join-process"},St}
            end
    end;

%% Leave channel
% Letting the user leaves a channel if there is a server connected to this user,
% Then it checks if the server is a member of the channel that the user wants to leave from
% If the user leaves this channel, then a new state will be sent in order to replace the old state with a new state with new changes
handle(St, {leave, Channel}) ->
    case St#client_st.server of
        '' ->
            {reply, {error, server_not_reached, "Server is disconnected"}, St};
        _ ->
            ChannelAtom = list_to_atom(atom_to_list(St#client_st.server) ++ Channel),
            try
              Response = genserver:request(ChannelAtom, {leave, self()}),
              case Response of
                  user_not_joined ->
                      {reply, {error, user_not_joined, "Not possible to leave from other chatrooms but the current one"}, St};
                  left ->
                      NewSt = St#client_st{chatrooms = lists:delete(Channel, St#client_st.chatrooms)},
                      {reply, ok, NewSt}
              end
            catch
              _:_ -> {reply,{error,server_not_reached,"Server unreachable in leave-process"},St}
            end
    end;

%% Sending messages
% If the user is not a member of this chatroom, then the user can't send messages and an error message will be shown
% If the user is a member, then a request will be received in order to send the message
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
% Replies with the old state/current state and the nick of the current user
handle(St, whoami) ->
    Nick = atom_to_list(St#client_st.nick),
    {reply, Nick, St} ;

%% Change nick
% Checks if the user is connected to a server if then an error message will be shown since it is not possible to change nick when connected
% If not, then the user will be updated with the new nick and a new state with the new nick will replace the old state
handle(St, {nick, Nick}) ->
  if St#client_st.server == '' ->
      {reply, ok,St#client_st{nick=list_to_atom(Nick)}};
    true -> % else
      {reply, {error, user_already_connected, "Not possible to change nick when connected to the server"}, St}
  end;


%% Incoming message
%  Receives incoming messages and shows it
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.

%% Retrieves the channel PID by convention from the server.erl
