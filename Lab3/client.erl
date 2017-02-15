-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { gui = GUIName, nick = Nick, connected  = false, chatrooms = []}.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
    Data = "hello?",
    io:fwrite("Client is sending: ~p~n", [Data]),
    ServerAtom = list_to_atom(Server),
    Response = genserver:request(ServerAtom, Data),
    io:fwrite("Client received: ~p~n", [Response]),
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Connect to server - Not implemented"}, St} ;

%% Disconnect from server
handle(St, disconnect) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Disconnect from server - Not implemented"}, St} ;

% Join channel
handle(St, {join, Channel}) ->
  % Not done and tested, have to add things to server.erl
    case St#client_st.connected of
      false ->
        Response = genserver:request(St#client_st.serverAtom, {join, Channel, St#client_st.nick, self()}),
        case Response of
          user_already_joined -> {reply, {error, user_already_joined, "User has already joined this chat room"}, St};
          joined ->
            NewSt = St#client_st{chatrooms = St#client_st.chatrooms ++ [Channel]},
            {reply, ok, NewSt}
        end;
        %försöka joina en channel
        %se vad jag få för meddelande om user_already
        %annars joina
      true -> {reply, {error, not_implemented, "Server is disconnected"}, St}
      %{reply, {error, not_implemented, "Server is disconnected"}, St}
    end;

%% Leave channel
handle(St, {leave, Channel}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Leave Channel - Not implemented"}, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Sending messages - Not implemented"}, St} ;

%% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
  if
    St#client_st.connected == true ->
      {reply, ok,St#client_st{nick=Nick}};
    true ->
      {reply, {error, not_implemented, "Not possible to change nick when connected to the server"}, St}
  end;


%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
