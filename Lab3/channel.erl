-module(channel).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ChannelName) ->
    #channel_st{name = ChannelName}.


handle(St, {join, PID}) ->
    AlreadyInChannel = lists:member(PID, St#channel_st.cUsers),
    case AlreadyInChannel of
        true ->
            {reply, user_already_joined, St};
        false ->
            NewState = St#channel_st{cUsers = [PID] ++ St#channel_st.cUsers}, % Add new user
            {reply, joined, NewState}
    end;

handle(St, {leave, PID}) ->
    InChannel = lists:member(PID,St#channel_st.cUsers),
    case InChannel of
        true ->
            NewList = lists:delete(PID, St#channel_st.cUsers),
            NewState = St#channel_st{cUsers = NewList},
            {reply, left, NewState};
        false ->
            {reply, user_not_joined, St}
    end;

handle(St, {message, Msg, Nick, PID}) ->
    % Sends the message to all other users
    [ sendMessage(E, Nick , St#channel_st.name, Msg) || E <- St#channel_st.cUsers, E /= PID],
    {reply, ok, St};

handle(St, Request) ->
    io:fwrite("In channel.erl, Shouldn't have gotten here, derp: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, joined, St}.

%Simple function that spawns a new process to send the message around.
sendMessage(PID, From, Channel, Msg) ->
    io:fwrite("Spreading message to: ~p~n", [PID]),
    spawn (fun() -> genserver:request(PID, {incoming_msg, Channel, atom_to_list(From), Msg}) end).
