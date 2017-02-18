-module(channel).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ChannelName) ->
    #channel_st{name = ChannelName}.

handle(St,{join,Nick,PID}) ->
    case lists:member(Nick,St#channel_st.cUsers) of
        true -> 
            {reply,user_already_joined,St};
        false ->
            NewState = St#channel_st{cUsers = [Nick] ++ St#channel_st.cUsers},
            {reply,joined,NewState}
    end;

handle(St,{leave,Nick,PID}) ->
    case lists:member(Nick,St#channel_st.cUsers) of
        true ->
            NewList = lists:delete(Nick,St#channel_st.cUsers),
            NewState = St#channel_st{cUsers = NewList},
            {reply, left, NewState};
        false ->
            {reply, user_not_joined, St}
    end;

handle(St, Request) ->
    io:fwrite("Shouldn't have gotten here, derp: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, joined, St}.
