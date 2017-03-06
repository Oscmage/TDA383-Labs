% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0, send_job/3]).
-include_lib("./defs.hrl").

%% Start a server
server() ->
    Server = "shire",
    genserver:start(list_to_atom(Server), server:initial_state(Server), fun server:handle/2).

%% Start a client GUI
client() ->
    gui:start().

%% Start local server and one client
start() ->
    server(),
    client().

%% Start local server and two clients
start2() ->
    server(),
    client(),
    client().

send_job(Server, Function, Argument) ->
  try
    Ref = make_ref(),
    Pid = self(),
    genserver:request(list_to_atom(Server), {send_job, Function, Argument, Ref, Pid}, infinity),
    io:fwrite("CChat, wait for response: ~p~n", [Function]),
    wait_for_response(Ref)
  catch
    _:_ -> "Server unreachable"
  end.

% retrieve_result (List_TaskUser) ->
%   [ retrieve(Ref) || {_, _, Ref} <- List_TaskUser].
%
% retrieve(Ref) ->
%   receive
%     {done, Result, Ref} ->
%       Result;
%     {_, Ref} ->
%       invalid_input
%   end.

wait_for_response (Ref) ->
  receive
    {done, Result, Ref} ->
      Result
  end.
