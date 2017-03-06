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

%% Send_jobs call send_job in server in order to distribute all jobs to all clients connected to the server
% Then it calls wait_for_response, a function that waits for responses and returns the result
% If an exception is thrown from genserver then it will print out "Server is unreachable"
send_job(Server, Function, Argument) ->
  try
    Ref = make_ref(),
    Pid = self(),
    genserver:request(list_to_atom(Server), {send_job, Function, Argument, Ref, Pid}, infinity),
    wait_for_response(Ref)
  catch
    _:_ -> "Server is unreachable"
  end.

%% wait_for_response waits until it receives a response in the form of {done, Result, Ref} and it returns Result
wait_for_response (Ref) ->
  receive
    {done, Result, Ref} ->
      Result
  end.
