% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0,send_job/3]).
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
  CUsers = genserver:request(list_to_atom(Server), get_all_users),
  io:fwrite("Reponse in cchat: ~p~n", [CUsers]),
  case CUsers of
    [] ->
      "No connected users, can't complete";
    Array ->
      FunctionArray = lists:duplicate(length(Argument) ,Function),
      Tasks = [ Task || Task <- lists:zip(FunctionArray, Argument)],
      spread_tasks(assign_tasks(CUsers, Tasks)),
  end.



spread_tasks(Array) ->
  Pid = self(),
  Refs = ,
  [spawn (fun() -> Pid ! genserver:request(Pid,{calculate,Task,Ref}) end) || {Pid,Task} <- Array].


assign_tasks([], _) -> [] ;
assign_tasks(Users, Tasks) ->
  [  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task} || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].
