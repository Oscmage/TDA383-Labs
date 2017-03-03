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
    CUsers = genserver:request(list_to_atom(Server), get_all_users),
    io:fwrite("Reponse in cchat: ~p~n", [CUsers]),
    case CUsers of
      [] ->
        "No connected users, can't complete";
      _ ->
        List_TaskUser = assign_tasks(CUsers, Argument),
        spread_tasks(Function, List_TaskUser),
        retrieve_result(List_TaskUser)
    end
  catch
    _:_ -> "Server unreachable"
  end.

retrieve_result (List_TaskUser) ->
  [ retrieve(Ref) || {_, _, Ref} <- List_TaskUser].

retrieve(Ref) ->
  receive
    {done, Result, Ref} ->
      Result;
    {_, Ref} ->
      invalid_input
  end.

% delegate tasks to my clients
spread_tasks(Function, List_TaskUser) ->
  Pid = self(),
  [ spawn (fun() -> Pid ! genserver:request(User, {do_task, Task, Function, Ref}, infinity) end) || {User, Task, Ref} <- List_TaskUser].



assign_tasks([], _) -> [] ;
assign_tasks(Users, Tasks) ->
  [  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task, make_ref()} || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].
