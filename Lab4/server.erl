-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{serverName = ServerName}.


%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, {connect, Nick, Pid}) ->
    UserConnected = lists:keymember(Pid, 2, St#server_st.cUsers),
    if UserConnected == true ->
        {reply, {error, user_already_connected, "User is already connected"}, St};
      true -> % else
        NickTaken = lists:keymember(Nick, 1, St#server_st.cUsers),
        case NickTaken of
            true -> % Nick taken by other user
                {reply, nick_taken, St};
            false -> % Nick not taken free to take
                NewState = St#server_st{cUsers = [{Nick, Pid}] ++ St#server_st.cUsers},
                {reply, user_is_connected, NewState}
        end
    end;

handle(St, {disconnect, Nick, Pid}) ->
    NewState = St#server_st{cUsers = lists:delete({Nick,Pid}, St#server_st.cUsers)}, % rmove the user from the users list
    {reply,ok, NewState}; % Send ok and use the updated state


handle(St, {join,Channel,PID}) ->
    ChannelAtom = list_to_atom(St#server_st.serverName ++ Channel),
    ChannelPID = whereis(ChannelAtom),
    % Does a process with this atom exists?
    case ChannelPID of
        undefined ->
            % Start a new process with a combination of the server name and the channel name
            genserver:start(ChannelAtom, channel:initial_state(Channel), fun channel:handle/2);
        _ ->
            ok
    end,
    {reply, genserver:request(ChannelAtom,{join, PID}), St}; % Is okay since client does request in try catch block.

handle(St, {done, Result, Ref}) ->
  io:fwrite("Server, done: ~p~n", [Result]),
  NewList = lists:add(Result, St#server_st.results),
  if
      length(NewList) =:= length(St#server_st.refs) ->
        Pid = element(2, St#server_st.respond_to),
        R = element(1, St#server_st.respond_to),
        SortedList = sort_by_pid(NewList, Ref),
        genserver:request(Pid, {done, SortedList, R}, infinity),
        {reply, ok, St#server_st{ respond_to={}, refs=[], results=[]}};
      true ->
        {reply, ok, St#server_st{results = NewList}}
  end;

handle(St, {send_job, F, Args, Ref, Pid}) ->
  Users = get_all_users(St),
  io:fwrite("Server, Send job start: ~p~n", [Users]),
  case Users of
    [] ->
      {reply,no_users,St};
    _ ->
      List_TaskUser = assign_tasks(Users, Args),
      Refs = [ Temp_Ref || {_, _, Temp_Ref} <-  List_TaskUser],
      P = self(),
      [ spawn (fun() -> P ! genserver:request(User, {do_task, Task, F, Temp_Ref}, infinity) end) || {User, Task, Temp_Ref} <- List_TaskUser],
      io:fwrite("Server, Send job almost end: ~p~n", [Refs]),
      {reply, ok, St#server_st{refs = Refs, respond_to = {Ref, Pid} } }
  end.

sort_by_pid(List, Ref) ->
  List.

assign_tasks([], _) -> [] ;
assign_tasks(Users, Tasks) ->
  [  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task, make_ref()} || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].

get_all_users (St) ->
  [Pid || {_, Pid} <- St#server_st.cUsers].
