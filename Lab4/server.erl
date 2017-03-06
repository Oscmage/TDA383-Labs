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

%% Handle-function done, add the job that is recently done to our results-list
% Then it checks if there are more jobs left by checking if the currentResultList is equal to refs-list
% If there are no more jobs left, it sorts the ResultList and sends back {done, SortedList and current Ref} back to Pid
% if there are more jobs left, it will add the current finished job to results-list
handle(St, {done, Result, Ref}) ->
  CurrentResultList = St#server_st.results ++ [{Ref,Result}],
  if
      length(CurrentResultList) == length(St#server_st.refs) -> % We got responses from all workers?
        SortedList = sort_by_ref(CurrentResultList, St#server_st.refs), % Sort the list based on refs.
        Pid = element(2, St#server_st.respond_to), % 'respond_to' is on format {Ref,Pid}
        R = element(1, St#server_st.respond_to), % R = Ref.
        Pid ! {done, SortedList, R}, % Send the reponse to whoever asked for job to be done.
        {reply, ok, St#server_st{ respond_to={}, refs=[], results=[]}}; % We're now done with the job, so reset variables
      true ->
        {reply, ok, St#server_st{results = CurrentResultList}} % Not done, just update state
  end;

%% This handle-function is called by send_job in cchat.erl
% The purpose of this function is to distribute all jobs to all users
% First, it checks if there is any users connected to the server at all
% If there is, it will call assign_tasks and then create a new process for every user that is going to do a task
handle(St, {send_job, F, Args, Ref, Pid}) ->
  Users = get_all_users(St),
  case Users of
    [] -> % we got no workers
      {reply,no_users,St};
    _ -> % Great, we got some users that can do some work for us
      List_TaskUser = assign_tasks(Users, Args), %Assign tasks to users
      Refs = [ Temp_Ref || {_, _, Temp_Ref} <-  List_TaskUser], % Get the Refs
      P = self(),
      [ spawn (fun() -> genserver:request(User, {do_task, Task, F, Temp_Ref, P}, infinity) end) || {User, Task, Temp_Ref} <- List_TaskUser], % Spread the tasks to the users.
      {reply, ok, St#server_st{refs = Refs, respond_to = {Ref, Pid} } } % Update state so we know who to reply to
  end.

%% Sort a list based on the second argument, Refs
sort_by_ref(List, Refs) ->
  [ element(2, lists:keyfind(R, 1, List)) || R <- Refs].

%% assign_tasks pair up users with tasks and add ref to each elements in the list
assign_tasks([], _) -> [] ;
assign_tasks(Users, Tasks) ->
  [  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task, make_ref()} || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].

%% Return all users
get_all_users (St) ->
  [Pid || {_, Pid} <- St#server_st.cUsers].
