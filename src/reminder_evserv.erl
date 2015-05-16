-module(reminder_evserv).
-compile(export_all).

-record(state, {events,     %% lis of #event{} records
                clients}).  %% list of Pids subscribed

-record(event, {name="",
                description="",
                pid,
                timeout={{1970,1,1},{0,0,0}}}).

loop(S= #state{events=Events, clients=Clients}) ->
  receive
    {Pid, MsgRef, {subscribe, Client}} ->
      ;
    {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
      ;
    {Pid, MsgRef, {cancle, Name}} ->
      ;
    shutdown ->
      ;
    {'DOWN', Ref, process, _Pid, _Reason} ->
      ;
    code_change ->
      ;
    Unknown ->
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(State)
  end.
