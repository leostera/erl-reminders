-module(reminder_evserv).
-compile(export_all).

-record(state, {events,     %% lis of #event{} records
                clients}).  %% list of Pids subscribed

-record(event, {name="",
                description="",
                pid,
                timeout={{1970,1,1},{0,0,0}}}).

init() ->
  loop(#state{events=orddict:new(),
              clients=orddict:new()}).

loop(S= #state{events=Events, clients=Clients}) ->
  receive
    {Pid, MsgRef, {subscribe, Client}} ->
      Ref = erlang:monitor(process, Client),
      NewClients = orddict:store(Ref, Client, S#state.clients),
      Pid ! {MsgRef, ok},
      loop(S#state{clients=NewClients});
    {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
      case valid_datetime(TimeOut) of
        true ->
          EventPid = event:start_link(Name, TimeOut),
          NewEvents = orddict:store(Name,
                                    #event{name=Name,
                                           description=Description,
                                           pid=EventPid,
                                           timeout=TimeOut},
                                    S#state.events),
          Pid ! {MsgRef, ok},
          loop(S#state{events=NewEvents});
        false ->
          Pid ! {MsgRef, {error, bad_timeout}},
          loop(S)
      end;
    {Pid, MsgRef, {cancel, Name}} ->
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


%% private date and time validation functions

valid_datetime({Date, Time}) ->
  try
    calendar:valid_date(Date) andalso valid_time(Time)
  catch
    error:function_clause ->
      false
  end;
valid_datetime(_) -> false.

valid_time({H,M,S}) -> valid_time(H,M,S).
valid_time(H,M,S) when H >= 0, H < 24,
                       M >= 0, M < 60,
                       S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.
