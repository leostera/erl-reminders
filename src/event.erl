-module(event).
-compile(export_all).

-record(state, {server,
                name="",
                delay=0}).

start(EventName, Delay) ->
  spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
  spawn_link(?MODULE, init, [self(), EventName, Delay]).

%% handle initialization of things
init(Server, EventName, Delay) ->
  loop(#state{server=Server,
              name=EventName,
              delay=normalize(Delay)}).

loop(S = #state{server=Server, delay=[T|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after T*1000 ->
          if Next =:= [] ->
               Server ! {done, S#state.name};
             Next =/= [] ->
               loop(S#state{delay=Next})
          end
  end.

cancel(Pid) ->
  %% Sets up a monitor beforehand to see if the process is alive
  %% and there
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(),Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.

%% Erlang is limited to about 49 days (49*24*60*60*1000) of wait
%% in an after statement, so we need to split any time into 49 days
%% parts!
normalize(N) ->
  %% 49 days limit
  Limit = 49*24*60*60,
  %% lists:duplicate(times, value).
  %% lists:duplicate(3, a) == [a,a,a].
  %% this generates a list of as many times Limit as
  %% N fits in Limit as a tail, and a head of the reminder
  %% of the division between N and Limit.
  [N rem Limit | lists:duplicate(N div Limit, Limit)].
