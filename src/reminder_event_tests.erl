-module(reminder_event_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%% macro because setup is the same across all tests
-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(test(C, F), {C, ?setup(F)}).

%%%
%%% TESTS DESCRIPTIONS
%%%

event_test_() ->
  [
   ?test("Creating an event returns a living Pid", fun create_event/1),
   ?test("Cancelling an event kills the Pid", fun cancel_event/1)
  ].

%%%
%%% SETUP FUNCTIONS
%%%

start() ->
  Pid = reminder_event:start("Test", {{2020,2,2},{2,2,2}}),
  Pid.

stop(Pid) -> 
  case erlang:is_process_alive(Pid) of
    true -> reminder_event:cancel(Pid);
    false -> ok
  end.

%%%
%%% ACTUAL TESTS
%%%

create_event(Pid) ->
  [?_assert(erlang:is_process_alive(Pid))].

cancel_event(Pid) ->
  [?_assertEqual(ok, reminder_event:cancel(Pid)),
   ?_assertNot(erlang:is_process_alive(Pid))].

%%%
%%% HELPER FUNCTIONS
%%%


