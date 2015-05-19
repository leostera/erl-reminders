-module(reminder_event_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%% macro because setup is the same across all tests
-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%%
%%% TESTS DESCRIPTIONS
%%%

create_event_test_() ->
  {"Creating an event returns a living Pid",
   {setup,
    fun start/0,
    fun stop/1,
    fun create_event/1}}.

%%%
%%% SETUP FUNCTIONS
%%%

start() ->
  Pid = reminder_event:start("Test", {{2020,2,2},{2,2,2}}),
  Pid.

stop(Pid) -> 
  reminder_event:cancel(Pid).

%%%
%%% ACTUAL TESTS
%%%

create_event(Pid) ->
  [?_assert(erlang:is_process_alive(Pid))].


%%%
%%% HELPER FUNCTIONS
%%%


