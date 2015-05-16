-module(reminde_event_tests).
-include_lib("eunit/include/eunit.hrl").

%% macro because setup is the same across all tests
-define(setup(F), {setup, fun start/0, fun stop/1, F}).


