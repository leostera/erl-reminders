-module(event).
-compile(export_all).

-record(state, {server,
                name="",
                delay=0}).

loop(S = #state{server=Server, delay=Delay}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after Delay*1000 ->
    Server ! {done, S#state.name}
  end.
