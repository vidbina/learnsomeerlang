-module(event).
-export([loop/1]).
-record(state, { server, name="", to_go=0 }).

loop(State = #state{server=Server}) ->
  receive
    { Server, Ref, cancel } ->
      Server ! { Ref, ok }
  after
    State#state.to_go*1000 ->
      Server ! { done, State#state.name }
  end.
