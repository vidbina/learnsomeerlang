-module(event).
-export([loop/1, start/3]).
-record(state, { server, name="", to_go=0 }).

start(Server, Name, Seconds) ->
  Pid = spawn(fun() -> loop(#state{server=Server, name=Name, to_go=Seconds}) end),
  io:format("Registering ~p on ~p", [Pid, self()]),
  register(Name, Pid).

loop(State = #state{server=Server}) ->
  receive
    { Server, Ref, cancel } ->
      io:format("Cancel ~p~n", [State#state.name]),
      Server ! { Ref, ok }
  after
    State#state.to_go*1000 ->
      io:format("Completed ~p after ~ps~n", [State#state.name, State#state.to_go]),
      Server ! { done, State#state.name }
  end.
