-module(event).
-export([loop/1, start/3, start/4, normalize/2]).
-record(state, { server, name="", to_go=0 }).

% time in milliseconds
start(Server, Name, Time) -> start(Server, Name, Time, 1000).
start(Server, Name, Time, Period) -> 
  Pid = spawn(fun() -> loop(#state{
                          server=Server,
                          name=Name,
                          to_go=normalize(Time, Period)}
                      ) end),
  io:format("Registering ~p on ~p", [Pid, self()]),
  register(Name, Pid).

loop(State = #state{server=Server, to_go=[T|Next]}) ->
  receive
    { Server, Ref, cancel } ->
      io:format("Cancel ~p~n", [State#state.name]),
      Server ! { Ref, ok }
  after T ->
          if Next == [] ->
               io:format("Completed ~p~n", [State#state.name]),
               Server ! { done, State#state.name };
             true ->
               io:format("Extend ~p~n", [State#state.name]),
               loop(State#state{to_go=Next})
          end
  end.

normalize(Duration, Limit) ->
  [Duration rem Limit|lists:duplicate(Duration div Limit, Limit)].
