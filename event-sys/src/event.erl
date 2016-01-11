-module(event).
-export([loop/1, init/4, start_link/3, start_link/4, start/3, start/4, cancel/1, normalize/2]).
-record(state, { server, name="", to_go=0 }).

% time in milliseconds
start(Server, Name, Time) -> start(Server, Name, Time, 1000).
start(Server, Name, Time, Period) -> 
  Pid = spawn(?MODULE, init, [Server, Name, Time, Period]),
  io:format("Registering ~p on ~p", [Pid, self()]),
  register(Name, Pid).

start_link(Server, Name, Time) -> start_link(Server, Name, Time, 1000).
start_link(Server, Name, Time, Period) -> 
  Pid = spawn_link(?MODULE, init, [Server, Name, Time, Period]),
  io:format("Registering ~p on ~p", [Pid, self()]),
  register(Name, Pid).

cancel(Event) ->
  Pid = case is_pid(Event) of
    true -> Event;
    false -> whereis(Event)
  end,
  Ref = erlang:monitor(process, Pid),
  Pid ! { self(), Ref, cancel },
  receive
    { Ref, ok } -> erlang:demonitor(Ref, [flush]), ok;
    { 'DOWN', Ref, process, Pid, _Reason } -> ok
  end.

init(Server, Name, Time, Period) ->
  loop(#state{ server=Server, name=Name, to_go=normalize(Time, Period)}).

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
