-module(evserv).
-compile(export_all).
-record(state, { events, clients }).
-record(event, { name="", description="", pid, timeout=calendar:local_time()}).

start() -> start(?MODULE).
start(Name) -> register(Name, Pid=spawn(?MODULE, init, [])), Pid.
start_link() -> start_link(?MODULE).
start_link(Name) -> register(Name, Pid=spawn_link(?MODULE, init, [])), Pid.

terminate() -> terminate(?MODULE).
terminate(Name) -> whereis(Name) ! shutdown.

init() -> loop(#state{events=orddict:new(), clients=orddict:new() }).

experiment() -> io:format("?MODULE is ~p~n", [?MODULE]).

% monitor the process because we want the client to be subscribed to an
% instance. If the instance dies the subscription cancels and may be restarted
% again by the party who requested the subscription. No assumption is made.
subscribe(Pid) ->
  Ref = erlang:monitor(process, whereis(?MODULE)),
  ?MODULE ! { self(), Ref, { subscribe, Pid } },
  receive
    { Ref, ok } -> { ok, Ref };
    { 'DOWN', Ref, process, _Pid, Reason } -> { error, Reason }
  after 5000 ->
          { error, timeout }
  end.
% add_event does not subscribe to process because it doesn't care about the
% process. If the process is restarted while this function is executed it will
% communicate with the latest registered process in order to handle the
% request.
add_event(Name, Description, TimeOut) ->
  Ref = make_ref(),
  ?MODULE ! { self(), Ref, { add, Name, Description, TimeOut } },
  receive
    { Ref, Msg } -> Msg
  after 5000 ->
          { error, timeout }
  end.
cancel_event(Name) ->
  Ref = make_ref(),
  ?MODULE ! { self(), Ref, { cancel, Name } },
  receive
    { Ref, ok } -> ok
  after 5000 ->
          { error, timeout }
  end.

listen(Delay) ->
  receive
    M = { done, _Name, _Description } ->
      [M | listen(0)]
  after Delay*1000 -> []
  end.

loop(State=#state{}) ->
  receive
    { Pid, MsgRef, { subscribe, Client } } ->
      Ref = erlang:monitor(process, Client),
      NewClients = orddict:store(Ref, Client, State#state.clients),
      Pid ! { MsgRef, ok },
      loop(State#state{clients=NewClients});
    { Pid, MsgRef, { add, Name, Description, TimeOut } } ->
      case valid_datetime(TimeOut) of
        true -> EventPid = event:start_link(Name, TimeOut),
                NewEvents = orddict:store(Name,
                                          #event{name=Name,
                                                 description=Description,
                                                 pid=EventPid,
                                                 timeout=TimeOut},
                                          State#state.events),
                Pid ! { MsgRef, ok },
                loop(State#state{events=NewEvents});
        false -> Pid ! { MsgRef, { error, bad_timeout }},
                 loop(State)
      end;
    { Pid, MsgRef, { cancel, Name } } ->
      Events = case orddict:find(Name, State#state.events) of
                 { ok, E } ->
                   event:cancel(E#event.pid),
                   orddict:erase(Name, State#state.events);
                 error ->
                   State#state.events
               end,
      Pid ! { MsgRef, ok },
      loop(State#state{events=Events});
    { done, Name } ->
      case orddict:find(Name, State#state.events) of
        { ok, E } ->
          send_to_clients(
            { done, E#event.name, E#event.description },
            State#state.clients
           ),
          NewEvents = orddict:erase(Name, State#state.events),
          loop(State#state{events=NewEvents});
        error ->
          loop(State)
      end;
    shutdown -> exit(shutdown);
    { 'DOWN', Ref, process, _Pid, _Reason } ->
      loop(State#state{clients=orddict:erase(Ref, State#state.clients)});
    code_change -> ?MODULE:loop(State);
    Unknown -> io:format("Unknown msg ~p~n", [Unknown]),
               loop(State)
  end.

send_to_clients(Msg, ClientDict) ->
  orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).
valid_datetime({Date, Time}) ->
  try
    calendar:valid_date(Date) andalso valid_time(Time)
  catch
    error:function_clause -> false
  end.
valid_time({H,M,S}) -> valid_time(H, M, S).
valid_time(H,M,S) when H >= 0, H < 24,
                         M >= 0, M < 60,
                         S >= 0, M < 60 -> true;
valid_time(_,_,_) -> false.
