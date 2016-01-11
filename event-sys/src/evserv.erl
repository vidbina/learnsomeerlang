-module(evserv).
-compile(export_all).
-record(state, { events, clients }).
-record(event, { name="", description="", pid, timeout=calendar:local_time()}).

start(Name) ->
  register(Name, spawn(?MODULE, init, [])).

init() ->
  loop(#state{events=orddict:new(), clients=orddict:new() }).

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