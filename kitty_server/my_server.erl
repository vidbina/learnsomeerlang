-module(my_server).
-compile(export_all).

call(Pid, Msg) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! { self(), Ref, Msg },
  receive
    { Ref, Response } ->
      erlang:demonitor(Ref, [flush]),
      Response;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 ->
          erlang:error(timeout)
  end.

cast(Pid, Msg) ->
  Pid ! Msg,
  ok.

loop(Module, State) ->
  receive
    Message -> Module:handle(Message, State)
  end.
