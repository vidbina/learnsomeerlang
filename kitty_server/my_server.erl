-module(my_server).
-compile(export_all).

call(Pid, Msg) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {sync, self(), Ref, Msg},
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
  Pid ! {async, Msg},
  ok.

reply({Pid, Ref}, Msg) ->
  Pid ! { Ref, Msg }.

loop(Module, State) ->
  receive
    {async, Msg} -> loop(Module, Module:handle(async, Msg, State));
    {sync, Pid, Ref, Msg} -> loop(Module, Module:handle(sync, {Pid, Ref, Msg}, State))
  end.
