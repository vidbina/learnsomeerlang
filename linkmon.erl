-module(linkmon).
-compile(export_all).

myproc() -> timer:sleep(5000), io:format("yawn"), exit(reason).
ok() -> timer:sleep(5000), io:format("done").

friendlychainreceives(N) ->
  receive
    die -> exit(fire);
    bye -> adios;
    _ -> ok, io:format("~p got the memo~n", [N]), friendlychainreceives(N)
  after 10000 -> exit(fuckit)
  end.

% process_flag(trap_exit, true),
% spawn_link(fun() -> linkmon:friendlychain(10) end),
% receive X -> X end.
friendlychain(N) ->
  case N of
    0 -> io:format("terminal at ~p~n", [self()]);
    _ ->
      Pid = spawn(?MODULE, friendlychain, [N-1]),
      io:format("unit with N=~p at ~p~n",[N, Pid]),
      link(Pid)
  end,
  friendlychainreceives(N).

% do a test with bidirectional links
ying() ->
  receive
    die -> io:format("yingling"), ying_out, exit(death);
    bye -> yolo;
    who -> io:format("ying"), ying, ying()
  end.
yang(Pid) ->
  link(Pid),
  receive
    die -> io:format("yanglang"), yang_out, exit(death);
    bye -> zen;
    who -> io:format("yang"), yang, yang(Pid)
  end.
yingyang() ->
  Ying = spawn(fun ying/0), Yang = spawn(?MODULE, yang, [Ying]), { Ying, Yang }.
