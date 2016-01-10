-module(linkmon).
-compile(export_all).

myproc() -> timer:sleep(5000), io:format("yawn"), exit(reason).
ok() -> timer:sleep(5000), io:format("done").

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
