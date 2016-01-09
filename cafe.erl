-module(cafe).
-export([start/0, say/2, saymore/2, listen/0, cafe/0]).

cafe() ->
  receive
    { Pid, hello } ->
      timer:sleep(3000),
      Pid ! { self(), hey },
      cafe();
    { Pid, marco } ->
      timer:sleep(1000),
      Pid ! { self(), polo },
      cafe();
    { Pid, polo } ->
      timer:sleep(1000),
      Pid ! { self(), marco },
      cafe();
    { Pid, bye } -> 
      timer:sleep(1000),
      Pid ! { self(), adios };
    { Pid, Something } ->
      timer:sleep(2000),
      io:format("~p~n", [Something]),
      Pid ! { self(), Something },
      cafe();
    { Pid, Something, Delay } ->
      timer:sleep(Delay),
      io:format("~p in ~pms~n", [Something, Delay]),
      Pid ! { self(), Something },
      cafe()
  end.
  
start() -> spawn(?MODULE, cafe, []).
% receiving a message after sending something just picks up the earliest
% message on the stack which may not be the message I'm looking for. 
say(Pid, Something) ->
  Pid ! { self(), Something },
  receive
    { _, Msg } ->
      io:format("~p => ~p~n", [Something, Msg]),
      Msg
  end.
say(Pid, Something, Delay) ->
  Pid ! { self(), Something, Delay },
  receive
    { _, Msg } ->
      io:format("~p => ~p (~pms)~n", [Something, Msg, Delay]),
      Msg
  end.
saymore(Pid, Things) ->
  lists:map(fun(Something) ->
                case Something of
                  { Msg, Delay } -> say(Pid, Msg, Delay);
                  Msg -> say(Pid, Msg)
                end
            end, Things).

listen() ->
  receive
    { _, Msg } -> io:format("got ~p~n", [Msg])
  end.
