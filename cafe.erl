-module(cafe).
-export([start/0, say/2, saymore/2, sayandlisten/2, sayandlisten/3, listen/0, cafe/0, routine/1,listentoresponse/1]).

cafe() ->
  receive
    { Pid, hello } ->
      spawn(fun() -> timer:sleep(3000), Pid ! { self(), hey } end),
      cafe();
    { Pid, marco } ->
      spawn(fun() -> timer:sleep(1000), Pid ! { self(), polo } end),
      cafe();
    { Pid, polo } ->
      spawn(fun() -> timer:sleep(1000), Pid ! { self(), marco } end),
      cafe();
    { Pid, bye } -> 
      spawn(fun() -> timer:sleep(1000), Pid ! { self(), adios } end);
    { Pid, Something } ->
      spawn(fun() ->
                timer:sleep(2000),
                io:format("~p~n", [Something]),
                Pid ! { self(), Something }
            end),
      cafe();
    { Pid, Something, Delay } ->
      spawn(fun() -> timer:sleep(Delay),
                     io:format("~p in ~pms~n", [Something, Delay]),
                     Pid ! { self(), Something }
            end),
      cafe()
  end.
  
start() -> spawn(?MODULE, cafe, []).
% receiving a message after sending something just picks up the earliest
% message on the stack which may not be the message I'm looking for. 
listentoresponse(Something) ->
  receive
    { _, Msg } ->
      io:format("~p => ~p~n", [Something, Msg]),
      Msg
  end.
sayandlisten(Pid, Something) ->
  say(Pid, Something),
  listentoresponse(Something).
sayandlisten(Pid, Something, Delay) ->
  say(Pid, Something, Delay),
  listentoresponse(Something).
say(Pid, Something) ->
  Pid ! { self(), Something }.
say(Pid, Something, Delay) ->
  Pid ! { self(), Something, Delay }.
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

routine(Pid) ->
  cafe:saymore(Pid,[this,is,weird,dont,you,think,{defalt,3000},{works,10000},{for,5000},blume,right,now]).
