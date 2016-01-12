-module(my_behaviour).
-export([behaviour_info/1, music/1]).

behaviour_info(callbacks) -> [{init,1}, {some_fun,0}, {other,3}];
behaviour_info(_) -> undefined.

music(Mod) -> io:format("~p~n", [Mod:other(la,di,da)]).
