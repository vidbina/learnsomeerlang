%%%%% Naive version
-module(kitty_server2).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1, handle/2]).
-record(cat, {name, color=green, description}).
 
%%% Client API
start_link() -> spawn_link(fun init/0).
 
%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
  my_server:call(Pid, {order, Name, Color, Description}).
 
%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
  my_server:cast(Pid, {return, Cat}).
 
%% Synchronous call
close_shop(Pid) ->
  my_server:call(Pid, terminate).
 
%%% Server functions
init() -> my_server:loop(?MODULE, []).
 
handle({Pid, Ref, {order, Name, Color, Description}}, Cats) ->
  if Cats =:= [] ->
       Pid ! {Ref, make_cat(Name, Color, Description)},
       my_server:loop(?MODULE, Cats);
     Cats =/= [] -> % got to empty the stock
       Pid ! {Ref, hd(Cats)},
       my_server:loop(?MODULE, tl(Cats))
  end;
handle({return, Cat=#cat{}}, Cats) ->
  my_server:loop(?MODULE, [Cat|Cats]);
handle({Pid, Ref, terminate}, Cats) ->
  Pid ! {Ref, ok},
  terminate(Cats);
handle(Unknown, Cats) ->
  %% do some loggin here too
  io:format("Unknown message: ~p~n", [Unknown]),
  my_server:loop(?MODULE, Cats).

%%% Private functions
make_cat(Name, Col, Desc) ->
  #cat{name=Name, color=Col, description=Desc}.
 
terminate(Cats) ->
  [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
  ok.
