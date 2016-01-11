%%%%% Naive version
-module(kitty_server2).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1, handle/3]).
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
 
handle(sync, {Pid, Ref, {order, Name, Color, Description}}, Cats) ->
  if Cats =:= [] ->
       my_server:reply({Pid, Ref}, make_cat(Name, Color, Description)),
       Cats;
     Cats =/= [] -> % got to empty the stock
       my_server:reply({Pid, Ref}, hd(Cats)),
      tl(Cats)
  end;
handle(sync, {Pid, Ref, terminate}, Cats) ->
  my_server:reply({Pid, Ref}, ok),
  terminate(Cats);
handle(async, {return, Cat=#cat{}}, Cats) ->
  my_server:loop(?MODULE, [Cat|Cats]).

%%% Private functions
make_cat(Name, Col, Desc) ->
  #cat{name=Name, color=Col, description=Desc}.
 
terminate(Cats) ->
  [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
  exit(normal).
