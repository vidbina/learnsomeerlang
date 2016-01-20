-module(super).
-behaviour(supervisor).
-export([init/1]).
-export([start/2]).

%% behaviour implementation
init(Args=#{supervisor:=SupervisorStrategy, children:=ChildSpecs}) ->
  io:format("starting supervisor with ~p~n", [Args]),
  {ok, {SupervisorStrategy, ChildSpecs}}.

%% helpers
start(childless, SupervisorRef) ->
  io:format("starting supervisor childless ~p~n", [SupervisorRef]),
  supervisor:start_link({local, SupervisorRef},
                        ?MODULE,
                        #{supervisor => #{
                            strategy => one_for_one,
                            intensity => 3, %% allow 3 restarts
                            period => 5000 },  %% within a 5000ms timeframe
                          children => [] });
start(basic, SupervisorRef) ->
  io:format("starting supervisor basic ~p~n", [SupervisorRef]),
  supervisor:start_link({local, SupervisorRef},
                        ?MODULE,
                        #{supervisor => #{
                            strategy => one_for_one,
                            intensity => 3, %% allow 3 restarts
                            period => 5000 },  %% within a 5000ms timeframe
                          children => [#{id => alpha,
                                         start => {basic, start, [0]},
                                         restart => permanent,
                                         shutdown => 3000,
                                         type => worker },
                                       #{id => bravo,
                                         start => {basic, start, [500]},
                                         restart => permanent,
                                         shutdown => 3000,
                                         type => worker },
                                       #{id => charlie,
                                         start => {basic, start, [900]},
                                         restart => permanent, %% transient,
                                         shutdown => 3000,
                                         type => worker } ] });
start(deep, SupervisorRef) ->
  io:format("starting supervisor deep ~p~n", [SupervisorRef]),
  supervisor:start_link({local, SupervisorRef},
                        ?MODULE,
                        #{supervisor => #{
                            strategy => one_for_one,
                            intensity => 3,
                            period => 5000 },
                          children => [#{id => kylo,
                                         start => {basic, start, [0]},
                                         restart => permanent,
                                         shutdown => 3000,
                                         type => worker },
                                       #{id => yoda,
                                         start => {?MODULE, start, [basic, o]},
                                         restart => permanent,
                                         shutdown => 3000,
                                         type => supervisor} ] }).
