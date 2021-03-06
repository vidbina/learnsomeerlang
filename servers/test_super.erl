-module(test_super).
-export([run/0, run/1]).

supervisor_children_pids(SupervisorName)->
  [element(2, C) || C <- supervisor:which_children(SupervisorName)].

run() -> run(all).
% this confirms whatever can be read at 12.7 Error Handling of
% http://www.erlang.org/doc/reference_manual/processes.html
run(all) ->
  run(exit_normal_does_not_terminate_children),
  run(exit_kill_does_terminate_children),
  run(exit_other_does_not_terminate_children),
  run(exit_supervising_child);
run(exit_normal_does_not_terminate_children) ->
  io:format("testing as ~p~n", [self()]),
  %% test killing all tasks multiple times and watch if they restart
  {ok, Supervisor} = super:start(basic, test_one),
  Children = supervisor_children_pids(test_one),
  % kill every child using exit(Pid, kill)
  io:format("The children are ~p~n", [Children]),
  [exit(Child, normal) || Child <- Children, is_process_alive(Child)],
  true = (supervisor_children_pids(test_one) == Children),
  receive
    Msg -> io:format("got ~p something back~n", [Msg])
  after 2000 -> io:format("waited too long~n")
  end,
  io:format("Current children ~p~n", [supervisor_children_pids(test_one)]),
  exit(Supervisor, normal),
  passed;
run(referencing_of_worker_by_id) ->
  io:format("testing as ~p~n", [self()]),
  {ok, Supervisor} = super:start(basic, ref_worker),
  io:format("must all be started~n"),
  receive
    Msg -> io:format("got ~p something back~n", [Msg])
  after 2000 -> io:format("waited too long~n")
  end,
  receive
  after 2000 ->
    % TODO: figure out why this call never arrives
    gen_server:cast(alpha, boo)
  end,
  exit(Supervisor, normal),
  passed;
run(exit_kill_does_terminate_children) ->
  io:format("testing as ~p~n", [self()]),
  %% test killing all tasks multiple times and watch if they restart
  {ok, Supervisor} = super:start(basic, test_two),
  Children = supervisor_children_pids(test_two),
  % kill every child using exit(Pid, kill)
  io:format("children@0 ~p~n", [Children]),
  [exit(Child, kill) || Child <- Children, is_process_alive(Child)],
  receive
    Msg -> io:format("got ~p something back~n", [Msg])
  after 2000 -> io:format("waited too long~n")
  end,
  io:format("chidren@1 ~p~n", [supervisor_children_pids(test_two)]),
  false = (supervisor_children_pids(test_two) == Children),
  exit(Supervisor, normal),
  passed;
run(exit_other_does_not_terminate_children) ->
  io:format("testing as ~p~n", [self()]),
  %% test killing all tasks multiple times and watch if they restart
  {ok, Supervisor} = super:start(basic, test_other),
  Children = supervisor_children_pids(test_other),
  % kill every child using exit(Pid, kill)
  io:format("children@0 ~p~n", [Children]),
  [exit(Child, other) || Child <- Children, is_process_alive(Child)],
  receive
    Msg -> io:format("got ~p something back~n", [Msg])
  after 2000 -> io:format("waited too long~n")
  end,
  io:format("chidren@1 ~p~n", [supervisor_children_pids(test_other)]),
  true = (supervisor_children_pids(test_other) == Children),
  exit(Supervisor, normal),
  passed;
% TODO: if the server is called by the supervisor and the supervisor receives
% {'EXIT',_,_} tuples, will the supervisor pickup a timeout?
run(pickup_timeout_of_child) ->
  {ok, Supervisor} = super:start(childless, trap_timeouts),
  {ok, Child} = supervisor:start_child(Supervisor, #{id => timer,
                                       start => {basic, start, [50]},
                                       restart => transient,
                                       shutdown => 3000,
                                       type => worker}),
  gen_server:call(Child, {wait, 1000}, 400),
  receive
    Msg -> io:format("test received ~p~n", [Msg])
  after 5000 ->
          io:format("done")
  end,
  passed;
run(exit_supervising_child) ->
  io:format("testing as ~p~n", [self()]),
  %% test killing all tasks multiple times and watch if they restart
  {ok, Supervisor} = super:start(deep, tree),
  Children = supervisor_children_pids(tree),
  % kill every child using exit(Pid, kill)
  %io:format("~p~n", [C || C <- supervisor:which_children(tree)]),
  Sups = [S || S<-supervisor:which_children(tree), element(3, S)==supervisor],
  io:format("children ~p~n", [supervisor:which_children(tree)]),
  receive
  after 2000 -> ChildSupChildren = supervisor:which_children(o)
  end,
  [exit(Child, kill) || Child <- Children, is_process_alive(Child)],
  receive
    Msg -> io:format("got ~p something back~n", [Msg])
  after 2000 -> io:format("waited too long~n"),
                io:format("sup kids~p~n", [ChildSupChildren])
  end,
  io:format("sups ~p~n", [Sups]),
  io:format("children ~p~n", [supervisor:which_children(tree)]),
  io:format("children@0 ~p~n", [Children]),
  io:format("children@1 ~p~n", [supervisor_children_pids(tree)]),
  io:format("yoda children@0 ~p~n", [ChildSupChildren]),
  io:format("yoda children@1 ~p~n", [supervisor:which_children(o)]),
  false = (supervisor_children_pids(tree) == Children),
  exit(Supervisor, normal),
  passed.
% TODO: attempt to pickup a worker by its name or id
