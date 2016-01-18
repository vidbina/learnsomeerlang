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
  run(exit_other_does_not_terminate_children);
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
  {ok, Supervisor} = super:start(basic, test_two),
  Children = supervisor_children_pids(test_two),
  % kill every child using exit(Pid, kill)
  io:format("children@0 ~p~n", [Children]),
  [exit(Child, other) || Child <- Children, is_process_alive(Child)],
  receive
    Msg -> io:format("got ~p something back~n", [Msg])
  after 2000 -> io:format("waited too long~n")
  end,
  io:format("chidren@1 ~p~n", [supervisor_children_pids(test_two)]),
  true = (supervisor_children_pids(test_two) == Children),
  exit(Supervisor, normal),
  passed.
