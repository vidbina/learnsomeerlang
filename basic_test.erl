-module(basic_test).
-include_lib("eunit/include/eunit.hrl").

reverse_empty_test() -> ?assert([] =:= lists:reverse([])).
reverse_single_test() -> [1] = lists:reverse([1]).
reverse_multiple_test() -> [3,2,1] = lists:reverse([1,2,3]).
reverse_failure_test() -> [2,3,1] = lists:reverse([]).

% test has to return true so if I want to run multiple lists I would either
% have to run different tests and check to see that the list contains only
% truths but then every test inside the test is executed seperately and the
% test halts at the first failure.

% sucks test needs to fail to break and too verbose
reverse_multiple_hack1_test() ->[?assert([] =:= lists:reverse([])),
                                  ?assert([1] =:= lists:reverse([1])),
                                  ?assert([2,1] =:= lists:reverse([1,2]))].

% less verbose but still needs a failure to make something happen
reverse_multiple_hack2_test() ->
  [?assert(Base =:= lists:reverse(Res)) || {Base, Res} <- [{[], []},
                                                           {[1], [1]},
                                                           {[2,1], [1,2]}]].

% looks a bit more succinct
reverse_multiple_hack3_test() ->
  [Base =:= lists:reverse(Res) || {Base, Res} <- [{[], []},
                                                  {[1], [1]},
                                                  {[2,1], [1,2]}]].

% needed the assert to fail well on failure
reverse_multiple_hack4_test() ->
  [?assert(Base =:= lists:reverse(Res)) || {Base, Res} <- [{[], []},
                                                           {[1], [1]},
                                                           {[2,1], [1,2]},
                                                           {[2,1], [2,2]},
                                                           {[2,1], [2,1]}]].

% a generator needs to return a test or a list of tests where a test is any
% 0-arity function that executes to represent success or fails to represent
% failure
gen_list_test_() ->
  [fun() -> ok end].
gen_single_test_() ->
  fun() -> ok end.

% the fun() -> something end. tests look like shit... so let's simplify it
simple_gen_list_test_() ->
  [?_test(ok)].

% back to the reverse example, now every instance of the values is going to be
% handled as a seperate test, problem is that the test output however still
% represents the bound variable names instead of the values which will make
% debugging difficult.
reverse_readable_gen_list_test_() ->
  [?_test(?assert(Base =:= lists:reverse(Res))) || {Base, Res} <- [{[], []},
                                                                   {[1], [1]},
                                                                   {[2,1], [1,2]},
                                                                   {[2,1], [2,2]},
                                                                   {[2,1], [2,1]}]].

gen_test_() -> [?_test(?assert(A =:= B)) || {A, B} <- [{1,1.0}, {2,2}, {0.0, 0}]].


% fails because assert needs true or false, assignment returns the assigned
% value or throws an error
assert_id1_test() -> ?assert(1 = 1).
assert_id2_test() -> 1 = 1.
%reverse_cases_test_() -> apply()
