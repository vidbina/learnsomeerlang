-module(basic_test).
-include_lib("eunit/include/eunit.hrl").

reverse_empty_test() -> [] = lists:reverse([]).
reverse_single_test() -> [1] = lists:reverse([1]).
reverse_multiple_test() -> [3,2,1] = lists:reverse([1,2,3]).
reverse_failure_test() -> [2,3,1] = lists:reverse([]).
