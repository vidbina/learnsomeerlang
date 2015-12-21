-module(guards).
-compile([export_all]).

old_enough(X) when is_number(X), X >= 16 -> true;
%% it's better if every other thing we can't explicitly disallow ends up being
%% false anyways. The former approach of returning false on a guarded condition
%% isn't scalable. Imagine adding the number check to that part:
%%   old_enough(X) when is_number(X) and X =< 16 -> false;
%%   old_enough(_) -> true
%% In that case the check would always perform the comparison if a number and
%% return true otherwise. Since numbers also compare lower to other types, is
%% really makes no difference. What we actually want to say is that we only
%% make a statement about age if we have a valid age, otherwise we abstain
%% ourselves from judgement by just saying fase (safety mechanism).
old_enough(_) -> false.
%% Why was:
%%   old_enough(X) when (is_number(X) and X >= 16) -> true;
%% false for all numbers?
%% Because we needed to set operational preference with parenthesis as it
%% evaluated `(is_number(X) and X) >= 16` where the `and` on an atom and number
%% should throw an error, so this respective clause fails.
%%
%% In light of this observation with regards to operator preference, one may
%% argue that using guard-native method for adding multiple conditions is more
%% readable since
%%   old_enough(X) when (is_number(X) and (X >= 16)) -> true;
%% gets messy fast is we have a lot of conditions because of the nesting nature
%% of the notation in comparison to the `andalso` (`,`) or `orelse` (`;`) guard
%% equivalent
%%   old_enough(X) when is_number(X), X >= 16 -> true;
%%
%% Another note, why the hell did I use `and` instead of `andalso` I should
%% remember that `andalso` and `orelse` are short-circuit notations that only
%% evaluate the the right-hand operand if necessary which means less
%% computation in some cases.
