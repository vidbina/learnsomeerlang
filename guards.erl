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

%% Experimenting with errors. Apparently using the `,` and `;` operators in
%% guard clauses catches errors, so let's write a function that would allow for
%% errors to be thrown in clauses (e.g.: additions between different types).
valid_denominator_even_sum(X, Y) when X+Y > 0, X>0, Y>0 ->
  X+Y;
valid_denominator_even_sum(_, _) ->
  false.

% Add some does something whenever one of the operands is greater than zero.
% This function should fail whenever one of the operands is not a number
% ∴ not addable by one. In case X isn't a number the entire guard fails.
% If X passes, the entire guard passes since the two operands are simply
% subjected to the OR operation.
add_nonzeros(X,Y) when X+1>=0 orelse Y+1>=0 ->
  [X, Y];
add_nonzeros(_,_) ->
  fuckoff.
% `add_nonzeros(atom, 12)` should fail while
% `add_nonzeros_again(atom, 12)` should return `[atom, 12]`

% Theoretically with X as a non-number (e.g.: an atom) the first guard
% clause should fail, but the second one should pass resulting to the function
% clause being handled.
add_nonzeros_again(X,Y) when X+1>=1; Y+1>=1 ->
  [X, Y];
add_nonzeros_again(_,_) ->
  fuckoff.
