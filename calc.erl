-module(calc).
-compile(export_all).

calc(Expr) -> calc(string:tokens(Expr, " "), []).

calc([], [Val|_]) -> Val;
calc(["+"|Tokens], [L|[R|Stack]]) ->
  calc(Tokens, [L+R|Stack]);
calc(["-"|Tokens], [Subtrahend|[Minuend|Stack]]) ->
  calc(Tokens, [Minuend-Subtrahend|Stack]);
calc(["*"|Tokens], [Multiplier|[Multiplicant|Stack]]) ->
  calc(Tokens, [Multiplicant*Multiplier|Stack]);
calc(["/"|Tokens], [Divisor|[Dividend|Stack]]) ->
  calc(Tokens, [Dividend/Divisor|Stack]);
calc([Val|Tokens], Stack) ->
  calc(Tokens, [erlang:list_to_integer(Val)|Stack]).

rpn(L) when is_list(L) ->
  [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")), Res.
rpn("+", [A|[B|Rest]]) -> [A+B|Rest];
rpn("-", [Subtrahend|[Minuend|Rest]]) -> [Minuend-Subtrahend|Rest];
rpn("*", [Multiplier|[Multiplicant|Rest]]) -> [Multiplicant*Multiplier|Rest];
rpn("/", [Divisor|[Dividend|Rest]]) -> [Dividend/Divisor|Rest];
rpn(Operand, Acc) -> [erlang:list_to_integer(Operand)|Acc].
