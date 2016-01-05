-module(calc).
-compile(export_all).

calc(Expr) -> calc(string:tokens(Expr, " "), []).

calc([], [Val|_]) -> Val;
calc(["+"|Tokens], [L|[R|Stack]]) -> calc(Tokens, [L+R|Stack]);
calc(["-"|Tokens], [Subtrahend|[Minuend|Stack]]) -> calc(Tokens, [Minuend-Subtrahend|Stack]);
calc(["*"|Tokens], [Multiplier|[Multiplicant|Stack]]) -> calc(Tokens, [Multiplicant*Multiplier|Stack]);
calc(["/"|Tokens], [Divisor|[Dividend|Stack]]) -> calc(Tokens, [Dividend/Divisor|Stack]);
calc([Val|Tokens], Stack) -> calc(Tokens, [erlang:list_to_integer(Val)|Stack]).
