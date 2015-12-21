-module(greeter).
-export([hello/2]).

hello(male, Name) -> io:format("Hello, Mr. ~s", [Name]);
hello(female, Name) -> io:format("Hello, Mrs. ~s", [Name]);
hello(_, Name) -> io:format("Hello ~s", [Name]).
