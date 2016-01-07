-module(records).
-compile(export_all).

-record(robot, {name, type, hobbies, details=[]}).
empty_robot() -> #robot{}.
named_robot(Name) -> #robot{name=Name}.
