-module(records).
-compile(export_all).

-include("records.hrl").

-record(robot, {name, type, hobbies, details=[]}).
-record(character, {name, gender=unknown, series, details=[]}).
-record(show, {name,topics=[]}).
-record(person, {name, year, gender}).

empty_robot() -> #robot{}.
named_robot(Name) -> #robot{name=Name}.

poi() -> #show{
            name="Person of Interest",
            topics=["surveillance", "internet", "data", "AI", "power"]
           }.
harold() -> #character{
               name="Harold Finch",
               gender=male,
               series=poi(),
               details=["smart", "careful", "quirky", "geeky", "intelligent", "god complex"]
              }.
% terrible idea, but I just wanted to experiment.
root() -> (harold())#character{
                     name="Samantha Groves",
                     gender=female,
                     details=["smart", "intelligent", "geeky", "hacker", "annoying", "gorgeous"]
                    }.
reese() -> #character{
              name="John Reese",
              gender=male,
              series=poi(),
              details=["fit", "martial artist", "charming", "sharpshooter", "dark ops", "ex blackwater", "broken heart"]
             }.

% try admin_panel(reese()) or admin_panel(harold())
% better than matching {character, "Harold Finch", _, _, _} in admin_panel
admin_panel(#character{name="Harold Finch"}) -> allowed;
admin_panel(#character{name="Samantha Groves"}) -> allowed;
admin_panel(_) -> denied.

porn(Watcher=#person{}) ->
  {{Year,_,_}, _} = calendar:local_time(),
  case Watcher#person.year < Year-18 of
    true -> allowed;
    false -> denied
  end.
