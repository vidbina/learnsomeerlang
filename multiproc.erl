-module(multiproc).
-compile(export_all).

important() ->
  receive
    { Prio, Msg } when Prio > 10 ->
      [ { important, Msg } | important() ]
  after 0 -> normal()
  end.

normal() ->
  receive
    { _, Msg } -> [ { normal, Msg } | normal() ]
  after 0 -> []
  end.

demo() ->
  io:format("self is ~p~n", [self()]),
  self() ! { 11, food },
  self() ! { 2, bullshit },
  self() ! { 20, health },
  self() ! { 1, crap },
  self() ! { 1, gossip },
  self() ! { 12, knowledge }.

scanner(Threshold) ->
  receive
    { president, _, Msg } -> [ { potus, Msg } | scanner(Threshold) ];
    { _, Prio, Msg } when Prio >= Threshold -> [ { poi, Msg } | scanner(Threshold) ]
  after 0 -> []
  end.
chatter() ->
  self() ! { president, 100, "I'm headed to the meeting. I'll call off the dogs tonight." },
  self() ! { general, 80, "I'll get the troops ready" },
  self() ! { vendor, 3, "fresh tuna for sale" },
  self() ! { agent, 50, "all clear" },
  self() ! { sniper, 30, "bogey at 12 o'clock... visual on the price." },
  self() ! { child, 1, "could we get some ice-cream?" },
  self() ! { parent, 2, "how about a smoothie instead?!?" },
  self() ! { agent, 50, "keep an eye out... suspicious lady... 3 o'clock" },
  self() ! { robber, 3, "he's got a nice purse... wonder what's in there" },
  self() ! { lawyer, 20, "I just looked at the files this is damning material" },
  self() ! { client, 10, "I take it we're headed to court then?" },
  self() ! { laywer, 20, "absolutely" },
  self() ! { soldier, 30, "POTUS is one click out. Charlie has a visual." },
  self() ! { model, 2, "like yeah... someone important seems to be driving through town" },
  self() ! { hairdresser, 2, "awww really, not sure what the fuss is all about" },
  self() ! { model, 2, "yeah... dunno... anyway... I've been soo looking forward to this party" },
  self() ! { hairdresser, 2, "I'm like sooo jealous" },
  self() ! { agent, 50, "within the perimeter" },
  self() ! { sniper, 30, "performing final sweep" },
  self() ! { president, 100, "thank you buddy" }, ok.
