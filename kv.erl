-module(kv).
-compile(export_all).

improper_list() -> [
             {maurice, "hitman"},
             {aiden, "protagonist"},
             {tbone, "hacker who screwed over Blume"},
             {quinn, "bad guy?"},
             {damien, "old friend"},
             {clara, "badboy17"},
             {defalt, "hacker"},
             {damien, "kidnapped Aiden's sister"}
            ].

proper_list() -> [
             {maurice, "hitman"},
             {aiden, "protagonist"},
             {tbone, "hacker who screwed over Blume"},
             {quinn, "bad guy?"},
             {clara, "badboy17"},
             {defalt, "hacker"},
             {damien, "kidnapped Aiden's sister"}
            ].

% lists:keyreplace(damien, 1, improper_list(), {damien, "sell-out"}).
% orddict:find(damien, proper_list()).
%
% lists:map(fun({K, V}) -> orddict:find(K, kv:proper_list()) end, kv:proper_list()).
% returns which is weird since kv:proper_lists() does contain the keys

% [{ok,"hitman"},
%  error,
%   {ok,"hacker who screwed over Blume"},
%    error,error,error,error]
% lists:map(fun({K, V}) -> { K, orddict:is_key(K, kv:proper_list()) } end, kv:proper_list()).
% [{maurice,true},
%  {aiden,false},
%  {tbone,true},
%  {quinn,false},
%  {clara,false},
%  {defalt,false},
%  {damien,false}]

% following list should only be able to find alpha, foxtrot, golf and hotel
% because of the alphabetic order they're in. Orddict needs to order the list
% in order to have the keys represented in alphabetic order in order to work.
orddict_mindfuck() -> [{alpha, 1}, {foxtrot, 2}, {bravo, 3}, {charlie, 4}, {golf, 5}, {delta, 6}, {hotel, 7}].

