-module(tree).
-export([empty/0, insert/3, lookup/2, demo/0]).

empty() -> { node, 'nil' }.

% create a node when empty
insert(Key, Val, { node, 'nil' }) ->
  { node, { Key, Val, { node, 'nil' }, { node, 'nil' } }};
% lesser new key to smaller branch
insert(NewKey, NewVal, { node, { Key, Val, Smaller, Larger } }) when NewKey < Key ->
  { node, { Key, Val, insert(NewKey, NewVal, Smaller), Larger } };
% greater new key to larger branch
insert(NewKey, NewVal, { node, { Key, Val, Smaller, Larger } }) when NewKey > Key ->
  { node, { Key, Val, Smaller, insert(NewKey, NewVal, Larger) } };
% equal new key replaces old KV pair
insert(Key, Val, { node, { Key, _, Smaller, Larger } }) ->
  { node, { Key, Val, Smaller, Larger } }.

%   Empty = { node, nil }
% insert('leni', 'paperboats', Empty)
%   LeniTree = { node, { 'leni', 'paperboats', Empty, Empty } }
% insert('hans', 'zimmer', LeniTree)
%   { node, { 'leni', 'paperboats',
%     { node, { 'hans', 'zimmer', Empty, Empty } },
%     Empty
%   } }
% insert('ani', 'skywalker', ...)
%   { node, { 'leni', 'paperboats',
%     { node, { 'hans', 'zimmer',
%       { node, { 'abi', 'skywalker', Empty, Empty } },
%       Empty
%     } },
%     Empty
%   } }
% insert('han', 'solo', ...)
%   { node, { 'leni', 'paperboats',
%     { node, { 'hans', 'zimmer',
%       { node, { 'abi', 'skywalker',
%         Empty,
%         { node, { 'han', 'solo', Empty, Empty } }
%       } },
%       Empty
%     } },
%     Empty
%   } }

demo() -> insert('han', 'solo', insert('ani', 'skywalker', insert('hans', 'zimmer', insert('leni', 'paperboats', empty())))).
% The result when executed in erl:
% {node,{leni,paperboats,
%           {node,{hans,zimmer,
%                       {node,{ani,skywalker,
%                                  {node,nil},
%                                  {node,{han,solo,{node,nil},{node,nil}}}}},
%                       {node,nil}}},
%           {node,nil}}}

lookup(_, { node, 'nil' }) -> undefined;
lookup(Key, { node, { Key, Val, _, _ } }) -> { ok, Val };
lookup(LookupKey, { node, { Key, _, Smaller, _ }} ) when LookupKey < Key ->
	lookup(LookupKey, Smaller);
lookup(LookupKey, { node, { _, _, _, Larger }} ) ->
	lookup(LookupKey, Larger).
