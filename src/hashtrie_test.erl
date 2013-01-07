%% Copyright (C) 2011 Björn-Egil Dahlberg
%%
%% File:    hashtrie_test.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2011-09-01

%% test

-module(hashtrie_test).


-export([
	test/0,
	test_file/0,
	go/1
    ]).

-define(iters, 100).
-define(value(X), X).
-define(key(X),X).
%-define(value(X), {<<"kfopekfekfpofkekfpwkfkepfkwpekfpkfekp">>, [3,[3,2]], X}).
%-define(key(X),[{kdoek,"diejdioejfoiejeoijfijfiej", what}, X]).

test() -> [go(1 bsl I) || I <- lists:seq(1,16)].

test_file() -> file([(1 bsl I) || I <- lists:seq(1,14)]).

gb_trees_put(N) -> gb_trees_put(N, gb_trees:empty()).
gb_trees_put(0, T) -> T;
gb_trees_put(N, T) -> gb_trees_put(N - 1, gb_trees:insert(?key(N), ?value(N), T)).

gb_trees_get(0, _T) -> ok;
gb_trees_get(N, T) -> 
    {value, ?value(N)} = gb_trees:lookup(?key(N), T),
    %?value(N) = gb_trees:get(?key(N), T),
    gb_trees_get(N - 1, T).

gb_trees_update(0, _T) -> ok;
gb_trees_update(N, T) -> 
    Ti = case gb_trees:lookup(?key(N), T) of
	{value, V} ->
	    gb_trees:enter(?key(N), [V,V], T)
    end,
    gb_trees_get(N - 1, Ti).


dict_put(N) -> dict_put(N, dict:new()).
dict_put(0, T) -> T;
dict_put(N, T) -> dict_put(N - 1, dict:store(?key(N), ?value(N), T)).

dict_get(0, _T) -> ok;
dict_get(N, T) -> 
    ?value(N) = dict:fetch(?key(N), T),
    dict_get(N - 1, T).

dict_update(0, _T) -> ok;
dict_update(N, T) -> 
    dict_get(N - 1, dict:update(?key(N), fun(V) -> [V,V] end, T)).

htrie_put(N) -> htrie_put(N, htrie:new()).
htrie_put(0, T) -> T;
htrie_put(N, T) -> htrie_put(N - 1, htrie:put(?key(N), ?value(N), T)).

htrie_get(0, _T) -> ok;
htrie_get(N, T) -> 
    ?value(N) = htrie:get(?key(N), T),
    htrie_get(N - 1, T).

htrie_update(0, T) -> T;
htrie_update(N, T) -> 
    htrie_update(N - 1, htrie:update(?key(N), fun(V) -> [V,V] end, T)).

hamt_put(N) -> hamt_put(N, hamt:new()).
hamt_put(0, T) -> T;
hamt_put(N, T) -> hamt_put(N - 1, hamt:put(?key(N), ?value(N), T)).

hamt_get(0, _T) -> ok;
hamt_get(N, T) -> 
    ?value(N) = hamt:get(?key(N), T),
    hamt_get(N - 1, T).

hamt_update(0, T) -> T;
hamt_update(N, T) -> 
    V = hamt:get(N, T),
    hamt_update(N - 1, hamt:put(?key(N), [V,V], T)).

% array_put(N)    -> array_put(N, array:new()).
% array_put(0, T) -> T;
% array_put(N, T) -> array_put(N - 1, array:set(?key(N), ?value(N), T)).
% 
% array_get(0, T) -> T;
% array_get(N, T) ->
%     ?value(N) = array:get(?key(N), T),
%     array_get(N - 1, T).
% 
% tarray_put(N)    -> tarray_put(N, tarray:new()).
% tarray_put(0, T) -> T;
% tarray_put(N, T) -> tarray_put(N - 1, tarray:set(?key(N), ?value(N), T)).
% 
% tarray_get(0, T) -> T;
% tarray_get(N, T) ->
%     ?value(N) = tarray:get(?key(N), T),
%     tarray_get(N - 1, T).

timeit(Fun) ->
    Iters = lists:seq(1,?iters),
    Me    = self(),
    Pid   = spawn_link(fun() -> 
		{T, R} = lists:foldl(fun(_, {T0, _}) ->
			    {Time, Res} = timer:tc(Fun),
			    {T0 + Time, Res}
		    end, {0, 0}, Iters),
		Me ! {self(), result, {T / ?iters, R}}
	end),
    receive 
	{Pid, result, Res} -> 
	    Res
    end.


file(Ns) ->
    Files = [
	{hamt_put,        "data/hamt_put.dat"},
	{hamt_get,        "data/hamt_get.dat"},
	{hamt_update,     "data/hamt_update.dat"},
	{htrie_put,       "data/htrie_put.dat"},
	{htrie_get,       "data/htrie_get.dat"},
	{htrie_update,    "data/htrie_update.dat"},
	{gb_trees_put,    "data/gb_trees_insert.dat"},
	{gb_trees_get,    "data/gb_trees_get.dat"},
	{gb_trees_update, "data/gb_trees_update.dat"},
	{dict_put,        "data/dict_store.dat"},
	{dict_get,        "data/dict_fetch.dat"},
	{dict_update,     "data/dict_update.dat"}
    ],

    Fds = lists:foldl(fun
	    ({K, File}, T) ->
		{ok,Fd} = file:open(File, [write]),
		htrie:put(K, Fd, T)
	end, htrie:new(), Files),
    file(Ns, Fds),

    lists:foreach(fun
	    ({K,_}) -> 
		file:close(htrie:get(K,Fds)) 
	end, Files),
    ok.

file([],_) -> ok; 
file([N|Ns], Fds) ->
    lists:foreach(fun
	    ({hamt, {P, G, U}}) ->
		pout(htrie:get(hamt_put, Fds), N, P),
		pout(htrie:get(hamt_update, Fds), N, U),
		pout(htrie:get(hamt_get, Fds), N, G);
	    ({htrie, {P, G, U}}) ->
		pout(htrie:get(htrie_put, Fds), N, P),
		pout(htrie:get(htrie_update, Fds), N, U),
		pout(htrie:get(htrie_get, Fds), N, G);
	    ({dict, {P, G, U}}) ->
		pout(htrie:get(dict_put, Fds), N, P),
		pout(htrie:get(dict_update, Fds), N, U),
		pout(htrie:get(dict_get, Fds), N, G);
	    ({gb_trees, {P, G, U}}) ->
		pout(htrie:get(gb_trees_put, Fds), N, P),
		pout(htrie:get(gb_trees_update, Fds), N, U),
		pout(htrie:get(gb_trees_get, Fds), N, G)
	end, go(N)),
    file(Ns, Fds).


pout(Fd, N, R) ->
    io:format(Fd, "~10w \t~.4f~n", [N, R]).


go(N) when is_integer(N) ->
    {Hi, Th} = timeit(fun() -> htrie_put(N) end),
    sout(N, "htrie", "put", Hi, Hi),
    {Hp, ok} = timeit(fun() -> htrie_get(N,Th) end),
    sout(N, "get", Hp, Hp),
    {Hu, _} = timeit(fun() -> htrie_update(N,Th) end),
    sout(N, "update", Hu, Hu),

    {Pi, Tp} = timeit(fun() -> hamt_put(N) end),
    sout(N, "hamt", "put", Pi, Hi),
    {Pp, ok} = timeit(fun() -> hamt_get(N,Tp) end),
    sout(N, "get", Pp, Hp),
    {Pu, _} = timeit(fun() -> hamt_update(N,Tp) end),
    sout(N, "update", Pu, Hu),

    {Gi, Tg} = timeit(fun() -> gb_trees_put(N) end),
    sout(N, "gb_trees", "insert", Gi, Hi),
    {Gp, ok} = timeit(fun() -> gb_trees_get(N, Tg) end),
    sout(N, "get", Gp, Hp),
    {Gu, ok} = timeit(fun() -> gb_trees_update(N, Tg) end),
    sout(N, "update", Gu, Hu),

    {Di, Td} = timeit(fun() -> dict_put(N) end),
    sout(N, "dict", "store", Di, Hi),
    {Dp, ok} = timeit(fun() -> dict_get(N, Td) end),
    sout(N, "fetch", Dp, Hp),
    {Du, _} = timeit(fun() -> dict_update(N, Td) end),
    sout(N, "update", Du, Hu),

%    {TAi, TAa} = timeit(fun() -> tarray_put(N) end),
%    sout(N, "tarray", "set", TAi, TAi),
%    {TAp, _  } = timeit(fun() -> tarray_get(N,TAa) end),
%    sout(N, "get", TAp, TAp),
%
%    {Ai, Ta} = timeit(fun() -> array_put(N) end),
%    sout(N, "array", "set", Ai, TAi),
%    {Ap, _ } = timeit(fun() -> array_get(N,Ta) end),
%    sout(N, "get", Ap, TAp),

    [
	{htrie, {Hi, Hp, Hu}},
	{hamt, {Pi, Pp, Pu}},
	{gb_trees, {Gi, Gp, Gu}},
	{dict, {Di, Dp, Du}}
    ].

sout(N, Mstr, Fstr, Val, Cval) ->
    Nf = length(Fstr),
    Bstr = lists:duplicate(12 - Nf, $ ),
    io:format("[~10w] ~8s:~s~s  ~12w | ~.2f~n", [N,Mstr, Fstr, Bstr, Val, Val/Cval]),
    ok.

sout(N, Fstr, Val, Cval) ->
    Nf = length(Fstr),
    Bstr = lists:duplicate(12 - Nf, $ ),
    io:format("[~10w]         :~s~s  ~12w | ~.2f~n", [N, Fstr, Bstr, Val, Val/Cval]),
    ok.
