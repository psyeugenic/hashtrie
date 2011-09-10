%% Copyright (C) 2011 Björn-Egil Dahlberg
%%
%% File:    test.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2011-09-01

%% test

-module(hashtrie_test).
-compile(export_all).

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


ht_put(N) -> ht_put(N, ht:new()).
ht_put(0, T) -> T;
ht_put(N, T) -> ht_put(N - 1, ht:put(?key(N), ?value(N), T)).

ht_get(0, _T) -> ok;
ht_get(N, T) -> 
    ?value(N) = ht:get(?key(N), T),
    ht_get(N - 1, T).

ht_update(0, T) -> T;
ht_update(N, T) -> 
    ht_update(N - 1, ht:update(?key(N), fun(V) -> [V,V] end, T)).

pht_put(N) -> pht_put(N, pht:new()).
pht_put(0, T) -> T;
pht_put(N, T) -> pht_put(N - 1, pht:put(?key(N), ?value(N), T)).

pht_get(0, _T) -> ok;
pht_get(N, T) -> 
    ?value(N) = pht:get(?key(N), T),
    pht_get(N - 1, T).

pht_update(0, T) -> T;
pht_update(N, T) -> 
    V = pht:get(N, T),
    pht_update(N - 1, pht:put(?key(N), [V,V], T)).

array_put(N)    -> array_put(N, array:new()).
array_put(0, T) -> T;
array_put(N, T) -> array_put(N - 1, array:set(?key(N), ?value(N), T)).

array_get(0, T) -> T;
array_get(N, T) ->
    ?value(N) = array:get(?key(N), T),
    array_get(N - 1, T).

tarray_put(N)    -> tarray_put(N, tarray:new()).
tarray_put(0, T) -> T;
tarray_put(N, T) -> tarray_put(N - 1, tarray:set(?key(N), ?value(N), T)).

tarray_get(0, T) -> T;
tarray_get(N, T) ->
    % io:format("ta get ~w -> ~w expecting ~w~n", [?key(N), tarray:get(?key(N), T), ?value(N)]), 
    ?value(N) = tarray:get(?key(N), T),
    tarray_get(N - 1, T).


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
	{ht_put, "data/ht_put.dat"},
	{ht_get, "data/ht_get.dat"},
	{ht_upd, "data/ht_update.dat"},
	{gb_put, "data/gb_trees_insert.dat"},
	{gb_get, "data/gb_trees_get.dat"},
	{gb_upd, "data/gb_trees_update.dat"},
	{di_put, "data/dict_store.dat"},
	{di_get, "data/dict_fetch.dat"},
	{di_upd, "data/dict_update.dat"}
    ],

    Fds = lists:foldl(fun
	    ({K, File}, T) ->
		{ok,Fd} = file:open(File, [write]),
		ht:put(K, Fd, T)
	end, ht:new(), Files),
    file(Ns, Fds),

    lists:foreach(fun
	    ({K,_}) -> 
		file:close(ht:get(K,Fds)) 
	end, Files),
    ok.




file([],_) -> ok; 
file([N|Ns], Fds) ->
    lists:foreach(fun
	    ({ht, {P, G, U}}) ->
		pout(ht:get(ht_put, Fds), N, P),
		pout(ht:get(ht_upd, Fds), N, U),
		pout(ht:get(ht_get, Fds), N, G);
	    ({dict, {P, G, U}}) ->
		pout(ht:get(di_put, Fds), N, P),
		pout(ht:get(di_upd, Fds), N, U),
		pout(ht:get(di_get, Fds), N, G);
	    ({gb_trees, {P, G, U}}) ->
		pout(ht:get(gb_put, Fds), N, P),
		pout(ht:get(gb_upd, Fds), N, U),
		pout(ht:get(gb_get, Fds), N, G)
	end, go(N)),
    file(Ns, Fds).


pout(Fd, N, R) ->
    io:format(Fd, "~10w \t~.4f~n", [N, R]).


go(N) when is_integer(N) ->
    {Hi, Th} = timeit(fun() -> hashtrie_test:ht_put(N) end),
    sout(N, "ht", "put", Hi, Hi),
    {Hp, ok} = timeit(fun() -> hashtrie_test:ht_get(N,Th) end),
    sout(N, "get", Hp, Hp),
    {Hu, _} = timeit(fun() -> hashtrie_test:ht_update(N,Th) end),
    sout(N, "update", Hu, Hu),

    {Pi, Tp} = timeit(fun() -> hashtrie_test:pht_put(N) end),
    sout(N, "pht", "put", Pi, Hi),
    {Pp, ok} = timeit(fun() -> hashtrie_test:pht_get(N,Tp) end),
    sout(N, "get", Pp, Hp),
    {Pu, _} = timeit(fun() -> hashtrie_test:pht_update(N,Tp) end),
    sout(N, "update", Pu, Hu),


    {Gi, Tg} = timeit(fun() -> hashtrie_test:gb_trees_put(N) end),
    sout(N, "gb_trees", "insert", Gi, Hi),
    {Gp, ok} = timeit(fun() -> hashtrie_test:gb_trees_get(N, Tg) end),
    sout(N, "get", Gp, Hp),
    {Gu, ok} = timeit(fun() ->hashtrie_test:gb_trees_update(N, Tg) end),
    sout(N, "update", Gu, Hu),

    {Di, Td} = timeit(fun() -> hashtrie_test:dict_put(N) end),
    sout(N, "dict", "store", Di, Hi),
    {Dp, ok} = timeit(fun() -> hashtrie_test:dict_get(N, Td) end),
    sout(N, "fetch", Dp, Hp),
    {Du, _} = timeit(fun() -> hashtrie_test:dict_update(N, Td) end),
    sout(N, "update", Du, Hu),

    {TAi, TAa} = timeit(fun() -> hashtrie_test:tarray_put(N) end),
    sout(N, "tarray", "set", TAi, TAi),
    {TAp, _  } = timeit(fun() -> hashtrie_test:tarray_get(N,TAa) end),
    sout(N, "get", TAp, TAp),

    {Ai, Ta} = timeit(fun() -> hashtrie_test:array_put(N) end),
    sout(N, "array", "set", Ai, TAi),
    {Ap, _ } = timeit(fun() -> hashtrie_test:array_get(N,Ta) end),
    sout(N, "get", Ap, TAp),

    [{ht, {Hi, Hp, Hu}}, {gb_trees, {Gi, Gp, Gu}}, {dict, {Di, Dp, Du}}].

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
