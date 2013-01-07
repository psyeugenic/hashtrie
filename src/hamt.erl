%% Copyright (C) 2011 Björn-Egil Dahlberg
%%
%% File:    hamt.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2011-09-08

-module(hamt).

-export([
	new/0,
	put/3,
	get/2
    ]).

%% testing
-export([
	go/1
    ]).

-record(index, {
	bitmask = 0,
	array   = {}
    }).

-record(full, {
	array = {}
    }).

%% defines

-define(node_size_32, 1).

-ifdef(node_size_8).
-define(bitmap_is_full(Bm), Bm =:= 16#FF).
-define(level_shift, 3).
-define(index_mask(Hx), Hx band 7).
-endif.

-ifdef(node_size_16).
-define(bitmap_is_full(Bm), Bm =:= 16#FFFF).
-define(level_shift,4).
-define(index_mask(Hx), Hx band 15).
-endif.

-ifdef(node_size_32).
-define(bitmap_is_full(Bm), Bm =:= 16#FFFFFF).
-define(level_shift,5).
-define(index_mask(Hx), Hx band 31).
-endif.


-define(ielement(Ix, T, V), insert_element(Ix, T, V)).
%-define(ielement(Ix, T, V), erlang:insert_element(Ix, T, V)).
-define(bitpop(Ix,Bm,C), bitcount(Ix,Bm,C)).
%-define(bitpop(Ix,Bm,_), erlang:bitcount( (Ix) band (Bm))).
			    
%% #full{}  = full node
%% #index{} = index node
%% [K|V]    = leaf node

go(N) ->
    lists:foldl(fun(I, O) ->
	    O1 = ?MODULE:put(I, I, O),
	    [io:format("get ~w -> ~w~n", [Ix, ?MODULE:get(Ix, O1)]) || Ix <- lists:seq(1, I)],
	    O1
	end, ?MODULE:new(), lists:seq(1,N)),
    ok.

new() -> #index{ bitmask = 0, array = {} }.

put(K, V, T) ->
    put(erlang:phash2(K), 0, K, V, T).

get(K, T) ->
    get(erlang:phash2(K), K, T).

put(Hx, Lvl, K,V, #index{ bitmask = Bm, array = A} = Ni) ->
    Ix   = ?index_mask(Hx),
    Bp   = 1 bsl Ix,   % bit position
    Slot = ?bitpop(Bm, Bp - 1, Ix) + 1,

    if 
	Bm band Bp > 0 ->
	    % array[Ix] occupied, traverse down
	    Next = case element(Slot, A) of
		[K|_]   -> [K|V];
		[K0|V0] ->
		    Ns = put(Hx bsr ?level_shift, Lvl + ?level_shift, K, V, new()),
		    put(erlang:phash2(K0) bsr (Lvl + ?level_shift), Lvl + ?level_shift, K0, V0, Ns);
		Node ->
		    put(Hx bsr ?level_shift, Lvl + ?level_shift, K, V, Node)
	    end,

	    Ni#index{ 
		array = setelement(Slot, A, Next)
	    };
	true ->
	    % array[Ix] *not* occupied, set it
	    Bm1 = Bm bor Bp,
	    A1 = ?ielement(Slot, A, [K|V]),
	    if ?bitmap_is_full(Bm1) ->
		    #full { array = A1 };
		true ->
		    #index{ bitmask = Bm1, array = A1 }
	    end
    end;
put(Hx, Lvl, K,V, #full{ array = A} = N) ->
    Ix = ?index_mask(Hx) + 1,
    Next = case element(Ix, A) of
	[K|_]   -> [K|V];
	[K0|V0] ->
	    Ns = put(Hx bsr ?level_shift, Lvl + ?level_shift, K, V, new()),
	    put(erlang:phash2(K0) bsr (Lvl + ?level_shift), Lvl + ?level_shift, K0, V0, Ns);
	Node ->
	    put(Hx bsr ?level_shift, Lvl + ?level_shift, K, V, Node)
    end,
    N#full{ array = setelement(Ix, A, Next) }.

get(Hx, K,#index{ bitmask = Bm, array = A}) ->
    Ix = ?index_mask(Hx),
    Bp = 1 bsl Ix,
    if Bm band Bp > 0 ->
	    Slot = ?bitpop(Bm, Bp - 1, Ix) + 1,
	    get(Hx bsr ?level_shift, K, element(Slot, A));
	true -> undefined
    end;
get(Hx, K, #full{ array = A }) ->
    Ix = ?index_mask(Hx) + 1,
    get(Hx bsr ?level_shift, K, element(Ix, A));
get(_, K, [K|V]) -> V;
get(_, _, [_|_]) -> undefined.

% should be bifs

bitcount(Bm, Mask, I) -> bc(Bm band Mask, I).
bc( _, 0) -> 0;
bc(B, I) -> bc(B bsr 1, I - 1) + B band 1.

% insert_element(2,{a,b,c},gg) -> {a, gg, b, c}
insert_element(I, A, V) when is_tuple(A), is_integer(I), I > 0 ->
    Vs = tuple_to_list(A),
    list_to_tuple(lelement(I, Vs, V)).

lelement(1, Vs, V) -> [V|Vs];
lelement(I, [V1 | Vs], V) -> [V1 | lelement(I - 1, Vs, V)].

