%% Copyright (C) 2011 Björn-Egil Dahlberg
%%
%% File:    pht.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2011-09-08

-module(pht).

-compile([export_all]).

-record(index, {
	bitmask = 0,
	array   = {}
    }).
-record( full, { array = {}}).

%% defines

-define(bitmap_is_full(Bm), Bm =:= 16#FF).
-define(level_shift, 3).
-define(index_mask(Hx), Hx band 7).


%% #full{}  = full node
%% #index{} = index node
%% [K|V]    = leaf node


new() -> #index{ bitmask = 0, array = {} }.

put(K, V, T) ->
    put(erlang:phash2(K), 0, K, V, T).

get(K, T) ->
    get(erlang:phash2(K), K, T).

put(Hx, Lvl, K,V, #index{ bitmask = Bm, array = A} = Ni) ->
    Ix   = ?index_mask(Hx),
    Bp   = 1 bsl Ix,   % bit position
    Slot = bitcount(Bm, Bp - 1, Ix) + 1,

    if 
	Bm band Bp > 0 ->
	    % array[Ix] occupied, traverse down
	    Ni#index{ 
		array = setelement(Slot, A, put(Hx bsr ?level_shift, Lvl + ?level_shift, K, V, element(Slot, A)))
	    };
	true ->
	    % array[Ix] *not* occupied, set it
	    Bm1 = Bm bor Bp,
	    A1 = insert_element(Slot, A, [K|V]),
	    if ?bitmap_is_full(Bm1) ->
		    #full { array = A1 };
		true ->
		    #index{ bitmask = Bm bor Bp, array = A1 }
	    end
    end;
put(_, _, K, V, [K|_]) -> 
    [K|V];
put(Hx, Lvl, K, V, [K0|V0]) ->
    N = put(erlang:phash2(K0) bsr (Lvl + ?level_shift), Lvl + ?level_shift, K0, V0, new()),
    put(Hx, Lvl, K, V, N);
put(Hx, Lvl, K,V, #full{ array = A} = N) ->
    Ix = ?index_mask(Hx) + 1,
    A1 = setelement(Ix, A, put(Hx bsr ?level_shift, Lvl + ?level_shift, K, V, element(Ix, A))),
    N#full{ array = A1 }.

get(Hx, K,#index{ bitmask = Bm, array = A}) ->
    Ix = ?index_mask(Hx),
    Bp = 1 bsl Ix,
    if Bm band Bp > 0 ->
	    Slot = bitcount(Bm, Bp - 1, Ix) + 1,
	    get(Hx bsr ?level_shift, K, element(Slot, A));
	true -> undefined
    end;
get(Hx, K, #full{ array = A }) ->
    Ix = ?index_mask(Hx) + 1,
    get(Hx, K, element(Ix, A));
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

