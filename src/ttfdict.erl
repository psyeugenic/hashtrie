-module(ttfdict).

-export([new/0]).

-export([fetch/2,find/2,store/3,erase/2]).

-ifdef(DEBUG).
-export([check_depth/1]).
-endif.

%% {Left,Key,Val,Right}
%% {Left,Key,Val,Middle,Key,Val,Right}
%% {A,Sk,Sv,B,Mk,Mv,C,Lk,Lv,D}
%% empty

new() -> empty.

fetch(K, {L,K1,_,_}) when K < K1 -> fetch(K, L);
fetch(K, {_,K1,_,R}) when K > K1 -> fetch(K, R);
fetch(_, {_,_,V,_}) -> V;
fetch(K, {L,K1,_,_,_,_,_}) when K < K1 -> fetch(K, L);
fetch(K, {_,K1,_,M,K2,V2,R}) when K > K1 ->
    if K < K2 -> fetch(K, M);			%Middle
       K > K2 -> fetch(K, R);			%Right
       true -> V2
    end;
fetch(_, {_,_,V1,_,_,_,_}) -> V1;
fetch(Key, {A,Sk,Sv,B,Mk,_,_,_,_,_}) when Key < Mk ->
    if Key < Sk -> fetch(Key, A);
       Key > Sk -> fetch(Key, B);
       true -> Sv
    end;
fetch(Key, {_,_,_,_,Mk,_,C,Lk,Lv,D}) when Key > Mk ->
    if Key < Lk -> fetch(Key, C);
       Key > Lk -> fetch(Key, D);
       true -> Lv
    end;
fetch(_, {_,_,_,_,_,Mv,_,_,_,_}) -> Mv.


find(K, {L,K1,_,_}) when K < K1 -> find(K, L);
find(K, {_,K1,_,R}) when K > K1 -> find(K, R);
find(_, {_,_,V,_}) -> {ok,V};
find(K, {L,K1,_,_,_,_,_}) when K < K1 -> find(K, L);
find(K, {_,K1,_,M,K2,V2,R}) when K > K1 ->
    if K < K2 -> find(K, M);			%Middle
       K > K2 -> find(K, R);			%Right
       true -> V2
    end;
find(_, {_,_,V1,_,_,_,_}) -> {ok,V1};
find(Key, {A,Sk,Sv,B,Mk,_,_,_,_,_}) when Key < Mk ->
    if Key < Sk -> find(Key, A);
       Key > Sk -> find(Key, B);
       true -> {ok,Sv}
    end;
find(Key, {_,_,_,_,Mk,_,C,Lk,Lv,D}) when Key > Mk ->
    if Key < Lk -> find(Key, C);
       Key > Lk -> find(Key, D);
       true -> {ok,Lv}
    end;
find(_, {_,_,_,_,_,Mv,_,_,_,_}) -> {ok,Mv};
find(_, empty) -> error.

store(Key, Val, T) ->
    %% Store and check for a returned "Up" node.
    case store_aux(Key, Val, T) of
	{up,Lu,Ku,Vu,Ru} -> {Lu,Ku,Vu,Ru};
	Node -> Node
    end.

store_aux(Key, Val, empty) -> {up,empty,Key,Val,empty};	%"Up" node
store_aux(Key, Val, {empty,K,V,empty}=Node) ->
    %% Special case to avoid creating temporary "up" nodes.
    %% It flattens tree and helps a little bit, but not much.
    if Key < K ->
	    {empty,Key,Val,empty,K,V,empty};
       Key > K ->
	    {empty,K,V,empty,Key,Val,empty};
       true -> {empty,Key,Val,empty}
    end;
store_aux(Key, Val, {L,K,V,R}) ->
    if Key < K ->				%Down the left
	    store_up2_l(store_aux(Key, Val, L), K, V, R);
       Key > K ->				%Down the right
	    store_up2_r(L, K, V, store_aux(Key, Val, R));
       true -> {L,Key,Val,R}			%Replace current value
    end;
store_aux(Key, Val, {empty,Kl,Vl,_,Kr,Vr,_}=Node) ->
    if Key < Kl ->
	    {empty,Key,Val,empty,Kl,Vl,empty,Kr,Vr,empty};
       Key > Kl ->
	    if Key < Kr ->
		    {empty,Kl,Vl,empty,Key,Val,empty,Kr,Vr,empty};
	       Key > Kr ->
		    {empty,Kl,Vl,empty,Kr,Vr,empty,Key,Val,empty};
	       true ->
		    {empty,Kl,Vl,empty,Key,Val,empty}
	    end;
       true -> {empty,Key,Val,empty,Kr,Vr,empty}
    end;
store_aux(Key, Val, {L,Kl,Vl,M,Kr,Vr,R}) when Key < Kl ->
    store_up3_l(store_aux(Key, Val, L), Kl, Vl, M, Kr, Vr, R);
store_aux(Key, Val, {L,Kl,Vl,M,Kr,Vr,R}) when Key > Kl ->
    if Key < Kr ->
	    store_up3_m(L, Kl, Vl, store_aux(Key, Val, M), Kr, Vr, R);
       Key > Kr ->
	    store_up3_r(L, Kl, Vl, M, Kr, Vr, store_aux(Key, Val, R));
       true -> {L,Kl,Vl,M,Key,Val,R}
    end;
store_aux(Key, Val, {L,_,_,M,Kr,Vr,R}) ->
    {L,Key,Val,M,Kr,Vr,R};
%% Split 4-nodes into 2 2-nodes on the way down.
store_aux(Key, Val, {A,Sk,Sv,B,Mk,Mv,C,Lk,Lv,D}) when Key < Mk ->
    if Key < Sk ->
	    case store_aux(Key, Val, A) of
		{up,Ua,Uk,Uv,Ub} ->
		    {up,{Ua,Uk,Uv,Ub,Sk,Sv,B},Mk,Mv,{C,Lk,Lv,D}};
		A1 -> {A1,Sk,Sv,B,Mk,Mv,C,Lk,Lv,D}
	    end;
       Key > Sk ->
	    case store_aux(Key, Val, B) of
		{up,Ua,Uk,Uv,Ub} ->
		    {up,{A,Sk,Sv,Ua,Uk,Uv,Ub},Mk,Mv,{C,Lk,Lv,D}};
		B1 -> {A,Sk,Sv,B1,Mk,Mv,C,Lk,Lv,D}
	    end;
       true -> {A,Key,Val,B,Mk,Mv,C,Lk,Lv,D}
    end;
%% Worked but always removed 4-node!
%%     Left = {A,Sk,Sv,B},				%Left/right 2-trees
%%     Right = {C,Lk,Lv,D},
%%     case store_aux(Key, Val, Left) of
%% 	{up,Na,Nk,Nv,Nb} ->
%% 	    {up,{Na,Nk,Nv,Nb},Mk,Mv,Right};
%% 	Left1 -> {up,Left1,Mk,Mv,Right}
%%     end;
store_aux(Key, Val, {A,Sk,Sv,B,Mk,Mv,C,Lk,Lv,D}) when Key > Mk ->
    if Key < Lk ->
	    case store_aux(Key, Val, C) of
		{up,Ua,Uk,Uv,Ub} ->
		    {up,{A,Sk,Sv,B},Mk,Mv,{Ua,Uk,Uv,Ub,Lk,Lv,D}};
		C1 -> {A,Sk,Sv,B,Mk,Mv,C1,Lk,Lv,D}
	    end;
       Key > Lk ->
	    case store_aux(Key, Val, D) of
		{up,Ua,Uk,Uv,Ub} ->
		    {up,{A,Sk,Sv,B},Mk,Mv,{C,Lk,Lv,Ua,Uk,Uv,Ub}};
		D1 -> {A,Sk,Sv,B,Mk,Mv,C,Lk,Lv,D1}
	    end;
       true -> {A,Sk,Sv,B,Mk,Mv,C,Key,Val,D}
    end;
%%     Left = {A,Sk,Sv,B},				%Left/right 2-trees
%%     Right = {C,Lk,Lv,D},
%%     case store_aux(Key, Val, Right) of
%% 	{up,Na,Nk,Nv,Nb} ->
%% 	    {up,Left,Mk,Mv,{Na,Nk,Nv,Nb}};
%% 	Right1 -> {up,Left,Mk,Mv,Right1}
%%     end;
store_aux(Key, Val, {A,Sk,Sv,B,_,_,C,Lk,Lv,D}) ->
    {A,Sk,Sv,B,Key,Val,C,Lk,Lv,D}.


%% store_up2_l/r(L, K, V, R) -> {L,K,V,R} | {L,Kl,Vl,M,Kr,Vr,R}.

store_up2_l({up,Lu,Ku,Vu,Ru}, K, V, R) ->
    {Lu,Ku,Vu,Ru,K,V,R};
store_up2_l(L, K, V, R) -> {L,K,V,R}.

store_up2_r(L, K, V, {up,Lu,Ku,Vu,Ru}) ->
    {L,K,V,Lu,Ku,Vu,Ru};
store_up2_r(L, K, V, R) -> {L,K,V,R}.

%% store_up3_l/m/r(L, Kl, Vl, M, Kr, Vr, R) ->
%%     {L,Kl,Vl,M,Kr,Vr,R} | {A,Sk,Sv,B,Mk,Mv,C,Lk,Lv,D}.

store_up3_l({up,Lu,Ku,Vu,Ru}, Kl, Vl, M, Kr, Vr, R) ->
    {Lu,Ku,Vu,Ru,Kl,Vl,M,Kr,Vr,R};
store_up3_l(L, Kl, Vl, M, Kr, Vr, R) ->
    {L,Kl,Vl,M,Kr,Vr,R}.

store_up3_m(L, Kl, Vl, {up,Lu,Ku,Vu,Ru}, Kr, Vr, R) ->
    {L,Kl,Vl,Lu,Ku,Vu,Ru,Kr,Vr,R};
store_up3_m(L, Kl, Vl, M, Kr, Vr, R) ->
    {L,Kl,Vl,M,Kr,Vr,R}.

store_up3_r(L, Kl, Vl, M, Kr, Vr, {up,Lu,Ku,Vu,Ru}) ->
    {L,Kl,Vl,M,Kr,Vr,Lu,Ku,Vu,Ru};
store_up3_r(L, Kl, Vl, M, Kr, Vr, R) ->
    {L,Kl,Vl,M,Kr,Vr,R}.

erase(Key, T) ->
    case erase_aux(Key, T) of
	{up,T1} -> T1;				%???
	T1 -> T1
    end.

erase_aux(_, empty) -> empty;			%No element
erase_aux(Key, {empty,K,_,empty}=N) ->
    if Key < K; Key > K -> N;			%No element
       true -> {up,empty}
    end;
erase_aux(Key, {L,K,V,R}) ->
    if Key < K ->				%Down the left
	    erase_up2_l(erase_aux(Key, L), K, V, R);
       Key > K ->				%Down the right
	    erase_up2_r(L, K, V, erase_aux(Key, R));
       true ->
	    {R1,{Km,Vm}}= erase_min(R),
	    erase_up2_r(L, Km, Vm, R1)
    end;
erase_aux(Key, {empty,Kl,Vl,empty,Kr,Vr,empty}=N) ->
    if Key < Kl -> N;				%No element
       Key > Kl ->
	    if Key < Kr -> N;			%No element
	       Key > Kr -> N;
	       true -> {empty,Kl,Vl,empty}
	    end;
       true -> {empty,Kr,Vr,empty}
    end;
erase_aux(Key, {L,Kl,Vl,M,Kr,Vr,R}) when Key < Kl ->
    L1 = erase_aux(Key, L),
    erase_up3_l(L1, Kl, Vl, M, Kr, Vr, R);
erase_aux(Key, {L,Kl,Vl,M,Kr,Vr,R}) when Key > Kl ->
    if Key < Kr ->
	    M1 = erase_aux(Key, M),
	    erase_up3_m(L, Kl, Vl, M1, Kr, Vr, R);
       Key > Kr ->
	    R1 = erase_aux(Key, R),
	    erase_up3_r(L, Kl, Vl, M, Kr, Vr, R1);
       true ->
	    {R1,{Km,Vm}} = erase_min(R),
	    erase_up3_r(L, Kl, Vl, M, Km, Vm, R1)
    end;
erase_aux(_, {L,_,_,M,Kr,Vr,R}) ->
    {M1,{Km,Vm}} = erase_min(M),
    erase_up3_m(L, Km, Vm, M1, Kr, Vr, R).

erase_min(T) ->
    %%io:format("em: ~p\n->  ~p\n", [T,T1]),
    erase_min1(T).

erase_min1({empty,K,V,empty}) -> {{up,empty},{K,V}};
erase_min1({L,K,V,R}) ->
    {L1,Min} = erase_min1(L),
    {erase_up2_l(L1, K, V, R),Min};
erase_min1({empty,Kl,Vl,empty,Kr,Vr,empty}) ->
    {{empty,Kr,Vr,empty},{Kl,Vl}};
erase_min1({L,Kl,Vl,M,Kr,Vr,R}) ->
    {L1,Min} = erase_min1(L),
    {erase_up3_l(L1, Kl, Vl, M, Kr, Vr, R),Min}.

%% erase_up2_l/r(L, K, V, R) -> Node | {up,Node}.
%% We use the same naming of nodes and keys as in the text. It makes
%% checking the rules easier.

erase_up2_l({up,L}, Xk, Xv, {M,Yk,Yv,R}) ->	%1.1
    {up,{L,Xk,Xv,M,Yk,Yv,R}};
erase_up2_l({up,A}, Xk, Xv, {B,Yk,Yv,C,Zk,Zv,D}) -> %2.1
    {{A,Xk,Xv,B},Yk,Yv,{C,Zk,Zv,D}};
erase_up2_l(L, K, V, R) -> {L,K,V,R}.

erase_up2_r({L,Xk,Xv,M}, Yk, Yv, {up,R}) ->	%1.2
    {up,{L,Xk,Xv,M,Yk,Yv,R}};
erase_up2_r({A,Xk,Xv,B,Yk,Yv,C}, Zk, Zv, {up,D}) -> %2.2
    {{A,Xk,Xv,B},Yk,Yv,{C,Zk,Zv,D}};
erase_up2_r(L, K, V, R) -> {L,K,V,R}.

%% erase_up3_l/m/r(L, Kl, Vl, M, Kr, Vr, R) -> Node | {up,Node}.
%% We use the same naming of nodes and keys as in the text. It makes
%% checking the rules easier.

erase_up3_l({up,A}, Xk, Xv, {B,Yk,Yv,C}, Zk, Zv, D) -> %3a.1
    {{A,Xk,Xv,B,Yk,Yv,C},Zk,Zv,D};
erase_up3_l({up,A}, Wk, Wv, {B,Xk,Xv,C,Yk,Yv,D}, Zk, Zv, E) -> %4a.1
    {{A,Wk,Wv,B},Xk,Xv,{C,Yk,Yv,D},Zk,Zv,E};
erase_up3_l(L, Kl, Vl, M, Kr, Vr, R) ->
    {L,Kl,Vl,M,Kr,Vr,R}.

erase_up3_m({A,Xk,Xv,B}, Yk, Yv, {up,C}, Zk, Zv, D) -> %3a.2
    {{A,Xk,Xv,B,Yk,Yv,C},Zk,Zv,D};
erase_up3_m(A, Xk, Xv, {up,B}, Yk, Yv, {C,Zk,Zv,D}) -> %3b.1
    {A,Xk,Xv,{B,Yk,Yv,C,Zk,Zv,D}};
erase_up3_m({A,Wk,Wv,B,Xk,Xv,C}, Yk, Yv, {up,D}, Zk, Zv, E) -> %4a.2
    {{A,Wk,Wv,B},Xk,Xv,{C,Yk,Yv,D},Zk,Zv,E};
erase_up3_m(A, Wk, Wv, {up,B}, Xk, Xv, {C,Yk,Yv,D,Zk,Zv,E}) -> %4b.1
    {A,Wk,Wv,{B,Xk,Xv,C},Yk,Yv,{D,Zk,Zv,E}};
erase_up3_m(L, Kl, Vl, M, Kr, Vr, R) ->
    {L,Kl,Vl,M,Kr,Vr,R}.

erase_up3_r(A, Xk, Xv, {B,Yk,Yv,C}, Zk, Zv, {up,D}) -> %3b.2
    {A,Xk,Xv,{B,Yk,Yv,C,Zk,Zv,D}};
erase_up3_r(A, Wk, Wv, {B,Xk,Xv,C,Yk,Yv,D}, Zk, Zv, {up,E}) -> %4b.2
    {A,Wk,Wv,{B,Xk,Xv,C},Yk,Yv,{D,Zk,Zv,E}};
erase_up3_r(L, Kl, Vl, M, Kr, Vr, R) ->
    {L,Kl,Vl,M,Kr,Vr,R}.

-ifdef(DEBUG).

%% Check the depth of all the leaves, should all be the same.
check_depth(T) -> check_depth(T, 1, orddict:new()).

check_depth(empty, D, Dd) ->
    orddict:update_counter(D, 1, Dd);
check_depth({L,_,_,R}, D, Dd0) ->
    Dd1 = orddict:update_counter(two, 1, Dd0),
    Dd2 = check_depth(L, D+1, Dd1),
    check_depth(R, D+1, Dd2);
check_depth({L,_,_,M,_,_,R}, D, Dd0) ->
    Dd1 = orddict:update_counter(three, 1, Dd0),
    Dd2 = check_depth(L, D+1, Dd1),
    Dd3 = check_depth(M, D+1, Dd2),
    check_depth(R, D+1, Dd3);
check_depth({M,_,_,N,_,_,O,_,_,P}, D, Dd0) ->
    Dd1 = orddict:update_counter(four, 1, Dd0),
    Dd2 = check_depth(M, D+1, Dd1),
    Dd3 = check_depth(N, D+1, Dd2),
    Dd4 = check_depth(O, D+1, Dd3),
    check_depth(P, D+1, Dd4).

-endif.
