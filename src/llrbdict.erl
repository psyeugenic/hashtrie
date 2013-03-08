%% Copyright (c) 2009 Robert Virding. All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%% File    : llrbdict.erl
%% Author  : Robert Virding
%% Purpose : Key-Value dictionary as a Left Leaning Red-Black tree.

-module(llrbdict).

%% Standard interface.
-export([new/0,is_key/2,to_list/1,from_list/1,size/1]).
-export([fetch/2,find/2,fetch_keys/1,erase/2]).
-export([store/3]).

-ifdef(DEBUG).
-export([check/1,erase_check/2,check_depth/1,t/1,r1/0,r2/0]).
-endif.

%% The algorithms here are taken directly from Okasaki and Rbset in
%% ML/Scheme. The interface is compatible with the standard dict
%% interface.
%%
%% The following structures are used to build the RB-dict:
%%
%% {r,Left,Key,Val,Right}
%% {b,Left,Key,Val,Right}
%% empty
%%
%% It is interesting to note that expanding out the first argument of
%% l/rbalance, the colour, in store etc. is actually slower than not
%% doing it. Measured.

-type llrbdict() :: 'empty' |
		    {'b', 'empty', any(), any(), 'empty'} |
		    {'b',
		     'empty',
		     any(), any(),
		     {'r', 'empty', any(), any(), 'empty'}} |
		    {'b',
		     {'r', 'empty', any(), any(), 'empty'},
		     any(), any(),
		     'empty'} |
		    {'b',
		     {'r', 'empty', any(), any(), 'empty'},
		     any(), any(),
		     {'r', 'empty', any(), any(), 'empty'}} |
		    {'b',
		     {'r' | 'b', tuple(), any(), any(), tuple()},
		     any(), any(),
		     {'r' | 'b', tuple(), any(), any(), tuple()}}.

-spec new() -> 'empty'.

%% new() -> Dict.

new() -> empty.

-spec is_key(any(), llrbdict()) -> boolean().

%% is_key(Key, Dict) -> true | false.

is_key(_, empty) -> false;
is_key(K, {_,Left,K1,_,_}) when K < K1 ->
    is_key(K, Left);
is_key(K, {_,_,K1,_,Right}) when K > K1 ->
    is_key(K, Right);
is_key(_, {_,_,_,_,_}) -> true.

-spec to_list(llrbdict()) -> list({any(), any()}).

%% to_list(Dict) -> [{Key,Value}].

to_list(T) -> to_list(T, []).

to_list(empty, List) -> List;
to_list({_,A,Xk,Xv,B}, List) ->
    to_list(A, [{Xk,Xv}|to_list(B, List)]).

-spec from_list(list({any(), any()})) -> llrbdict().

%% from_list([{Key,Value}]) -> Dict.

from_list(L) ->
    lists:foldl(fun ({K,V}, D) -> store(K, V, D) end, new(), L).

-spec size(llrbdict()) -> non_neg_integer().

%% size(Dict) -> int().

size(T) -> size1(T).

size1(empty) -> 0;
size1({_,L,_,_,R}) ->
    size1(L) + size1(R) + 1.

-spec fetch(any(), llrbdict()) -> any().

%% fetch(Key, Dict) -> Value.

fetch(K, {_,Left,K1,_,_}) when K < K1 ->
    fetch(K, Left);
fetch(K, {_,_,K1,_,Right}) when K > K1 ->
    fetch(K, Right);
fetch(_, {_,_,_,Val,_}) -> Val.

-spec find(any(), llrbdict()) -> {'ok', any()} | 'error'.

%% find(Key, Dict) -> {ok,Value} | error.

find(_, empty) -> error;
find(K, {_,Left,K1,_,_}) when K < K1 ->
    find(K, Left);
find(K, {_,_,K1,_,Right}) when K > K1 ->
    find(K, Right);
find(_, {_,_,_,Val,_}) -> {ok,Val}.

-spec fetch_keys(llrbdict()) -> list(any()).

%% fetch_keys(Dict) -> [Key].

fetch_keys(T) -> fetch_keys(T, []).

fetch_keys(empty, Tail) -> Tail;
fetch_keys({_,L,K,_,R}, Tail) ->
    fetch_keys(L, [K|fetch_keys(R, Tail)]).

-spec store(any(), any(), llrbdict()) -> llrbdict().

%% store(Key, Val, Dict) -> Dict.

store(K, V, T) ->
    {_,L,K1,V1,R} = store_aux(K, V, T),
    {b,L,K1,V1,R}.				%setelement(1, b, T1).

store_aux(K, V, empty) -> {r,empty,K,V,empty};
store_aux(K, V, {C,Left,K1,V1,Right}) when K < K1 ->
    lbalance(C, store_aux(K, V, Left), K1, V1, Right);
store_aux(K, V, {C,Left,K1,V1,Right}) when K > K1 ->
    rbalance(C, Left, K1, V1, store_aux(K, V, Right));
store_aux(K, V, {C,L,_,_,R}) ->
    {C,L,K,V,R}.

%% lbalance(Colour, Left, Key, Val, Right).
%% rbalance(Colour, Left, Key, Val, Right).
%%  Balance a tree afer (possibly) adding a node to the left/right.

lbalance(b, {r,{r,A,Xk,Xv,B},Yk,Yv,C}, Zk, Zv, D) ->
    {r,{b,A,Xk,Xv,B},Yk,Yv,{b,C,Zk,Zv,D}};
lbalance(C, A, Xk, Xv, B) -> {C,A,Xk,Xv,B}.

rbalance(b, {r,A,Xk,Xv,B}, Yk, Yv, {r,C,Zk,Zv,D}) ->
    {r,{b,A,Xk,Xv,B},Yk,Yv,{b,C,Zk,Zv,D}};
rbalance(K, A, Xk, Xv, {r,C,Zk,Zv,D}) ->
    {K,{r,A,Xk,Xv,C},Zk,Zv,D};
rbalance(K, A, Xk, Xv, B) -> {K,A,Xk,Xv,B}.

-spec erase(any(), llrbdict()) -> llrbdict().

%% erase(Key, Dict) -> Dict.

erase(K, T) ->
    {T1,_} = erase_aux(K, T),
    T1.

%% erase_aux(Key, Node) -> {Node,Decreased}.

erase_aux(_, empty) -> {empty,false};
erase_aux(K, {b,A,Xk,Xv,B}) ->
    if K < Xk ->
	    {A1,Dec} = erase_aux(K, A),
	    if  Dec -> unbalright(b, A1, Xk, Xv, B);
		true -> {{b,A1,Xk,Xv,B},false}
	    end;
       K > Xk ->
	    {B1,Dec} = erase_aux(K, B),
	    if  Dec -> unballeft(b, A, Xk, Xv, B1);
		true -> {{b,A,Xk,Xv,B1},false}
	    end;
       true ->
	    case B of
		empty -> blackify(A);
		_ ->
		    {B1,{Mk,Mv},Dec} = erase_min(B),
		    if  Dec -> unballeft(b, A, Mk, Mv, B1);
			true -> {{b,A,Mk,Mv,B1},false}
		    end
	    end
    end;
erase_aux(K, {r,A,Xk,Xv,B}) ->
    if K < Xk ->
	    {A1,Dec} = erase_aux(K, A),
	    if  Dec -> unbalright(r, A1, Xk, Xv, B);
		true -> {{r,A1,Xk,Xv,B},false}
	    end;
       K > Xk ->
	    {B1,Dec} = erase_aux(K, B),
	    if  Dec -> unballeft(r, A, Xk, Xv, B1);
		true -> {{r,A,Xk,Xv,B1},false}
	    end;
       true ->
	    case B of
		empty -> {A,false};
		_ ->
		    {B1,{Mk,Mv},Dec} = erase_min(B),
		    if  Dec -> unballeft(r, A, Mk, Mv, B1);
			true -> {{r,A,Mk,Mv,B1},false}
		    end
	    end
    end.

%% erase_min(Node) -> {Node,{NodeKey,NodeVal},Decreased}.

erase_min({b,empty,Xk,Xv,empty}) ->
    {empty,{Xk,Xv},true};
erase_min({b,empty,Xk,Xv,{r,A,Yk,Yv,B}}) ->
    {{b,A,Yk,Yv,B},{Xk,Xv},false};
erase_min({b,empty,_,_,{b,_,_,_,_}}) -> exit(boom);
erase_min({r,empty,Xk,Xv,A}) ->
    {A,{Xk,Xv},false};
%% Rec from left
erase_min({b,A,Xk,Xv,B}) ->
    {A1,Min,Dec} = erase_min(A),
    if Dec ->
	    {T,Dec1} = unbalright(b, A1, Xk, Xv, B),
	    {T,Min,Dec1};
       true -> {{b,A1,Xk,Xv,B},Min,false}
    end;
erase_min({r,A,Xk,Xv,B}) ->
    {A1,Min,Dec} = erase_min(A),
    if Dec ->
	    {T,Dec1} = unbalright(r, A1, Xk, Xv, B),
	    {T,Min,Dec1};
       true -> {{r,A1,Xk,Xv,B},Min,false}
    end.

blackify({r,A,K,V,B}) -> {{b,A,K,V,B},false};
blackify(Node) -> {Node,true}.

unballeft(r, {b,A,Xk,Xv,B}, Yk, Yv, C) ->
    {lbalance(b, {r,A,Xk,Xv,B}, Yk, Yv, C),false};
unballeft(b, {b,A,Xk,Xv,B}, Yk, Yv, C) ->
    {lbalance(b, {r,A,Xk,Xv,B},Yk, Yv, C),true};
unballeft(b, {r,A,Xk,Xv,{b,B,Yk,Yv,C}}, Zk, Zv, D) ->
    {{b,A,Xk,Xv,lbalance(b, {r,B,Yk,Yv,C}, Zk, Zv, D)},false}.

unbalright(r, A, Xk, Xv, {b,B,Yk,Yv,C}) ->
    {rbalance(b, A, Xk, Xv, {r,B,Yk,Yv,C}),false};
unbalright(b, A, Xk, Xv, {b,B,Yk,Yv,C}) ->
    {rbalance(b, A, Xk, Xv, {r,B,Yk,Yv,C}),true};
unbalright(b, A, Xk, Xv, {r,{b,B,Yk,Yv,C},Zk,Zv,D}) ->
    {{b,rbalance(b, A, Xk, Xv, {r,B,Yk,Yv,C}), Zk, Zv, D},false}.

-ifdef(DEBUG).
%% Test functions.

erase_check(K, T) ->
    T1 = erase(K, T),
    check(T1),
    T1.

check(T) -> check(T, r).

check(empty, _) -> 1;
check({r,A,Xk,Xv,B}, b) ->		       	%Must have black parent
    case {check(A, r),check(B, r)} of
	{D,D}-> D;
	{Dl,Dr} -> exit({depth,{r,Dl,Xk,Xv,Dr}})
    end;
check({r,_,Xk,Xv,_}, r) ->		       	%Must have black parent
    exit({parent,{r,'-',Xk,Xv,'-'}});
check({b,A,Xk,Xv,B}, _) ->
    case {check(A, b),check(B,b)} of
	{D,D}-> D+1;				%Increase depth
	{Dl,Dr} -> exit({depth,{b,Dl,Xk,Xv,Dr}})
    end.

check_depth(T) -> check_depth(T, 1, orddict:new()).

check_depth(empty, D, Dd) ->
    orddict:update_counter(D, 1, Dd);
check_depth({_,A,_,_,B}, D, Dd0) ->
    Dd1 = orddict:update_counter(D, 1, Dd0),
    Dd2 = check_depth(A, D+1, Dd1),
    check_depth(B, D+1, Dd2).

t(Ks) ->
    lists:foldl(fun (K, D) -> store(K, K, D) end, new(), Ks).

%% Known error cases which have been fixed.

r1() ->
    {{b,{b,empty,37,37,empty},
       38,
       38,
       {b,{r,empty,39,39,empty},40,40,empty}},
     39,
     {b,{r,empty,37,37,empty},38,38,{b,empty,40,40,empty}}}.

r2() ->
    {{b,{r,{b,empty,43,43,empty},
	   46,
	   46,
	   {b,empty,48,48,empty}},
	50,
	50,
	{b,empty,53,53,empty}},
     53,
     {b,{b,empty,43,43,empty},
	46,
	46,
	{r,{b,empty,48,48,empty},50,50,empty}}}.
-endif.
