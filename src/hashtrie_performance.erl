%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    hashtrie_performance.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-03-08
%%


-module(hashtrie_performance).

-record(options, {
	modules = [],
	inputs = [],
	iterations = 0,
	operations = [],
	path,
	fds
    }).

-export([
	run/1, run/2
    ]).

-define(value(X), X).
-define(key(X),X).
%-define(value(X), {<<"kfopekfekfpofkekfpwkfkepfkwpekfpkfekp">>, [3,[3,2]], X}).
%-define(key(X),[{kdoek,"diejdioejfoiejeoijfijfiej", what}, X]).

%% benchmark

execute(#options{ inputs = Is } = Opts0) ->
    Opts = open_files(Opts0),
    lists:map(fun(I) ->
		benchmark(I, Opts)
	end, Is),
    close_files(Opts).

benchmark(I, #options{ modules = Ms } = Opts) ->
    lists:map(fun({Mod, Data}) ->
		Operations = proplists:get_value(operations, Data),
		{Mod, benchmark_operations(I, Mod, Operations, Opts)}
	end, Ms).

benchmark_operations(N, Mod, Operations, Opts) ->
    C0 = apply_with_translation(Mod, new, [], Opts),
    map_operations(N, Mod, Operations, Opts, C0, [put, get, update]).

map_operations(_, _, _, _, _, []) -> [];
map_operations(N, Mod, Operations, Opts, C0, [Op|Ops]) ->
    case proplists:get_value(Op, Operations) of
	undefined ->
	    map_operations(N, Mod, Operations, Opts, C0, Ops);
	Fun ->
	    {Result, C1} = timeit(fun() -> Fun(N, C0, Fun) end, Opts),
	    Translated = translate_function(Mod, Op,Opts),
	    sout(N, Mod, Translated, Result),
	    pout(gb_trees:lookup({Mod, Op}, Opts#options.fds), N, Result),
	    [{Op, Result}|map_operations(N, Mod, Operations, Opts, C1, Ops)]
    end.

apply_with_translation(Mod, F, Args, Opts) ->
    erlang:apply(Mod, translate_function(Mod, F, Opts), Args).

translate_function(Mod, Function, #options{ modules = Mods }) ->
    Data  = proplists:get_value(Mod, Mods, []),
    Trans = proplists:get_value(translations, Data, []),
    translate_function(Function, Trans).

translate_function(Function, Translations) ->
    case proplists:get_value(Function, Translations) of
	undefined -> Function;
	Translation -> Translation
    end.



timeit(Fun, #options{ iterations = I}) ->
    Is  = lists:seq(1, I),
    Me  = self(),
    Pid = spawn_link(fun() -> 
		{T, R} = lists:foldl(fun(_, {T0, _}) ->
			    {Time, Res} = timer:tc(Fun),
			    {T0 + Time, Res}
		    end, {0, 0}, Is),
		Me ! {self(), result, {T div I, R}}
	end),
    receive 
	{Pid, result, Res} -> 
	    Res
    end.


%% parse specification

run(File) -> run(File, undefined).
run(File, Path) ->
    try
	Options = parse_specification(File),
	execute(Options#options{ path = Path }),
	ok
    catch
	Class:Reason ->
	    print_stacktrace(standard_error, Class, Reason, erlang:get_stacktrace()),
	    error
    end.


parse_specification(File) ->
    {ok, {specification, Spec}} = file:script(File),
    Options = proplists:get_value(options, Spec, []),
    Mods    = proplists:get_value(modules, Spec, []),
    Os1     = lists:foldl(fun
	    ({iterations, V}, Os) -> Os#options{ iterations = V };
	    ({inputs,     V}, Os) -> Os#options{ inputs = V};
	    ({operations, V}, Os) -> Os#options{ operations = V};
	    (Opt, Os) ->
		io:format(standard_error, "Warning: option ~p not recognized in file ~p~n", [Opt, File]),
		Os
	end, #options{}, Options),
    Os1#options{ modules = setup_modules(Mods, Os1#options.operations) }.


setup_modules([], _) -> [];
setup_modules([{Mod, Options}|Ms], Ops) ->
    [{Mod, setup_operations(Mod, Ops, Options)}|setup_modules(Ms, Ops)].

setup_operations(Mod, Ops, Options) ->
    Translations = proplists:get_value(translations, Options, []),
    Operations   = lists:map(fun(Op) ->
		{Op, setup_operation(Mod, Op, translate_function(Op, Translations))}
	end, Ops),
    [
	{operations, Operations},
	{translations, Translations}
    ].


setup_operation(M, get, Trans) ->
    fun
	(0, _, _) -> ok;
	(I, T, F) -> ?value(I) = M:Trans(?key(I), T), F(I - 1, T, F)
    end;
setup_operation(M, put, Trans) ->
    fun
	(0, T, _) -> T;
	(I, T, F) -> F(I - 1, M:Trans(?key(I), ?value(I), T), F)
    end;
setup_operation(M, update, Trans) ->
    fun
	(0, _, _) -> ok;
	(I, T, F) -> F(I - 1, M:Trans(?key(I), fun(V) -> [V,V] end, T), F)
    end.

%% print operations

sout(N, M, F, Value) ->
    Mstr = atom_to_list(M),
    Fstr = atom_to_list(F),
    Nf   = length(Fstr),
    Bstr = lists:duplicate(12 - Nf, $ ),
    io:format("[~10w] ~8s:~s~s  ~12w~n", [N,Mstr, Fstr, Bstr, Value]),
    ok.

pout(none, _N, _R) -> ok;
pout({value, Fd}, N, R) ->
    io:format(Fd, "~10w \t~w~n", [N, R]).


%% error handling

print_stacktrace(IO, Class, Reason, StackTrace) ->
    io:format(IO, "~s~n", [format_stacktrace(Class, Reason, StackTrace)]),
    ok.

format_stacktrace(Class, Reason, StackTrace) ->
    %% Class, Reason and StackTrace are the exception;
    %% FormatFun = fun(Term, I) -> iolist() formats terms;
    %% StackFun = fun(Mod, Fun, Arity) -> boolean() is used for trimming the
    %%   end of the stack (typically calls to erl_eval are skipped).

    FormatFun = fun
	(Term, _I) ->
	    io_lib:format("~p", [Term])
    end,

    % in call from erl_eval:do_apply/6 (erl_eval.erl, line 573)
    % in call from shell:exprs/7 (shell.erl, line 674)
    % in call from shell:eval_exprs/7 (shell.erl, line 629)
    % in call from shell:eval_loop/3 (shell.erl, line 614)

    StackFun = fun
	(erl_eval, do_apply, 6) -> true;
	(shell, exprs, 7) -> true;
	(shell, eval_exprs, 7) -> true;
	(shell, eval_loop, 3) -> true;
	(_Mod, _Fun, _Arity) -> false
    end,
    S = lib:format_exception(1, Class, Reason, StackTrace, StackFun, FormatFun),
    lists:flatten(S).
    
open_files(#options{ path = undefined } =Opts) -> Opts#options{ fds = gb_trees:empty() };
open_files(#options{ path = Path, modules = Ms } = Opts) ->
    Fds = lists:foldl(fun({M, Data}, T) ->
		Ops   = proplists:get_value(operations, Data),
		Trans = proplists:get_value(translations, Data),
		open_files(M, Ops, Trans, Path, T)
	end, gb_trees:empty(), Ms),
    Opts#options{ fds = Fds }.

open_files(_M, [], _, _, T) -> T;
open_files(M, [{Op,_}|Ops], Trans, Path, T) ->
    F    = translate_function(Op, Trans),
    Dat  = atom_to_list(M) ++ "_" ++ atom_to_list(F) ++ ".dat",
    File = filename:join([Path, Dat]),
    {ok, Fd} = file:open(File, [write]),
    open_files(M, Ops, Trans, Path, gb_trees:enter({M, Op}, Fd, T)).

close_files(#options{ fds = Fds }) ->
    [ ok = file:close(Fd) || {_, Fd} <- gb_trees:to_list(Fds) ],
    ok.



