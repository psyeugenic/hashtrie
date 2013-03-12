%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    hashtrie.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-03-08
%%

-module(hashtrie).

-export([
	main/1
    ]).

main([]) ->
    io:format(standard_error, "hashtrie performance.spec [data-path]~n", []),
    ok;

main([File, Path]) -> hashtrie_performance:run(File, Path);
main([File]) -> hashtrie_performance:run(File).
