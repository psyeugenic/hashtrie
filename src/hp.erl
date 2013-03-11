%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    hp.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-03-08
%%

-module(hp).

-compile([export_all]).

go() ->
    hashtrie_performance:run("performance.spec").
