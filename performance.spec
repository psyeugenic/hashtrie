

{specification, [
	{options, [
		{iterations, 100},
		{inputs, [(1 bsl I) || I <- lists:seq(1,14)] }
	    ]},


	{modules, [
		{hamt, [{operations, [put, get]}
		    ]},
		{htrie, [{operations, [put, get]}
		    ]},
		{gb_trees, [
			{operations, [put, get]},
			{translations, [
				{put, insert},
				{new, empty}
			    ]}
		    ]},
		{rbdict, [
			{operations, [put,get]},
			{translations, [
				{put, store},
				{get, fetch}
			    ]}
	
		    ]},
		{ttdict, [
			{operations, [put,get]},
			{translations, [
				{put, store},
				{get, fetch}
			    ]}
	
		    ]},

		{ttfdict, [
			{operations, [put,get]},
			{translations, [
				{put, store},
				{get, fetch}
			    ]}
	
		    ]},
		{aadict, [
			{operations, [put,get]},
			{translations, [
				{put, store},
				{get, fetch}
			    ]}
	
		    ]},

		{llrbdict, [
			{operations, [put,get]},
			{translations, [
				{put, store},
				{get, fetch}
			    ]}
	
		    ]}
	    ]}
    ]}.

% {hamt_put,        "data/hamt_put.dat"},
% {hamt_get,        "data/hamt_get.dat"},
% {hamt_update,     "data/hamt_update.dat"},
% {htrie_put,       "data/htrie_put.dat"},
% {htrie_get,       "data/htrie_get.dat"},
% {htrie_update,    "data/htrie_update.dat"},
% {gb_trees_put,    "data/gb_trees_insert.dat"},
% {gb_trees_get,    "data/gb_trees_get.dat"},
% {gb_trees_update, "data/gb_trees_update.dat"},
% {dict_put,        "data/dict_store.dat"},
% {dict_get,        "data/dict_fetch.dat"},
% {dict_update,     "data/dict_update.dat"}


%% vim: ft=erlang
