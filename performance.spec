
{specification, [
	{options, [
		{iterations, 200},
		{operations, [put, get, memory]},
		{inputs, [(1 bsl I) || I <- lists:seq(3,14)] }
	    ]},


	{modules, [
		{hamt, []},
		{htrie, []},
		{dict, [
			{translations, [
				{put, store},
				{get, fetch}
			    ]}
		    ]},
		{gb_trees, [
			{translations, [
				{put, insert},
				{new, empty}
			    ]}
		    ]},
		{rbdict, [
			{translations, [
				{put, store},
				{get, fetch}
			    ]}
		    ]},
		{ttdict, [
			{translations, [
				{put, store},
				{get, fetch}
			    ]}
	
		    ]},

		{ttfdict, [
			{translations, [
				{put, store},
				{get, fetch}
			    ]}
	
		    ]},
		{aadict, [
			{translations, [
				{put, store},
				{get, fetch}
			    ]}
	
		    ]},

		{llrbdict, [
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
