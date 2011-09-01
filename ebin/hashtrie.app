{application, hashtrie, [
	{description, "hashtrie"},
	{vsn, "1.0"},
	{modules, [
		ht,
		tarray,
		hashtrie_test
	    ]
	},
	{registered, []},
        {applications, [
	        kernel,
		stdlib
	    ]
	},
       {env, []}
    ]
}.
