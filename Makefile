REBAR=./rebar

all: compile

compile: $(REBAR)
	@$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean

test:
	erl -pa ebin -noshell -run hashtrie_test test -run erlang halt

test_file:
	erl -pa ebin -noshell -run hashtrie_test test_file -run erlang halt
