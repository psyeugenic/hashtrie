REBAR=./rebar
DATA=data
EX=../example

GETS= aadict_fetch.dat \
      dict_fetch.dat \
      gb_trees_get.dat \
      hamt_get.dat \
      htrie_get.dat \
      llrbdict_fetch.dat \
      rbdict_fetch.dat \
      ttdict_fetch.dat \
      ttfdict_fetch.dat

PUTS= aadict_store.dat \
      dict_store.dat \
      gb_trees_insert.dat \
      hamt_put.dat \
      htrie_put.dat \
      llrbdict_store.dat \
      rbdict_store.dat \
      ttdict_store.dat \
      ttfdict_store.dat

all: escript

escript: compile
	@$(REBAR) escriptize

compile: $(REBAR)
	@$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean

test: escript performance.spec
	./hashtrie performance.spec

test_file: escript performance.spec
	./hashtrie performance.spec data

plots: test_file
	(cd $(DATA) && eplot -plot plot2d -margin 40 -x_label "#elements" -y_label "microseconds" -width 800 -height 600 -o $(EX)/data_get.png $(GETS) )
	(cd $(DATA) && eplot -plot plot2d -margin 40 -x_label "#elements" -y_label "microseconds" -width 800 -height 600 -o $(EX)/data_put.png $(PUTS) )
	#(cd $(DATA) && eplot -margin 40 -x_label "#elements" -y_label "microseconds" -width 800 -height 600 -o $(EX)/data_update.png dict_update.dat gb_trees_update.dat htrie_update.dat)

