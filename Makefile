REBAR=./rebar
DATA=data/norm
EX=example

#GETS= aadict_fetch.dat \
#      dict_fetch.dat \
#      gb_trees_get.dat \
#      hamt_get.dat \
#      htrie_get.dat \
#      llrbdict_fetch.dat \
#      rbdict_fetch.dat \
#      ttdict_fetch.dat \
#      ttfdict_fetch.dat
#
#PUTS= aadict_store.dat \
#      dict_store.dat \
#      gb_trees_insert.dat \
#      hamt_put.dat \
#      htrie_put.dat \
#      llrbdict_store.dat \
#      rbdict_store.dat \
#      ttdict_store.dat \
#      ttfdict_store.dat

GETS=`ls $(DATA)/*_get.dat`
PUTS=`ls $(DATA)/*_put.dat`
MEMS=`ls $(DATA)/*_memory.dat`

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
	mkdir -p $(DATA)
	./hashtrie performance.spec $(DATA)

plots: test_file
	eplot -plot bar2d -norm $(DATA)/ttfdict_fetch_get.dat -margin 40 -x_label "#elements" -y_label "relative" -width 1024 -height 800 -o $(EX)/data_get_norm.png $(GETS)
	eplot -plot bar2d -norm $(DATA)/ttfdict_store_put.dat -margin 40 -x_label "#elements" -y_label "relative" -width 1024 -height 800 -o $(EX)/data_put_norm.png $(PUTS)
	eplot -plot bar2d -norm $(DATA)/ttfdict_memory_memory.dat -margin 40 -x_label "#elements" -y_label "relative" -width 1024 -height 800 -o $(EX)/data_memory_norm.png $(MEMS)

