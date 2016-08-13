all: compile

compile:
				erlc -Wall -o ebin +debug_info src/*.erl 

.PHONY: doc
doc:
	erl -noshell -run edoc_run application "'impl-tsp'" '"."' '[{def,{vsn,""}}]'

nif:
	gcc -o ebin/ls3opt_nif.so -I /usr/lib/erlang/erts-*/include -fpic -shared src/ls3opt.c
	gcc -o ebin/graph_utils_nif.so -I /usr/lib/erlang/erts-*/include -fpic -shared src/graph_utils_nif.c
