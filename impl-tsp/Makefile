all: compile

compile:
				erlc -o ebin +debug_info src/*.erl

.PHONY: doc
doc:
	erl -noshell -run edoc_run application "'impl-tsp'" '"."' '[{def,{vsn,""}}]'

