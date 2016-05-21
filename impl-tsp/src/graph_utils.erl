%%
%% impl-tsp – Traveling Salesman Problem in Erlang
%% @author Jan Niklas Böhm <mail@jnboehm.com>
%% @author Jens Nazarenus <me@jens-na.de>
%%

-module(graph_utils).
-import(lists, [nth/2, seq/2, zip/2, sum/1]).
-export([]).
-compile(export_all).


%% @doc Returns the weight between two adjacent vertices.
get_weight(EdgeList, V1, V2) ->
  F = fun({_E, V1T, V2T, _W}) -> {V1T, V2T} =:= {V1,V2} end,
  [{_,_,_,W}] = lists:filter(F, EdgeList),
  W.

%% @doc Returns the fitness value for a specific roundtrip
get_fitness(EdgeList, Roundtrip) ->
  Weights = [get_weight(EdgeList, 
                        nth(N, Roundtrip), 
                        nth((N rem length(Roundtrip)) + 1, Roundtrip))
              || N <- seq(1, length(Roundtrip))],
  sum(Weights).
