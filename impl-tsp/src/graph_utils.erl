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
%% If no weight can be determined, which means tthat there is no
%% connection between V1 -> V2, the atom no is returned.
get_weight(EdgeList, V1, V2) ->
  F = fun({_E, V1T, V2T, _W}) -> {V1T, V2T} =:= {V1,V2} end,
  case lists:filter(F, EdgeList) of
    [{_,_,_,W}] -> W;
    [] -> undef
  end.

%% @doc Creates an edge list for the given graph.
get_edge_list(Graph) ->
  [digraph:edge(Graph, Edge) || Edge <- digraph:edges(Graph)].

%% @doc Returns the fitness value for a specific roundtrip
get_fitness(EdgeList, Roundtrip) ->
  Weights = [get_weight(EdgeList, 
                        nth(N, Roundtrip), 
                        nth((N rem length(Roundtrip)) + 1, Roundtrip))
              || N <- seq(1, length(Roundtrip))],
  sum(Weights).
