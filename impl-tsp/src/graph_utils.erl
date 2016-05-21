%%
%% impl-tsp – Traveling Salesman Problem in Erlang
%% @author Jan Niklas Böhm <mail@jnboehm.com>
%% @author Jens Nazarenus <me@jens-na.de>
%%

-module(graph_utils).
-export([]).
-compile(export_all).


%% @doc Returns the weight between two adjacent vertices.
get_weight(EdgeList, V1, V2) ->
  F = fun({_E,V1T,V2T,_W}) -> {V1T, V2T} =:= {V1,V2} end,
  [{_,_,_,W}] = lists:filter(F, EdgeList),
  W.
