%%
%% impl-tsp – Traveling Salesman Problem in Erlang
%% @author Jan Niklas Böhm <mail@jnboehm.com>
%% @author Jens Nazarenus <me@jens-na.de>
%%

-module(optmove3).
-export([]).
-compile(export_all).

%% Performs a 3-opt move iff it improves the roundtrip.
%%
%% For the three nodes, assuming that V1 > V3 > V5 is
%% the order in the roundtrip. Further we assume that for each V in G
%% exists axactly one outgoing neighbor.
%%
%% Before:
%%
%% ... -> [V1] -> [V2] -> [V3] -> [V4] -> [V5] -> [V6] -> ...
%%
%%
%% After:
%%
%%         +-----------------------+
%%         |                       |
%%         |                       V
%% ... -> [V1]    [V2] -> [V3]    [V4] -> [V5]    [V6] -> ...
%%                 ^       |               |       ^
%%                 |       |               |       |
%%                 |       +-----------------------+
%%                 |                       |
%%                 +-----------------------+
%%
%% G - the graph
%% V1 - the first vertex
%% V3 - the second vertex
%% V5 - the third vertex
%%
%% Returns true if the tour has been improved, return false otherwise.
optmove3(G, V1, V3, V5, EdgeList) ->
  V2 = hd(digraph:out_neighbours(G, V1)),
  V4 = hd(digraph:out_neighbours(G, V3)),
  V6 = hd(digraph:out_neighbours(G, V5)),

  % weights before
  W_V1_V2 = graph_utils:get_weight(EdgeList, V1, V2),
  W_V3_V4 = graph_utils:get_weight(EdgeList, V3, V4),
  W_V5_V6 = graph_utils:get_weight(EdgeList, V5, V6),

  % weights after
  W_V1_V4 = graph_utils:get_weight(EdgeList, V1, V4),
  W_V3_V6 = graph_utils:get_weight(EdgeList, V3, V6),
  W_V5_V2 = graph_utils:get_weight(EdgeList, V5, V2),
  
  case (W_V1_V4 + W_V3_V6 + W_V5_V2) < (W_V1_V2 + W_V3_V4 + W_V5_V6) of
    true -> 
      digraph:del_edge(G, hd(digraph:out_edges(G, V1))),
      digraph:del_edge(G, hd(digraph:out_edges(G, V3))),
      digraph:del_edge(G, hd(digraph:out_edges(G, V5))),
      digraph:add_edge(G, V1, V4, W_V1_V4),
      digraph:add_edge(G, V3, V6, W_V3_V6),
      digraph:add_edge(G, V5, V2, W_V5_V2),
      true;
    false ->
      false
  end.

%% @doc Implementation of the 3-opt-move algorithm as described in 2.3 of 
%% "Y. Nagata and D. Soler. A new genetic algorithm for the asymmetric 
%% TSP. Expert Syst. with Applications, 39(10):8947–8953, 2012."
%%
%% G - the graph (roundtrip) to perform the 3-opt-moves
%% EdgeList - the edge list with all the weights
optmove3_run(G, EdgeList, N) ->
  BitList = [ {V, false} || V <- digraph:vertices(G) ],
  optmove3_run(G, EdgeList, BitList, N).

optmove3_run(G, EdgeList, BitList, N) ->
  V1 = hd([ X || {X,Y} <- BitList, Y =:= false]),
  Neighborhood = optmove3_get_subneighborhood(G, [V1], N),
  {V1, V3, V5} = optmove3_next_triple(BitList, Neighborhood),
  case optmove3(G, V1, V3, V5, EdgeList) of
    true ->
      ok;
    false ->
      notok
  end.

%% @doc Returns the next valid 3-tupel of vertices which may be used to
%% perform a 3-opt-move. This function respects the 3-opt-move variant
%% where "don't look bits" are used.
%%))
%% Bitlist - The 2 tuple of {Vertex, true|false}
%% Neighborhood - the neighborhood.
%% V - the vertex to start looking for a triple
optmove3_next_triple(BitList, Neighborhood) ->
  optmove3_next_triple(BitList, Neighborhood, [], 3).

optmove3_next_triple(BitList, Neighborhood, Triple, 0) ->
  list_to_tuple(lists:reverse(Triple));
optmove3_next_triple(BitList, Neighborhood, Triple, N) ->
  Neighbors = tl(Neighborhood),
  case length(BitList) =:= length([ {X,Y} || {X,Y} <- BitList, Y =:= true ]) of
    true -> finished;
    false ->
      NextCand = hd(Neighborhood),
      {Cand, Flag} = hd([ {X,Y} || {X,Y} <- BitList, X =:= NextCand ]),
      case Flag of
        true ->
          optmove3_next_triple(BitList, Neighbors, Triple, N);
        false ->
          optmove3_next_triple(BitList, Neighbors, [Cand|Triple], N-1)
      end
  end.

%% @doc For a given V in G return the subneighborhood as described in
%% "A new genetic algorithm for the asymmetric TSP"
%% -> "V4 is restricted to the vertices that are at most within the
%% *ten* nearest from V4"
%%
%% G - the graph
%% V - the V to to satisfy condition N(V) = V1
optmove3_get_subneighborhood(G, Neighborhood, 1) ->
  lists:reverse(Neighborhood);

optmove3_get_subneighborhood(G, Neighborhood, N) ->
  X = hd(digraph:out_neighbours(G, hd(Neighborhood))),
  optmove3_get_subneighborhood(G, [X|Neighborhood], N-1).
