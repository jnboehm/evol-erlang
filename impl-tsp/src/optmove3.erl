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

  case V1 =:= V5 of 
    true ->
      io:format("V1 = V5");
    false ->
      ok
  end,

 % io:format("V1,V3,V5: ~p~n", [[V1,V3,V5]]),

  V2 = hd(digraph:out_neighbours(G, V1)),
  V4 = hd(digraph:out_neighbours(G, V3)),
  V6 = hd(digraph:out_neighbours(G, V5)),

%  io:format("V2,V4,V6: ~p~n", [[V2,V4,V6]]),


  % weights before
  W_V1_V2 = graph_utils:get_weight_el(EdgeList, V1, V2),
  W_V3_V4 = graph_utils:get_weight_el(EdgeList, V3, V4),
  W_V5_V6 = graph_utils:get_weight_el(EdgeList, V5, V6),

  % weights after
  W_V1_V4 = graph_utils:get_weight_el(EdgeList, V1, V4),
  W_V3_V6 = graph_utils:get_weight_el(EdgeList, V3, V6),
  W_V5_V2 = graph_utils:get_weight_el(EdgeList, V5, V2),

  %io:format("Before: ~p~n", [(W_V1_V2 + W_V3_V4 + W_V5_V6)]),
  %io:format("After: ~p~n", [(W_V1_V4 + W_V3_V6 + W_V5_V2)]),

  case (W_V1_V4 + W_V3_V6 + W_V5_V2) < (W_V1_V2 + W_V3_V4 + W_V5_V6) of
    true -> 
      %io:format("Improvment with vertices ~p~n", [[V1,V3,V5]]),
      %io:format("Old fitness: ~p~n", [graph_utils:get_fitness_graph(G)]),
      digraph:del_edge(G, hd(digraph:out_edges(G, V1))),
      digraph:del_edge(G, hd(digraph:out_edges(G, V3))),
      digraph:del_edge(G, hd(digraph:out_edges(G, V5))),
      digraph:add_edge(G, V1, V4, W_V1_V4),
      digraph:add_edge(G, V3, V6, W_V3_V6),
      digraph:add_edge(G, V5, V2, W_V5_V2),
      %io:format("New fitness: ~p~n", [graph_utils:get_fitness_graph(G)]),
      true;
    false ->
%      io:format(":-( No improvment with ~p~n", [[V1,V3,V5]]),
      false
  end.


optmove3_yes(G, V1, V3, V5, EdgeList) ->
  V2 = hd(digraph:out_neighbours(G, V1)),
  V4 = hd(digraph:out_neighbours(G, V3)),
  V6 = hd(digraph:out_neighbours(G, V5)),

  % weights after
  W_V1_V4 = graph_utils:get_weight_el(EdgeList, V1, V4),
  W_V3_V6 = graph_utils:get_weight_el(EdgeList, V3, V6),
  W_V5_V2 = graph_utils:get_weight_el(EdgeList, V5, V2),
  
  digraph:del_edge(G, hd(digraph:out_edges(G, V1))),
  digraph:del_edge(G, hd(digraph:out_edges(G, V3))),
  digraph:del_edge(G, hd(digraph:out_edges(G, V5))),
  digraph:add_edge(G, V1, V4, W_V1_V4),
  digraph:add_edge(G, V3, V6, W_V3_V6),
  digraph:add_edge(G, V5, V2, W_V5_V2).


%% @doc Implementation of the 3-opt-move algorithm as described in 2.3 of 
%% "Y. Nagata and D. Soler. A new genetic algorithm for the asymmetric 
%% TSP. Expert Syst. with Applications, 39(10):8947–8953, 2012."
%%
%% G - the graph (roundtrip) to perform the 3-opt-moves
%% CompleteGraph - the edge list with all the weights
%% N - neighborhood size
optmove3_run(G, CompleteGraph, N) ->
  BitList = [ {V, false} || V <- digraph:vertices(G) ],
  EdgeList = graph_utils:get_edge_list(CompleteGraph),
  io:format("ls3opt: before ~p,", [graph_utils:get_fitness_graph(G)]),
  R = optmove3_run(G, EdgeList, BitList, N),
  io:format(" after ~p~n", [graph_utils:get_fitness_graph(G)]),
  R.

optmove3_run(G, EdgeList, BitList, N) ->
  Members = [ X || {X,Y} <- BitList, Y =:= false],
  case length(Members) >= 1 of
    false ->
      finished;
    true ->
      V1 = hd(Members),
%      io:format("V1: ~p~n", [V1]),
      Neighborhood = optmove3_get_subneighborhood(G, [V1], N),
%      io:format("Bitlist: ~p~n", [BitList]),
%      io:format("NH: ~p~n", [Neighborhood]),

      Triples = optmove3_triples(BitList, Neighborhood),
      %io:format("Triples: ~p~n, Neighborhood:~p~n ", [Triples, Neighborhood]),
      case Triples of
        [] -> finished;
        _Triples ->
          R = optmove3_loop(G, Triples, EdgeList),

          case R of
            no_improvment -> % no improvment at all
%              io:format("Nope - "),
              optmove3_run(G, EdgeList,
                           optmove3_update_bitlist(BitList, Neighborhood), N);
            improvment ->
%              io:format("Ye) - "),
              optmove3_run(G, EdgeList, BitList, N)
          end
      end
  end.

optmove3_loop(_G, [], _EdgeList) ->
  no_improvment;
optmove3_loop(G, [H|T], EdgeList) ->
  [V1,V3,V5] = H,
  R = optmove3(G,V1,V3,V5,EdgeList),

  case R of
    true -> 
      improvment;
    false ->
      optmove3_loop(G, T, EdgeList)
  end.
%%
%% @doc Returns the next valid 3-tupel of vertices which may be used to
%% perform a 3-opt-move. This function respects the 3-opt-move variant
%% where "don't look bits" are used.
%%
%% Bitlist - The 2 tuple of [Vertex, true|false]
%% Neighborhood - the neighborhood.
%% V - the vertex to start looking for a triple
optmove3_triples(BitList, Neighborhood) -> 
  [ [V1,V2,V3] || [V1,V2,V3] <- combinations(Neighborhood),
                  %% length([V1,V2,V3]) =:= 3,
         not lists:member({V1, true}, BitList)].

%% @doc Sets the elements in the bitlist to true.
%%
%% BitList - the bitlist
%% Triple - the triple to set true in BitList.
%%
optmove3_update_bitlist(BitList, Elements) ->
  optmove3_update_bitlist(BitList, Elements, 1).

optmove3_update_bitlist(BitList, Elements, N) when N > length(Elements) ->
%  io:format("Strike: ~p~n", [Elements]),
  BitList;

optmove3_update_bitlist(BitList, Elements, N) ->
  X = lists:nth(N, Elements),
  optmove3_update_bitlist(lists:keyreplace(X, 1, BitList, {X, true}),
                          Elements, N+1).


%% @doc For a given V in G return the subneighborhood as described in
%% "A new genetic algorithm for the asymmetric TSP"
%% -> "V4 is restricted to the vertices that are at most within the
%% *ten* nearest from V4"
%%
%% G - the graph
%% V - the V to to satisfy condition N(V) = V1
optmove3_get_subneighborhood(_G, NH, 1)  ->
  lists:reverse(NH);
optmove3_get_subneighborhood(G, NH, NSize) ->
  optmove3_get_subneighborhood(G,
      [hd(digraph:out_neighbours(G, hd(NH)))|NH], NSize-1).  

% 15 -> 4 -> 3

combinations(L) ->
  %% DropFun = fun (L) -> lists:dropwhile(fun(Elem) -> El =/= Elem end, L),
  Res = combinations(L, [], 3),
  tuplify(lists:flatten(Res)).

combinations(_L, Res, 0) ->
  lists:reverse(Res);
combinations(L, Res, N) ->
  lists:map(fun (El) -> [H | T] = lists:dropwhile(fun(Elem) -> El =/= Elem end, L),
                        combinations(T, [H| Res], N - 1) end, L).

tuplify(L) ->
  tup_acc(L, []).

tup_acc([], Res) -> Res;
tup_acc([A,B,C| T], Res) ->
  tup_acc(T, [{A,B,C}|Res]).

init() ->
    ok = erlang:load_nif("./ls3opt_nif", 0).
