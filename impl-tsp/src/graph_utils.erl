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

%% Returns true iff the given vertex has a degree of 4. Returns false
%% otherwise.
has_deg_4(G, V) ->
  digraph:in_degree(G, V) + digraph:out_degree(G,V) =:= 4.

%% Creates a ghost node for the given Node V in the given union graph.
%% Note: Ghost nodes can only be created if the degree of the node is
%% exactly 4.
%%
%%  Before:             After:
%%
%%  E1   +---+  E2     E1   +---+  E4
%%  ---->|   |---->    ---->| V |---->
%%  E3   | V |  E4          +---+
%%  ---->|   |---->           | E5
%%       +---+         E3   +---+  E2
%%                     ---->| V'|---->
%%                          +---+
%%
%% The name of the ghost node is V * (-1).
%%
create_ghost_node(GU, EdgeList, V) ->
  digraph:add_vertex(GU, -V, -V),
  Out = digraph:out_edges(GU, V),
  In = digraph:in_edges(GU, V),

  [{_,VI,_,WI}] = lists:filter(fun({E, _V1, _V2, _W}) -> E =:= nth(1, In)
                                end, EdgeList),
  [{_,_,VO,WO}] = lists:filter(fun({E, _V1, _V2, _W}) -> E =:= nth(2, Out)
                                end, EdgeList),
    
  digraph:add_edge(GU, VI, -V, WI), % E3
  digraph:add_edge(GU, -V, VO, WO), % E2

  % delete the original paths to V
  digraph:del_edge(GU, nth(1, In)),
  digraph:del_edge(GU, nth(2, Out)),

  % create the dummy edge of weight 0 between V and V' (E5)
  digraph:add_edge(GU, V, -V, 0).
  
