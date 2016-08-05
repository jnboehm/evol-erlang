%%
%% impl-tsp – Traveling Salesman Problem in Erlang
%% @author Jan Niklas Böhm <mail@jnboehm.com>
%% @author Jens Nazarenus <me@jens-na.de>
%%

-module(graph_utils).
-import(lists, [nth/2, seq/2, zip/2, sum/1]).
-import(string, [concat/2]).
-export([]).
-compile(export_all).


%% @doc Returns the weight between two adjacent vertices.
%% If no weight can be determined, which means that there is no
%% connection between V1 -> V2, the atom undef is returned.
get_weight(EdgeList, V1, V2) ->
  F = fun({_E, V1T, V2T, _W}) -> {V1T, V2T} =:= {V1,V2} end,
  case lists:filter(F, EdgeList) of
    [{_,_,_,W}] -> W;
    [] -> undef
  end.

get_weight(EdgeList, E) ->
  F = fun({E1, _, _, _W}) -> E1 =:= E end,
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

%% @doc Returns the fitness value for the individual (roundtrip)
%%
%% EdgeList - the list where the weights can be found
%% G - the graph which represents the roundtrip
get_fitness_graph(EdgeList, G) ->
  Weights = [ get_weight(EdgeList, X) || X <- digraph:edges(G) ],
  sum(Weights).

%% Returns true iff the given vertex has a degree of 4. Returns false
%% otherwise.
%% G - the graph
%% V - the vertex to check
has_deg_4(G, V) ->
  length(get_unique_neighbors(G, V)) =:= 4.


%% Creates a ghost node for the given Node V in the given graph.
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

  [{_,_,VO,WO}, {_,_,V1,W1}] = lists:filter(fun({E, _V1, _V2, _W}) -> lists:member(E, Out) end, EdgeList),
    
  digraph:add_edge(GU, -V, V1, W1), % E3
  digraph:add_edge(GU, -V, VO, WO), % E2

  % delete the original paths to V
  digraph:del_edge(GU, nth(1, Out)),
  digraph:del_edge(GU, nth(2, Out)),

  % create the dummy edge of weight 0 between V and V' (E5)
  digraph:add_edge(GU, V, -V, 0).

%% Creates all the ghost nodes for a merged graph G
%% G - the merged graph of two parents
%% EdgeList - the edge list of G
create_ghost_node_run(G) ->
  EdgeListG = get_edge_list(G),
  lists:map(fun (Vertex) -> graph_utils:create_ghost_node(G, EdgeListG, Vertex) end, 
            [ V || V <- digraph:vertices(G), graph_utils:has_deg_4(G, V) =:= true ]).


%% @doc Identifies common edges in the given merged graph.
%% Returns the edges in a list.
get_common_edges(EdgeList) ->
  Common = [ {E,V1,V2,W} || {E,V1,V2,W} <- EdgeList, 
                (graph_utils:count({V1,V2}, [{V1,V2} || {_,V1,V2,_} <- EdgeList ]) > 1) or 
                (V1 =:= -V2) % Ghost edge between v and v' 
           ],
  Common.

%% @doc Returns a list of poisened vertices, which must not be an entry or
%% exit point in the given component. 
%%
%% A vertex is poisened iff the in- or outgoing edge of the vertex is
%% a) a common edge as returned in get_common_edges/1
%% b) points to a vertex which is also part of the component.
%%
%% Example poisened:
%% We assume: There was a common edge between 15 and -15.
%%
%%  14' -----> 1 <----- -15
%%   |                   |
%%   V                   V
%%  15  <----- 8 -----> 9
%%
%% Result: Since there was a common edge between 15 and -15 and both
%% vertices are inside this component: 15 and -15 are poisened vertices.
get_poisened_vertices(SubGraph, CommonEdgeList) ->
  P = [ {X, Y} || {_,X,Y,_} <- CommonEdgeList, lists:member(X, digraph:vertices(SubGraph)), 
              lists:member(Y, digraph:vertices(SubGraph)) ],
  lists:flatten(tuple_to_list(lists:unzip(P))).


%% @doc Deletes common edges in the given graph.
%% G - the graph
del_common_edges(G) ->
  CommonEdges = get_common_edges(graph_utils:get_edge_list(G)),
  [ digraph:del_edge(G, E) || {E,_,_,_} <- CommonEdges ].


%% @doc Creates an union graph of the two parent elements
%% ParentA - list of a roundtrip
%% ParentB - list of the second roundtrip
%% EdgeList - the edgeList for the weights and edges
ug_of(ParentA, ParentB, EdgeList) ->
  G = parse_tsp_file:set_up_vertices(length(ParentA)),
  ug_of(G, ParentA, [], EdgeList, 1),
  ug_of(G, ParentB, graph_utils:get_edge_list(G), EdgeList, 1).

ug_of(Graph, Parent, _TempEdgeList, _EdgeList, N) when N > length(Parent) ->
  Graph;
ug_of(Graph, Parent, TempEdgeList, EdgeList, N) when N =< length(Parent) ->
  V1 = nth(N, Parent), 
  V2 = nth((N rem length(Parent)) + 1, Parent),
  
  % Add the edge if there isn't an edge describing the
  % connection between V1 and V2 with the same flow direction.
  case graph_utils:get_weight(TempEdgeList, V1, V2) of
    undef -> digraph:add_edge(Graph, V1, V2, 
                            graph_utils:get_weight(EdgeList, V1, V2));
    _ -> ok
  end, 
  ug_of(Graph, Parent, TempEdgeList, EdgeList, N + 1).

%% @doc Creats a union graph of the given graph.
%% Not implemented yet.
%% This function should replace all instances of ug_of in the future.
union_graph(G, EdgeList) ->
  ok.

%% @doc Returns the merged graph of G1 and G2 with G1 and G2 untouched.
%% The vertices will be taken from G1.
%%
%% G1 - the first graph
%% G2 - the second graph
get_merged_graph_of(G1, G2) ->
  GM = parse_tsp_file:set_up_vertices(digraph:no_vertices(G1)),
  EdgeListG1 = get_edge_list(G1),
  EdgeListG2 = get_edge_list(G2),
  [ digraph:add_edge(GM, V1, V2, W) || {_, V1, V2, W} <- EdgeListG1 ],
  [ digraph:add_edge(GM, V1, V2, W) || {_, V1, V2, W} <- EdgeListG2 ],
  GM.  

%% @doc Transforms a given list into a graph
%% G - the graph to transform
%% EdgeList - The edge list where the weights between two vertices A and
%% B can be found
list_to_graph(L, EdgeList) ->
  G = parse_tsp_file:set_up_vertices(length(L)),
  [ digraph:add_edge(G, nth(N, L), nth((N rem length(L)) + 1, L), 
      graph_utils:get_weight(EdgeList, nth(N, L), nth((N rem length(L)) + 1, L)))
      || N <- lists:seq(1, length(L)) ],
  G.

%% Returns a unique list of neighbors for a given vertex in the
%% specified graph.
get_unique_neighbors(G, V) ->
  sets:to_list(sets:from_list(digraph:out_neighbours(G, V) ++ digraph:in_neighbours(G,V))).

%% @doc Transforms a graph into a list.
%% L - the list to transform into a graph.
graph_to_list(G) ->
  ok.

%% @doc Performs a 2-opt-move as described in 
%% http://web.tuke.sk/fei-cit/butka/hop/htsp.pdf
%%
%% We assume, that for each V in G exists exactly one outgoing neighbor.
%%
%% G - the graph
%% V1 - the first vertex
%% V2 - the second vertex
%% EdgeList - the edge list where the weights can be found
%%optmove2(G,V1,V2,EdgeList) ->
%%  V1OutNeighbor = hd(digraph:out_neighbours(G, V1)),
%%  V2OutNeighbor = hd(digraph:out_neighbours(G, V2)),
%%
%%  digraph:add_edge(G, V1, V2, get_weight(EdgeList, V1, V2)),
%%  digraph:add_edge(G, V1OutNeighbor, V2OutNeighbor, get_weight(EdgeList,
%%                                                              V1OutNeighbor,
%%                                                              V2OutNeighbor)),
%%
%% digraph:del_edge(G, hd(digraph:out_edges(G, V1))),
%% digraph:del_edge(G, hd(digraph:out_edges(G, V2))).
%%
%%  % direction swap?

%% @doc Returns the entry points for the specified sub graph, which is a
%% component of a merged graph.
%%
%% SubGraph - the sub graph in which to search for entry points
%% CommonEdges - the common edges of all components
get_entry_points(SubGraph, CommonEdges) ->
  Poison = get_poisened_vertices(SubGraph, CommonEdges),
  [ X || X <- digraph:vertices(SubGraph),
         length(digraph:in_neighbours(SubGraph, X)) =:= 0, 
         not lists:member(X, Poison) ].

%% @doc Equivalent to get_entry_points/1 this function searches for exit
%% points in the given component
%%
%% SubGraph - the sub graph in which to seach for exit points
%% CommonEdges - the common edges of all components
get_exit_points(SubGraph, CommonEdges) ->
  Poison = get_poisened_vertices(SubGraph, CommonEdges),
  [ X || X <- digraph:vertices(SubGraph),
         length(digraph:out_neighbours(SubGraph, X)) =:= 0, 
         not lists:member(X, Poison) ].

%% @doc Displays a graph in a very unconventional way. :-)
%% It uses fdp for graph rendering.
display_graph(G) ->
  EdgeList = graph_utils:get_edge_list(G),
  CommandStr = io_lib:format("~p", [ [{V1, V2} || {_, V1,V2,_} <- EdgeList]]),
  NodeStr = os:cmd(concat_all(["echo \"", io_lib:format("~p", [digraph:vertices(G)]),"\" | sed ''"])),

  SedStr = concat_all(["echo \"", NodeStr, CommandStr,
		       "\" | sed 's/{//g;s/},/\\n/g;s/,/->/g;s/}]//g;s/\\[//g'"]),
  CmdOut = os:cmd(SedStr),
  CmdStr = io_lib:format("echo \" digraph x { ~s }\" | fdp -Tsvg | display", [CmdOut]),
  os:cmd(CmdStr).

concat_all(L) ->
  lists:foldl(fun string:concat/2, "", L).


%% @doc Helper function to count the element X in the list H.
%% X - the element to count
%% H - the list
count(X, H) -> count(X, H, 0).
count(_, [], Count) -> Count;
count(X, [X|Rest], Count) -> count(X, Rest, Count+1);
count(X, [_|Rest], Count) -> count(X, Rest, Count).
