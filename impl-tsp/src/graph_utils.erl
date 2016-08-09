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

%% copied from digraph.erl
-record(digraph, {vtab = notable :: ets:tab(),
		  etab = notable :: ets:tab(),
		  ntab = notable :: ets:tab(),
	          cyclic = true  :: boolean()}).

%% @doc Returns the weight between two adjacent vertices.
%% If no weight can be determined, which means that there is no
%% connection between V1 -> V2, the atom undef is returned.
get_weight(G, V1, V2) ->
  WL = ets:select(G#digraph.etab, [{{'_',V1,V2,'$1'},[],['$1']}]),
  case WL of
    [] -> undef;
    WL -> hd(WL)
  end.

get_weight(G, E) ->
  WL = ets:select(G#digraph.etab, [{{E,'_','_','$1'},[],['$1']}]),
  case WL of
    [] -> undef;
    WL -> hd(WL)
  end.

%% @doc Creates an edge list for the given graph.
get_edge_list(Graph) ->
  ets:select(Graph#digraph.etab, [{{'$1','$2','$3','$4'},[],[{{'$1','$2','$3','$4'}}]}]).

%% @doc Returns the fitness value for a specific roundtrip
%% DEPRECATED!!!
get_fitness(EdgeList, Roundtrip) ->
  Weights = [get_weight(EdgeList, 
                        nth(N, Roundtrip), 
                        nth((N rem length(Roundtrip)) + 1, Roundtrip))
              || N <- seq(1, length(Roundtrip))],
  sum(Weights).

%% @doc Returns the fitness value for the individual (roundtrip)
%%
%% EdgeList - the list where the weights can be found (isn't needed any more)
%% G - the graph which represents the roundtrip
%% TODO make an ets_select/2 funcall and remove the param EdgeList
get_fitness_graph(G) ->
  Weights = [ get_weight(G, X) || X <- digraph:edges(G) ],
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
create_ghost_node(GU, _EdgeList, V) ->
  digraph:add_vertex(GU, -V, -V),
  Out = digraph:out_edges(GU, V),

  [{_,_,VO,WO}, {_,_,V1,W1}] = lists:filter(fun({E, _V1, _V2, _W}) -> lists:member(E, Out) end, EdgeList),
    
  digraph:add_edge(GU, -V, V1, W1), % E3
  digraph:add_edge(GU, -V, VO, WO), % E2

  % delete the original paths from V to the ones now connected through V'
  digraph:del_edge(GU, nth(1, Out)),
  digraph:del_edge(GU, nth(2, Out)),

  % create the dummy edge of weight 0 between V and V' (E5)
  digraph:add_edge(GU, V, -V, 0),
  -V.

%% Creates all the ghost nodes for a merged graph G
%% G - the merged graph of two parents
create_ghost_node_run(G) ->
  EdgeListG = get_edge_list(G),
  lists:map(fun (Vertex) -> graph_utils:create_ghost_node(G, EdgeListG, Vertex) end, 
            [ V || V <- digraph:vertices(G), graph_utils:has_deg_4(G, V) =:= true ]).


%% @doc Identifies common edges in the given merged graph.
%% Returns the edges in a list.
get_common_edges(G) ->
  EdgeList = get_edge_list(G),
  Common = [ {E,V1,V2,W} || {E,V1,V2,W} <- EdgeList, 
                (graph_utils:count({V1,V2}, [{X1,X2} || {_,X1,X2,_} <- EdgeList ]) > 1) or 
                (V1 =:= -V2) % Ghost edge between v and v' 
           ],
  Common.

%% @doc Returns a list of poisoned vertices, which must not be an entry or
%% exit point in the given component. 
%%
%% A vertex is poisoned iff the in- or outgoing edge of the vertex is
%% a) a common edge as returned in get_common_edges/1
%% b) points to a vertex which is also part of the component.
%%
%% Example poisoned:
%% We assume: There was a common edge between 15 and -15.
%%
%%  14' -----> 1 <----- -15
%%   |                   |
%%   V                   V
%%  15  <----- 8 -----> 9
%%
%% Result: Since there was a common edge between 15 and -15 and both
%% vertices are inside this component: 15 and -15 are poisoned vertices.
get_poisoned_vertices(SubGraph, CommonEdgeList) ->
  P = [ {X, Y} || {_,X,Y,_} <- CommonEdgeList, lists:member(X, digraph:vertices(SubGraph)), 
              lists:member(Y, digraph:vertices(SubGraph)) ],
  lists:flatten(tuple_to_list(lists:unzip(P))).


%% @doc Deletes common edges in the given graph.
%% G - the graph
del_common_edges(G) ->
  CommonEdges = get_common_edges(G),
  [ digraph:del_edge(G, E) || {E,_,_,_} <- CommonEdges ].


%% @doc Creates an union graph of the two parent elements
%% ParentA - list of a roundtrip
%% ParentB - list of the second roundtr)p
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

%% @doc Creates a union graph of the given base graphs
%% G1 - the first graph
%% G2 - the second graph
%%
%% Note: We assume, that the vertices of G1 and G2 are equal, since this
%% will always be the case, when a union graph between two parents are
%% created.
union_graph_of(G1, G2) ->
  GU = parse_tsp_file:set_up_vertices(digraph:no_vertices(G1)),
  InsertFunc = fun({V1,V2,W}) -> digraph:add_edge(GU,V1,V2,W) end,

  InsFromG1 = [ {V1,V2,W} || {_,V1,V2,W} <- get_edge_list(G1) ],
  lists:map(InsertFunc, InsFromG1),

  InsFromG2 = [ {V1,V2,W} || {_,V1,V2,W} <- get_edge_list(G2),
                         get_weight(GU, V1,V2) =:= undef ],
  lists:map(InsertFunc, InsFromG2),
  GU.

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
%% CompleteGraph - The complete graph created by parse_tsp_file
list_to_graph(L, CompleteGraph) ->
  G = parse_tsp_file:set_up_vertices(length(L)),
  [ digraph:add_edge(G, nth(N, L), nth((N rem length(L)) + 1, L), 
      graph_utils:get_weight(CompleteGraph, nth(N, L), nth((N rem length(L)) + 1, L)))
      || N <- lists:seq(1, length(L)) ],
  G.

%% Returns a unique list of neighbors for a given vertex in the
%% specified graph.
get_unique_neighbors(G, V) ->
  sets:to_list(sets:from_list(digraph:out_neighbours(G, V) ++ digraph:in_neighbours(G,V))).

graph_to_list(G) ->
  Comps = digraph_utils:components(G),
  EdgeList = get_edge_list(G),
  In0 = lists:filter(fun(V) -> digraph:in_degree(G, V) =:= 0 end, lists:flatten(Comps)),
  In0Map = [ {I, C} || I <- In0, C <- Comps, lists:member(I, C) ],

  case length(In0) =:= 0 of
    true ->
      undef; % we always have components
    false ->
      lists:map(fun({I,C}) -> graph_to_list(G, EdgeList, C, [I]) end, In0Map)
  end.

graph_to_list(G, EdgeList, Comp, []) ->
  []; 
graph_to_list(G, EdgeList, Comp, I) ->
  V2 = [ V2 || {_,V1,V2,_} <- EdgeList, V1 =:= hd(I)],
  [hd(I) | graph_to_list(G, EdgeList, Comp, V2)].


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
  Poison = get_poisoned_vertices(SubGraph, CommonEdges),
  [ X || X <- digraph:vertices(SubGraph),
         length(digraph:in_neighbours(SubGraph, X)) =:= 0, 
         not lists:member(X, Poison) ].

%% @doc Equivalent to get_entry_points/1 this function searches for exit
%% points in the given component
%%
%% SubGraph - the sub graph in which to seach for exit points
%% CommonEdges - the common edges of all components
get_exit_points(SubGraph, CommonEdges) ->
  Poison = get_poisoned_vertices(SubGraph, CommonEdges),
  [ X || X <- digraph:vertices(SubGraph),
         length(digraph:out_neighbours(SubGraph, X)) =:= 0, 
         not lists:member(X, Poison) ].

%% @doc Checks if the merged graph with its common edges removed is a
%% feasible partition for GAPX.
%%
%% GM - the merged graph with its common edges removed
%% CommonEdges - the list of common edges which are already removed.
%% ParentA - the graph of Parent A
%% ParentB - the graph of Parent B
%% GhostNodes - the ghostnodes which were used to remove common edges
%%              and partition the graph GM
feasible_partition(GM, CommonEdges, ParentA, ParentB, GhostNodes) ->
  Components = digraph_utils:components(GM),
  %io:format("Components: ~p~n", [Components]),
  case length(Components) > 1 of
    false ->
      %io:format("Not feasible. Components == 1~n"),
      false;
    true ->
      F = fun(Component) -> check_component(GM, Component, CommonEdges,
                                        ParentA, ParentB, GhostNodes) end,
      CompList = lists:map(F, Components),
      %io:format("CompList: ~p~n", [CompList]),
      R = lists:foldl(fun(A,B) -> A and B end, true, [ X || {X,_,_,_,_} <- CompList]),
      io:format("Feasible partition: ~p~n", [R]),
      case R of
        true ->
          [ {C,CA,CB,SimplGraphs} || {_,C,CA,CB,SimplGraphs} <- CompList ];
        false ->
          false
      end
    end.

%% @doc Checks a specific component of GM
%%
%% GM - the merged graph with its common edges removed
%% Component - the list of vertices which represents a component of the
%%             partition
%% CommonEdges - the list of common edges which are already removed
%% ParentA - the graph of Parent A
%% ParentB - the graph of Parent B
%% GhostNodes - the ghostnodes which were used to remove common edges
%%              and partition the graph GM
check_component(GM, Component, CommonEdges, ParentA, ParentB, GhostNodes) ->
  CompGraph = digraph_utils:subgraph(GM, Component),

  EntryPoints = get_entry_points(CompGraph, CommonEdges),
  ExitPoints = get_exit_points(CompGraph, CommonEdges),

  CompParA = subgraph_comp(ParentA, GhostNodes, EntryPoints, 
                             GM, Component),
  CompParB = subgraph_comp(ParentB, GhostNodes, EntryPoints, 
                             GM, Component),

  %graph_utils:display_graph(CompParA),
  %graph_utils:display_graph(CompParB),

  %io:format("EdgeList A: ~p~n", [get_edge_list(CompParA)]),
  %io:format("EdgeList B: ~p~n", [get_edge_list(CompParB)]),
  %io:format("Entry Points: ~p~n", [EntryPoints]),
  %io:format("Exit Points: ~p~n", [ExitPoints]),
  
  case length(EntryPoints) =:= 1 of
    true -> % Only one entry and one exit 
      PathA = digraph:get_path(CompParA, hd(EntryPoints),
                               hd(ExitPoints)),
      %io:format("Path A: ~p~n", [PathA]),

      PathB = digraph:get_path(CompParB, hd(EntryPoints),
                               hd(ExitPoints)),
      %io:format("Path B: ~p~n", [PathB]),
      
      case (PathA =:= false) or (PathB =:= false) of
        true -> 
	  lists:map(fun digraph:delete/1, [CompGraph, CompParA, CompParB]),
          {false, [],[],[],[]};
        false ->
          Simpl = {hd(PathA), lists:last(PathA)},
          R = (hd(PathA) =:= hd(PathB)) and (lists:last(PathA) =:= lists:last(PathB)),
          io:format("XX PathA: ~p, PathB: ~p, Result:~p~n", [hd(PathA),
                                                          lists:last(PathA),R]),
          {R, CompGraph, CompParA, CompParB, [Simpl]} % <- return
      end;
    false -> % N entry points and N exit points
      Combs = [{X,Y} || X <- EntryPoints, Y <- ExitPoints],
      PathsA = lists:map(fun({E,X}) -> digraph:get_path(CompParA, E,X) end, Combs),
      %io:format("Paths A: ~p~n", [PathsA]),
      PathsB = lists:map(fun({E,X}) -> digraph:get_path(CompParB, E,X) end, Combs),
      %io:format("Paths B: ~p~n", [PathsA]),

      A1 = lists:filter(fun(E) -> E =/= false end, PathsA),
      B1 = lists:filter(fun(E) -> E =/= false end, PathsB),

      case length(A1) + length(B1) =:= 0 of
        true ->
	  lists:map(fun digraph:delete/1, [CompGraph, CompParA, CompParB]),
          {false,[],[],[],[]};
        false -> 
          A1Simpl = lists:map(fun(E) -> {hd(E),lists:last(E)} end, A1),
          B1Simpl = lists:map(fun(E) -> {hd(E),lists:last(E)} end, B1),
          R = (A1Simpl =:= B1Simpl),
          io:format("ZZ PathsA: ~p, PathB: ~p, Result:~p~n", [A1Simpl,
                                                          B1Simpl,R]),
          {R, CompGraph, CompParA, CompParB, A1Simpl } % <- return
        end
  end.

%% @doc Creates a subgraph for the given base graph. This function
%% respects inserted ghost nodes which may be entry vertices.
%%
%% G - the base graph
%% GhostNodes - the ghost nodes which were inserted
%% EntryVertices - the entry vertices of the components
%%              nodes and the edges of the ghost nodes can be found 
%%              (after removing the common edges
%% Vertices - the vertices to insert in the subgraph
subgraph_comp(G, GhostNodes, EntryVertices, GM, Vertices) ->
  Comp = digraph_utils:subgraph(G, Vertices),
  EdgeListGM = get_edge_list(GM),
  GhostInCand = [ {E,V1,V2,W} || {E,V1,V2,W} <- EdgeListGM, 
                             lists:member(V1, EntryVertices) and 
                             lists:member(V1, GhostNodes) and 
                             lists:member(V2, Vertices)
            ],
  
  %% Here we somehow reverse the ghost node in order to get a complete subgraph
  %% of the given base graph with the ghost node inserted.
  GhostIn = [ {V1,V2,W} || {_,V1,V2,W} <- GhostInCand, lists:member({V1,V2}, 
    [ {X1 * (-1), X2} || {_,X1,X2,_} <- get_edge_list(G)]) ],
  
  %% Note: Only realised for ghost nodes which flow is "incoming",
  %% ghostnodes with outgoing flow direction is probably not possible.
  GhostToAdd = sets:to_list(sets:from_list([ V || {V,_,_} <- GhostIn ])),

  lists:map(fun (Vertex) -> digraph:add_vertex(Comp, Vertex) end, GhostToAdd),
  lists:map(fun ({V1,V2,W}) -> digraph:add_edge(Comp,V1,V2,W) end, GhostIn),
  Comp.

%% @doc Displays a graph in a very unconventional way. :-)
%% It uses fdp for graph rendering.
display_graph(G) ->
  EdgeList = graph_utils:get_edge_list(G),
  CommandStr = io_lib:format("~p", [ [{V1, V2} || {_, V1,V2,_} <- EdgeList]]),
  NodeStr = os:cmd(concat_all(["echo \"", io_lib:format("~p", [digraph:vertices(G)]),
                               "\" | sed 's/\\[//g;s/,/\\n/g;s/\\]/\\n/g'"])),
  SedStr = concat_all(["echo \"", CommandStr,
                       "\" | sed 's/{//g;s/},/\\n/g;s/,/->/g;s/}]//g;s/\\[//g'"]),
  CmdOut = os:cmd(SedStr),
  CmdStr = io_lib:format("echo \" digraph x { ~s~n~s }\" | fdp -Tsvg | display", [NodeStr, CmdOut]),
  os:cmd(CmdStr).

concat_all(L) ->
  lists:foldr(fun string:concat/2, "", L).


%% @doc Helper function to count the element X in the list H.
%% X - the element to count
%% H - the list
count(X, H) -> count(X, H, 0).
count(_, [], Count) -> Count;
count(X, [X|Rest], Count) -> count(X, Rest, Count+1);
count(X, [_|Rest], Count) -> count(X, Rest, Count).
