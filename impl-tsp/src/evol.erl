%%
%% impl-tsp – Traveling Salesman Problem in Erlang
%% @author Jan Niklas Böhm <mail@jnboehm.com>
%% @author Jens Nazarenus <me@jens-na.de>
%%

-module(evol).
-import(lists, [nth/2]).
-export([init/0]).
-compile(export_all).


%% @doc Creates an union graph of the two parent elements
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

%% @doc Initializes the Offspring with the random first element of the
%% parent (should they not start at the same node anyways).
edge_recomb(ParentA, ParentB) ->
  AdjA = [{nth(N, ParentA),
           nth((N rem length(ParentA)) + 1, ParentA)}
          || N <- lists:seq(1, length(ParentA)) ],
  AdjB = [{nth(N, ParentB),
           nth((N rem length(ParentB)) + 1, ParentB)}
          || N <- lists:seq(1, length(ParentB)) ],
  %% Create a list with all available vertices in the form of {FromVertex, [Tovertex, ...]}, removing duplicates
  Possibilities = [{FromA, lists:usort([ToA, ToB])} || {FromA, ToA} <- AdjA, {FromB, ToB} <- AdjB, FromA == FromB],
  edge_recomb_1(Possibilities).

edge_recomb_1(Possibilities) ->
  Sort = fun({_, El1}, {_, El2}) ->
             length(El1) =< length(El2)
         end,
  edge_recomb_1([], Possibilities, [], Sort).

edge_recomb_1([], [], Offspring, _) ->
  lists:reverse(Offspring);
edge_recomb_1([], AllPossibilities, Offspring, SortFun) ->
  edge_recomb_1(AllPossibilities, AllPossibilities, Offspring, SortFun);
edge_recomb_1(P, AllPossibilities, Offspring, SortFun) ->
  [NextV | _] = lists:sort(SortFun, P),
  {Vertex, Paths} = NextV,
  %% make a list with all nodes we can reach from the selected node
  Poss = [{From, lists:delete(Vertex, To)}
          || {From, To} <- [{X, Y} || X <- Paths, {X1, Y} <- AllPossibilities -- [NextV], X == X1]],
  %% Update the “global” node list.  We remove the selected node from evey possiblility.
  UpdatedPos = [{From, lists:delete(Vertex, To)}
                || {From, To} <- AllPossibilities -- [NextV]],
  edge_recomb_1(Poss, UpdatedPos, [Vertex | Offspring], SortFun).

mutate_invert(Roundtrip) ->
  Rnd1 = random:uniform(length(Roundtrip)),
  Rnd2 = random:uniform(length(Roundtrip)),
  mutate_invert_1(Roundtrip, Rnd1, Rnd2).

mutate_invert_1(Roundtrip, N1, N2) when N1 > N2 ->
  mutate_invert_1(Roundtrip, N2, N1);
mutate_invert_1(Roundtrip, N1, N2) ->
  {HeadL, _} = lists:split(N1, Roundtrip),      % the indices come mostly from trial and error
  TailL = if N2 < length(Roundtrip) -> lists:nthtail(N2 + 1, Roundtrip);
	     true -> []
	  end,
  InvertedL = lists:reverse(lists:sublist(Roundtrip, N1 + 1, N2 - N1 + 1)),
  HeadL ++ InvertedL ++ TailL.


%% @doc Entry point for get_rnd_roundtrip/3
get_rnd_roundtrip(Vertices, N) ->
  get_rnd_roundtrip(Vertices, N, []).

%% @doc Creates N random roundtrips.
get_rnd_roundtrip(_Vertices, 0, VertexList) ->
  VertexList;

get_rnd_roundtrip(Vertices, N, VertexList) ->
  Trip = [X || {_,X} <- lists:sort([{random:uniform(), Vertex} || Vertex <- Vertices])],
  get_rnd_roundtrip(Vertices, N-1, [Trip|VertexList]).


init(_InitialRoundtrips, FileName) ->
  {_Opts, _Graph} = parse_tsp_file:make_atsp_graph(FileName).
  
  %Roundtrips = get_rnd_roundtrip(digraph:vertices(Graph), InitialRoundtrips),
  %Edgelist = graph_utils:get_edge_list(Graph).

  %random:seed(erlang:now()),          % from http://erlang.org/doc/man/random.html
  %run(Opts, Graph, Roundtrips, Edgelist, fun(_, _) -> true end).

run(Opts, Graph, Roundtrips, Edgelist, CancelFun) ->
  [Score] = orddict:fetch(best, Opts),
  run(Opts, Graph, Roundtrips, Edgelist, Score, CancelFun, fun edge_recomb/2, 0).

run(Opts, Graph, Roundtrips, Edgelist, HiScore,CancelFun, MateFun, Gen) ->
  NextGen = select_best(make_offspring(Roundtrips, MateFun, 50), Edgelist, length(Roundtrips)),
  [Best | _] = Roundtrips,
  case CancelFun(Gen, Best) of                  % recurse as long as cancelfun tells us to
    true -> case graph_utils:get_fitness(Edgelist, Best) > HiScore of
	      true -> run(Opts, Graph, NextGen, Edgelist, HiScore, CancelFun, MateFun, Gen + 1);
	      false -> Best
	    end;
    false -> Best
  end.

select_parents(Roundtrips) ->
  Par1 = nth(random:uniform(length(Roundtrips)), Roundtrips),
  Par2 = nth(random:uniform(length(Roundtrips)), Roundtrips),
  {Par1, Par2}.

gapx(Par1, Par2, EdgeList) ->
  Ug = ug_of(Par1, Par2, EdgeList),
  EdgeListUg = graph_utils:get_edge_list(Ug),

  % Create ghost nodes for all nodes of degree 4.
  lists:map(fun (Vertex) -> graph_utils:create_ghost_node(Ug,
                                                          EdgeListUg,
                                                          Vertex) end, 
            [ V || V <- digraph:vertices(Ug), graph_utils:has_deg_4(Ug, V) =:= true ]),

  % Partitioning
  
  % Return offspring
  [].


make_offspring(Roundtrips, MateFun, Pool) ->
  make_offspring(Roundtrips, [], MateFun, Pool).

make_offspring(Roundtrips, Offspring, _, Pool) when length(Offspring) == Pool ->
  Roundtrips ++ Offspring;
make_offspring(Roundtrips, Offspring, MateFun, Pool) ->
  Par = nth(random:uniform(length(Roundtrips)), Roundtrips),
  New = case random:uniform() < 0.3 of
                  true-> mutate_invert(MateFun(Par, nth(random:uniform(length(Roundtrips)), Roundtrips)));
                  false-> mutate_invert(Par)
        end,
  make_offspring(Roundtrips, [New | Offspring], MateFun, Pool).


select_best(Trips, Edgelist, HowMany) ->
  FitnessFun = fun(El) ->
                  {graph_utils:get_fitness(Edgelist, El), El}
               end,
  {Survivors, _} = lists:split(HowMany, lists:sort(lists:map(FitnessFun, Trips))),
  [S || {_, S} <- Survivors].

%% @doc
init() ->
  init(10, "../data/br17.atsp").
