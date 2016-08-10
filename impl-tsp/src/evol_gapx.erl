%%
%% impl-tsp – Traveling Salesman Problem in Erlang
%% @author Jan Niklas Böhm <mail@jnboehm.com>
%% @author Jens Nazarenus <me@jens-na.de>
%%

-module(evol_gapx).
-export([run/0, run/2]).
-compile([export_all, {nowarn_deprecated_function, [{random,uniform,0},
                                                    {random,uniform,1},
                                                    {random,seed,   1},
                                                    {erlang,now,    0}]}]).

%% @doc Creates the initial population
%% Returns a list of digraphs
%% RndsVertexList - The random vertex list
%% CompleteGraph - the complete graph
%% NSize - the neighborhood size for ls3opt moves
pop_init(RndsVertexList, CompleteGraph, NSize) ->
  F = fun(L) -> graph_utils:list_to_graph(L, CompleteGraph) end,
  RndGraphs = lists:map(F, RndsVertexList),

  % optimize with ls3opt
  Ls3OptFunc = fun(G) -> optmove3:optmove3_run(G, CompleteGraph, NSize) end,
  lists:map(Ls3OptFunc, RndGraphs),

  % return
  RndGraphs.

%% @doc Selects two parents from the given population
%% Population - the input population
selection(Population) ->
  P = get_fitness_pairs(Population),
  {P1,P2} = { select_parent(P), select_parent(P) },

  case P1 =:= P2 of
    true ->
      selection(Population);
    false ->
      {P1,P2}
  end.

crossover_loop(CompleteGraph, Population) ->
  {P1, P2} = selection(Population),
  case crossover(CompleteGraph, P1, P2) of
    no_offspring -> crossover_loop(CompleteGraph, Population);
    CompMapping ->
      CompMapping
  end.

%%
crossover(CompleteGraph, ParentA, ParentB) ->
  MergedGraph = graph_utils:get_merged_graph_of(ParentA, ParentB),
  GhostNodes = graph_utils:create_ghost_node_run(MergedGraph),
  CommonEdges = graph_utils:get_common_edges(MergedGraph),
  graph_utils:del_common_edges(MergedGraph),
  P = graph_utils:feasible_partition(MergedGraph, CommonEdges, 
                                     ParentA, ParentB, GhostNodes),
  digraph:delete(MergedGraph),                  % don't want to leak ets tables
  case P of
    false ->
      no_offspring;
    CompMapping -> 
      F = fun({C,CA,CB,Simpl}) -> get_path_for_simple_graph(CompleteGraph,C,CA,CB,Simpl) end,
      BestComps = lists:map(F, CompMapping),
      BestCompsList = lists:map(fun(G) -> graph_utils:graph_to_list(G)
                                end, BestComps),
      % a star to create shortest path
      %F = fun(X,Y) -> graph_utils:get_weight(EdgeList, X,Y) end,
      %{Cost, Path} = a_star:run(G1, 1, 11, F)
      {P, BestCompsList}
  end.

%% @doc Selects one parent from the given population
%% Selection mode: tournament selection with probability 0.8 that
%% fittest random chosen roundtrip is selected.
select_parent(FitnessPairs) ->
  N = length(FitnessPairs),
  FirstRand = random:uniform(N),
  SecondRand = uniform_except(N, random:uniform(N), FirstRand),
  X = lists:keysort(2, [lists:nth(FirstRand, FitnessPairs), 
              lists:nth(SecondRand, FitnessPairs)]),

  S = random:uniform(),
  case S < 0.8 of
    true ->
      {G,_} = lists:nth(1, X);
    false ->
      {G,_} = lists:nth(2, X)
  end,
  G.


run(InitialRoundtrips, FileName) ->
  {_Opts, Graph} = parse_tsp_file:make_atsp_graph(FileName),
  RndVertexList = get_rnd_vertexlist(digraph:vertices(Graph), InitialRoundtrips),
  InitPop = pop_init(RndVertexList, Graph, 17),
  random:seed(erlang:now()),
  _X = crossover_loop(Graph, InitPop).

run() ->
  run(100, "../data/br17.atsp").

%% -----------------------------
%% Helper functions for the GA
%% -----------------------------

%% @doc Entry point for get_r)d_vertexlist/3
get_rnd_vertexlist(Vertices, N) ->
  get_rnd_vertexlist(Vertices, N, []).

%% @doc Creates N random roundtrips.
get_rnd_vertexlist(_Vertices, 0, VertexList) ->
  VertexList;
get_rnd_vertexlist(Vertices, N, VertexList) ->
  Trip = [X || {_,X} <- lists:sort([{random:uniform(), Vertex} || Vertex <- Vertices])],
  get_rnd_vertexlist(Vertices, N-1, [Trip|VertexList]).

%% @doc Returns a list of tuples which contains the Graph and its
%% fitness value.
%%
%% Roundtrips - the list of roundtrips as digraph elements
get_fitness_pairs(Roundtrips) ->
  F = fun(R) -> { R, graph_utils:get_fitness_graph(R) } end,
  lists:keysort(2, lists:map(F, Roundtrips)).

uniform_except(_N, Random, Exception) when Random =/= Exception ->
  Random;
uniform_except(N, Random, Exception) when Random =:= Exception ->
  uniform_except(N, random:uniform(N), Exception).

%% @doc Returns the better subpath for the component
get_path_for_simple_graph(_CompleteGraph,_C,CA,CB,_Simpl) ->
  %% AStarFunc = fun(X,Y) -> graph_utils:get_weight(CompleteGraph,X,Y) end,
  FA = graph_utils:get_fitness_graph(CA),
  FB = graph_utils:get_fitness_graph(CB),
      
  if FA < FB -> CA;
     FB =< FA -> CB
  end.
