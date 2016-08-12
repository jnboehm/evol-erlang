%%
%% impl-tsp – Traveling Salesman Problem in Erlang
%% @author Jan Niklas Böhm <mail@jnboehm.com>
%% @author Jens Nazarenus <me@jens-na.de>
%%

-module(evol_gapx).
-export([run/5, run_test/0, init/5]).
-compile([export_all, {nowarn_deprecated_function, [{random,uniform,0},
                                                    {random,uniform,1},
                                                    {random,seed,   1},
                                                    {erlang,now,    0}]}]).


%% @doc reverses the ghost node.  It gives all edges emanating from -V
%% to V (there are no incident ones) and afterwards deletes -V which
%% cause all adjacent edges of -V to be deleted as well.
%%
%% Graph - the graph that the ghost node will be deleted from.
%% V - the real node (I think it doesn't have to have a ghost node).
reverse_ghost_node(Graph, V) when V > 0->
  GhostEdges = lists:map(fun(E) -> {_,_,EndV, W} =  digraph:edge(Graph, E), {EndV, W} end,
                         digraph:edges(Graph, -V)),
  lists:map(fun({EndV, W}) -> digraph:add_edge(Graph, V, EndV, W) end, GhostEdges),
  digraph:del_vertex(Graph, -V),
  %% io:format("Got ~p edges from ghost node ~p, real one ~p~n", [GhostEdges, -V, V]),
  Graph;
reverse_ghost_node(G, _) ->
  G.



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
  {P1,P2} = { select_parent(Population), select_parent(Population) },

  case P1 =:= P2 of
    true ->
      selection(Population);
    false ->
      {P1,P2}
  end.

crossover_loop(CompleteGraph, Population, NSize) ->
  _Graphs = [ X || {X,_} <- Population ],
  {P1, P2} = selection(Population),
  case crossover(CompleteGraph, P1, P2) of
    no_offspring -> crossover_loop(CompleteGraph, Population, NSize);
    Offspring ->
      {C, _} = hd(get_fitness_pairs([P1, P2, Offspring])),
      case C =:= Offspring of
        true ->
          Offspring;
        false ->
          mutate(Offspring ,CompleteGraph, NSize)
      end
  end.

%% Mutates a roundtrip
%% G - the roundtrip to mutate
mutate(G, CompleteGraph, NSize) ->
  optmove3:optmove3_run(G, CompleteGraph, NSize),
  G.

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
      [B1, B2] = _BestComps = lists:map(F, CompMapping),

      %% _BestCompsList = lists:map(fun(G) -> graph_utils:graph_to_list(G)
      %%                           end, BestComps),
      %% io:format("~p~n", [_BestCompsList]),
      % a star to create shortest path
      %F = fun(X,Y) -> graph_utils:get_weight(EdgeList, X,Y) end,
      %{Cost, Path} = a_star:run(G1, 1, 11, F)

      B = graph_utils:merge_graphs(B1, B2),
      %% io:format("~p~n", [CommonEdges]),
      lists:map(fun({_, V1, V2, W}) -> case graph_utils:get_weight(B, V1, V2) of
                                         undef ->
                                           digraph:add_edge(B, V1, V2, W);
                                         W -> ok
                                       end
                end, CommonEdges),

      lists:map(fun(V) -> reverse_ghost_node(B, V) end, [V || V <- digraph:vertices(B), V > 0]),
      B
  end.

%% @doc Selects one parent from the given population
%% Selection mode: tournament selection with probability 0.8 that
%% fittest random chosen roundtrip is selected.
select_parent(FitnessPairs) ->
  N = length(FitnessPairs),
  FirstRand = random:uniform(N),
  SecondRand = uniform_except(N, random:uniform(N), FirstRand),
  X = lists:keysort(2, [lists:nth(FirstRand, FitnessPairs), lists:nth(SecondRand, FitnessPairs)]),

  S = random:uniform(),
  case S < 0.8 of
    true ->
      {G,_} = lists:nth(1, X);
    false ->
      {G,_} = lists:nth(2, X)
  end,
  G.


%% Create N offsprings from the given population, using tournament
%% selection to select the parents.
%% Population - the poluation to select from
%% CompleteGraph - the complete graph
%% N - how many offsprings should be produced
create_offsprings(_Population, _CompleteGraph, Offsprings, _, 0) ->
  Offsprings;
create_offsprings(Population, CompleteGraph, Offsprings, NSize, N) ->
  O = crossover_loop(CompleteGraph, Population, NSize),
  create_offsprings(Population, CompleteGraph, [O|Offsprings], NSize, N-1).

init(_InitialRoundtrips, FileName, _ProcessesNum, _NSize, _GenerationMax) ->
  random:seed(erlang:now()),
  {_Opts, _Graph} = parse_tsp_file:make_atsp_graph(FileName),
  % spawn processes and run
  ok.


run_test() ->
  {Opts, Graph} = parse_tsp_file:make_atsp_graph('../data/ftv33.atsp'),
  optmove3:init(),
  run(30, Graph, Opts, 150, 10).

run(InitialRoundtrips, Graph, Opts, GenerationMax, NSize) ->
  RndVertexList = get_rnd_vertexlist(digraph:vertices(Graph), InitialRoundtrips),
  InitPop = get_fitness_pairs(pop_init(RndVertexList, Graph, NSize)),
  run_loop(InitPop, Graph, hd(orddict:fetch(best, Opts)), GenerationMax, NSize, 0).

run_loop(Population, _CompleteGraph, _BestKnown, 0, _NSize, _LastMutation) ->
  io:format("Reached generation limit. Stop.~n"),
  hd(Population);

run_loop(Population, CompleteGraph, BestKnown, GenerationMax, NSize, LastMutation) ->
  {G, F} = hd(Population),
  PopLimit = length(Population),
  {WorstG, WorstF} = lists:last(Population),
  io:format("Gen ~p~n  Best: Fitness ~p, Route: ~p~n Worst: Fitness ~p, Route: ~p~n",
            [GenerationMax, F, graph_utils:roundtrip_to_list(G),
             WorstF, graph_utils:roundtrip_to_list(WorstG)]),
  case F =< BestKnown of
    true ->
      io:format("Reached best known solution. Stop.~n"),
      {G, F};
    false ->
      Offsprings = create_offsprings(Population, CompleteGraph, [], NSize, 10),
      {NextPop, Dead} = lists:split(PopLimit,
                            lists:keymerge(2, Population, get_fitness_pairs(Offsprings))),
      lists:map(fun({DeadG, _}) -> digraph:delete(DeadG) end, Dead),
      {_, NextF} = hd(NextPop),
      NewLm = case F > NextF of
                true -> 0;
                false -> LastMutation + 1
              end,
      run_loop(NextPop, CompleteGraph, BestKnown, GenerationMax-1,
               NSize, NewLm)
  end.

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
