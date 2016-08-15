%%
%% impl-tsp – Traveling Salesman Problem in Erlang
%% @author Jan Niklas Böhm <mail@jnboehm.com>
%% @author Jens Nazarenus <me@jens-na.de>
%%

-module(evol_gapx).
-export([run/5, run_test/0, init/6]).
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
%%
%% Population - the input population
selection(Population) ->
  {P1,P2} = { select_parent(Population), select_parent(Population) },

  case P1 =:= P2 of
    true ->
      selection(Population);
    false ->
      {P1,P2}
  end.

%% @doc Indefinitely tries to create an offspring.
%%
%% CompleteGraph - the complete graph
%% Population - the population to create an offspring from
%% NSize - the ls3opt neighborhood size
crossover_loop(CompleteGraph, Population, NSize) ->
  {P1, P2} = selection(Population),
  case crossover(CompleteGraph, P1, P2) of
    no_offspring -> crossover_loop(CompleteGraph, Population, NSize);
    Offspring ->
      case digraph_utils:is_acyclic(Offspring) of
        true -> 
                graph_utils:display_graph(Offspring),
                digraph:delete(Offspring),
                %% no_offspring,
                crossover_loop(CompleteGraph, Population, NSize);
        false ->
          {C, _} = hd(get_fitness_pairs([P1, P2, Offspring])),
          case C =:= Offspring of
            true ->
              {Offspring, graph_utils:get_fitness_graph(Offspring)};
            false ->                                % we may have produced a duplicate
              O = mutate_loop(Offspring, CompleteGraph, NSize),
              case verify_graph(Population, {O, graph_utils:get_fitness_graph(O)}) of
                unique -> {O, graph_utils:get_fitness_graph(O)};
                duplicate ->
                  digraph:delete(O),
                  crossover_loop(CompleteGraph, Population, NSize)
              end
          end
      end
  end.

mutate_loop(G, CompleteGraph, NSize) ->
  mutate_loop(G, CompleteGraph, NSize, random:uniform(10)).

mutate_loop(G, _CompleteGraph, _NSize, 0) ->
  G;
mutate_loop(G, CompleteGraph, NSize, N) ->
  mutate(G, CompleteGraph, NSize),
  mutate_loop(G, CompleteGraph, NSize, N-1).

%% Mutates a roundtrip
%% G - the roundtrip to mutate
mutate(G, CompleteGraph, NSize) ->
  N = digraph:no_vertices(G),
  FirstRand = random:uniform(N),
  SecondRand = uniform_except(N, random:uniform(N), FirstRand),
  
  % optmove2 with direction change in subpath
  optmove3:optmove2(G, FirstRand, SecondRand, CompleteGraph),
  
  % optmove3
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
      BL = lists:map(F, CompMapping),

      B = foldl1(fun graph_utils:merge_graphs/2, BL),
      lists:map(fun({_, V1, V2, W}) -> case graph_utils:get_weight(B, V1, V2) of
                                         undef ->
                                           digraph:add_edge(B, V1, V2, W);
                                         W -> ok
                                       end
                end, CommonEdges),

      lists:map(fun(V) -> reverse_ghost_node(B, V) end, [V || V <- digraph:vertices(B), V > 0]),
      lists:foreach(fun(M) -> free_compmapping(M) end, CompMapping),
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
  io:format("ETS: ~p~n", [length(ets:all())]),
  O = crossover_loop(CompleteGraph, Population, NSize),
  case verify_graph(get_fitness_pairs(Offsprings), O) of
    unique ->
      create_offsprings(Population, CompleteGraph, [O|Offsprings], NSize, N-1);
    duplicate ->
      digraph:delete(O),
      create_offsprings(Population, CompleteGraph, Offsprings, NSize, N)
  end.

init(FileName, PopSize, OffspringSize, ProcessesNum, NSize, GenerationMax) ->
  random:seed(erlang:now()),
  optmove3:init_nif(),
  ets_gc:init(),
  {GraphOpts, Graph} = parse_tsp_file:make_atsp_graph(FileName),
  OptList = [{pop_size, PopSize}, {offspring_size, OffspringSize},
             {generations_num, GenerationMax}, {initial_neigh_size, NSize},
             {proc_num, ProcessesNum}],
  Opts = orddict:merge(fun(_,_,_) -> ok end, GraphOpts, orddict:from_list(OptList)),
  msg:master_spawn(Graph, Opts).
  % spawn processes and run

run_test() ->
  init('../data/ftv33.atsp', 30, 10, 1, 10, 150).

run(Graph, Opts) ->
  RndVertexList = get_rnd_vertexlist(digraph:vertices(Graph),
                                     orddict:fetch(generations_num, Opts)),
  InitPop = get_fitness_pairs(pop_init(RndVertexList, Graph,
                                       orddict:fetch(initial_neigh_size, Opts))),
  run_loop(InitPop, Graph,
           hd(orddict:fetch(best, Opts)),
           orddict:fetch(generations_num, Opts),
           orddict:fetch(initial_neigh_size, Opts),
           orddict:fetch(pid, Opts), 0).


run(InitialRoundtrips, Graph, Opts, GenerationMax, NSize) ->
  RndVertexList = get_rnd_vertexlist(digraph:vertices(Graph), InitialRoundtrips),
  InitPop = get_fitness_pairs(pop_init(RndVertexList, Graph, NSize)),
  Pid = spawn(fun(_) -> ok end),
  run_loop(InitPop, Graph, hd(orddict:fetch(best, Opts)), GenerationMax, NSize, Pid, 0).


run_loop(Population, _CompleteGraph, _BestKnown, 0, _NSize, _, _LastMutation) ->
  hd(Population);

run_loop(Population, CompleteGraph, BestKnown, GenerationMax, NSize, Pid, LastMutation) ->
  {G, F} = hd(Population),
  {_, WorstF} = lists:last(Population),
  PopLimit = length(Population),
  receive
    {_Pid, graph, {RecvG, RecvF}} ->
      if RecvF =< WorstF -> NewPop = update_population(Population, [{RecvG, RecvF}], PopLimit),
                            run_loop(NewPop, CompleteGraph, BestKnown, GenerationMax,
                                      NSize, Pid, LastMutation);
         RecvF > WorstF -> dont_use
  end;
    {graph_query, Pid} -> Pid ! {self(), graph, {G, F}};
    stop -> Pid ! stop,
            exit({stopped, {G, F}})
  after 0 ->
      nothing_happened
  end,
  case F =< BestKnown of
    true ->
      {G, F};
    false ->
      Offsprings = create_offsprings(Population, CompleteGraph, [], NSize, 10),
      NextPop = update_population(Population, Offsprings, PopLimit),

      {NextG, NextF} = hd(NextPop),
      NewLm = if NextF < F -> Pid ! {self(), graph, {NextG, NextF}},
                              evol_master ! {self(), graph, {NextG, NextF}},
                              0;                % We did improve
                 NextF >= F -> LastMutation + 1
              end,

      if (NewLm >= 20) -> 
           run_loop(NextPop, CompleteGraph, BestKnown, GenerationMax-1,
                    NSize, Pid, NewLm);
         NewLm < 20 ->
            run_loop(NextPop, CompleteGraph, BestKnown, GenerationMax-1,
                     NSize, Pid, NewLm)
      end
  end.

%% -----------------------------
%% Helper functions for the GA
%% -----------------------------

%% @doc Entry point for get_rnd_vertexlist/3
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

verify_graph(Pop, G = {digraph,_,_,_,true}) ->
  verify_graph(Pop, {G, graph_utils:get_fitness_graph(G)});
verify_graph(Pop, {G1, F1}) ->
  try lists:map(fun(El) -> throw_compare(El, G1, F1) end, Pop) of
      [] -> unique;          % we don't have anythong to compare it to
      [skip|T] when is_list(T) -> unique
  catch
    unique -> unique;
    duplicate -> duplicate
  end.

compare_graph(G1, G2) ->
  L1 = graph_utils:get_edge_list(G1),
  L2 = graph_utils:get_edge_list(G2),
  Len = length([V1a || {_, V1a, V1b, _} <- L1, {_, V2a, V2b, _} <- L2, V1a == V2a, V1b == V2b]),
  if Len == length(L1) -> throw(duplicate);
     Len =/= length(L1) -> unique
  end.

throw_compare({Graph, Fitness}, G1, F1) ->
  if F1 > Fitness ->
      skip;
     F1 < Fitness ->
      throw(unique);
     F1 =:= Fitness ->
      case compare_graph(G1, Graph) of
        unique ->
          skip;
        _ -> throw(duplicate)
      end
  end.

foldl1(Fun, [H|L]) when is_function(Fun, 2), is_list(L) ->
  lists:foldl(Fun, H, L).

free_compmapping({C,CA,CB,_}) ->
  catch digraph:delete(C),
  catch digraph:delete(CA),
  catch digraph:delete(CB),
  ok.

%% @doc Updates the new population, and frees unneeded roundtrips
%%
%% Population - the current population
%% Offsprings - the new offsprings
%% PopLimit - max population
%%
%% Returns the new population
update_population(Population, Offsprings, PopLimit) ->
  {NextPop, Dead} = lists:split(PopLimit,
      lists:keymerge(2, Population, get_fitness_pairs(Offsprings))),
  lists:foreach(fun({DeadG, _}) -> ets_gc ! {del, DeadG} end, Dead),
  NextPop.
