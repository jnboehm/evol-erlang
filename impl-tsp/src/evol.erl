%%
%% impl-tsp – Traveling Salesman Problem in Erlang
%% @author Jan Niklas Böhm <mail@jnboehm.com>
%% @author Jens Nazarenus <me@jens-na.de>
%%

-module(evol).
-import(lists, [nth/2]).
-export([init/0]).
-compile(export_all).


%% @doc Let R1 mate with R2. Return the result of that process.
mate_func(_R1, _R2) ->
  ok.


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

%% @doc
cancel_func() ->
  true.


%% @doc Entry point for get_rnd_roundtrip/3
get_rnd_roundtrip(Vertices, N) ->
  get_rnd_roundtrip(Vertices, N, []).

%% @doc Creates N random roundtrips.
get_rnd_roundtrip(_Vertices, 0, VertexList) ->
  VertexList;

get_rnd_roundtrip(Vertices, N, VertexList) ->
  Trip = [X || {_,X} <- lists:sort([{random:uniform(), Vertex} || Vertex <- Vertices])],
  get_rnd_roundtrip(Vertices, N-1, [Trip|VertexList]).


init(InitialRoundtrips, FileName) ->
  {Opts, Graph} = parse_tsp_file:make_atsp_graph(FileName),
  EdgeList = [digraph:edge(Graph, Edge) || Edge <- digraph:edges(Graph)],
  Roundtrips = get_rnd_roundtrip(digraph:vertices(Graph), InitialRoundtrips),
  random:seed(erlang:phash2([node()]),		% from http://erlang.org/doc/man/random.html
	      erlang:monotonic_time(),
	      erlang:unique_integer()),

  run(Opts, Graph, Roundtrips, EdgeList, fun evol:cancel_func/0).


run(_Opts, _Graph, Roundtrips, EdgeList, _CancelFunc) ->
  graph_utils:get_fitness(EdgeList, lists:nth(1, Roundtrips)).


%% @doc
init() ->
  init(10, "../data/br17.atsp").
