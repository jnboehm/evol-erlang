%%
%% impl-tsp – Traveling Salesman Problem in Erlang
%% @author Jan Niklas Böhm <mail@jnboehm.com>
%% @author Jens Nazarenus <me@jens-na.de>
%%

-module(evol).
-export([init/0]).
-compile(export_all).


%% @doc Let R1 mate with R2. Return the result of that process.
mate_func(R1, R2) ->
  ok.

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

  run(Opts, Graph, Roundtrips, EdgeList, cancel_func).


run(Opts, Graph, Roundtrips, EdgeList, CancelFunc) ->
  graph_utils:get_fitness(EdgeList, lists:nth(1, Roundtrips)).


%% @doc
init() ->
  init(10, "../data/br17.atsp").
