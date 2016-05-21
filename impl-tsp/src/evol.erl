%%
%% impl-tsp – Traveling Salesman Problem in Erlang
%% @author Jan Niklas Böhm <mail@jnboehm.com>
%% @author Jens Nazarenus <me@jens-na.de>
%%

-module(evol).
-export([init/0]).
-compile(export_all).

init() -> 
  {Opts, Graph} = parse_tsp_file:make_atsp_graph("../data/br17.atsp"),
  T = [digraph:edge(Graph, Edge) || Edge <- digraph:edges(Graph)].

