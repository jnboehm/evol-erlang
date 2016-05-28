%%
%% impl-tsp – Traveling Salesman Problem in Erlang
%% @author Jan Niklas Böhm <mail@jnboehm.com>
%% @author Jens Nazarenus <me@jens-na.de>
%%

-module(parse_tsp_file).
-export([make_atsp_graph/1, set_up_vertices/1]).

%% @doc Returns a pair of options and graph for the specified file.
%% @spec make_atsp_graph(Filename::string()) -> tuple()
make_atsp_graph(Filename) ->
  {ok, Filedesc} = file:open(Filename, [read]),
  Opts = parse_opts(Filedesc),
  Name = orddict:fetch(name, Opts),
  {orddict:append(best, get_best_score(Name), Opts), parse_graph(Filedesc, orddict:fetch(dimension, Opts))}.

%% @doc Entry point for the function parse_opts/2
parse_opts(Filedesc) ->
  parse_opts(Filedesc, orddict:new()).

%% @doc Returns an orddict filled with the options
parse_opts(Filedesc, Options) ->
  {ok, Line} = file:read_line(Filedesc),
  UpdatedOptions = case string:tokens(Line, ":") of
                     ["NAME", Name] -> orddict:append(name, trim_str(Name), Options);
                     ["TYPE", Type] -> orddict:append(type, trim_str(Type), Options);
                     ["COMMENT", Comment] -> orddict:append(comment, trim_str(Comment), Options);
                     ["DIMENSION", Dimension] -> orddict:append(dimension, list_to_integer(trim_str(Dimension)), Options);
                     ["EDGE_WEIGHT_TYPE", Edge_Weight_Type] ->
                       orddict:append(edge_weight_type, trim_str(Edge_Weight_Type), Options);
                     ["EDGE_WEIGHT_FORMAT", Edge_Weight_Format] ->
                       orddict:append(edge_weight_format, trim_str(Edge_Weight_Format), Options);
                     ["EDGE_WEIGHT_SECTION\n"] -> Options;
                     Unknown -> io:format("Unrecognized option ~s", [Unknown]),
                                error(unrecognized_tsp_option)
                   end,
  case string:equal(Line ,"EDGE_WEIGHT_SECTION\n") of
    true-> UpdatedOptions;
    false -> parse_opts(Filedesc, UpdatedOptions)
  end.


%% @doc Entry point for function set_up_vertices/2
set_up_vertices(Dim) when Dim > 0 ->
  set_up_vertices(digraph:new(), Dim).

%% @doc Creates vertices according to the specified dimension.
set_up_vertices(Graph, [Dim]) ->
  set_up_vertices(Graph, Dim);
set_up_vertices(Graph, Dim) when Dim > 0 ->
  digraph:add_vertex(Graph, Dim, Dim),
  set_up_vertices(Graph, Dim - 1);
set_up_vertices(Graph, 0) ->
  Graph.

%% @doc Entry point for function parse_graph/5
parse_graph(Filedesc, [Dim]) when Dim > 0 ->
  Graph = set_up_vertices(Dim),
  parse_graph(Filedesc, Dim, Graph, 1, 1).

%% @doc Reads file number by number and adds edges to the given graph.
parse_graph(_Filedesc, Dim, Graph, Row, _Col) when (Row > Dim) ->
  Graph;
parse_graph(Filedesc, Dim, Graph, Row, Col) when (Row =< Dim) , (Col < Dim) ->
  {ok, [Num]} = io:fread(Filedesc, "", "~d"),
  digraph:add_edge(Graph, Row, Col, Num),
  parse_graph(Filedesc, Dim, Graph, Row, Col + 1);
parse_graph(Filedesc, Dim, Graph, Row, Col) when (Col =:= Dim) , (Row =< Dim) ->
  {ok, [Num]} = io:fread(Filedesc, "", " ~d"),
  digraph:add_edge(Graph, Row, Col, Num),
  parse_graph(Filedesc, Dim, Graph, Row + 1, 1).

get_best_score(Name) ->
  {ok, Filedesc} = file:open("../data/best-known-solutions.txt", [read]),
  get_best_score(lists:flatten(Name), Filedesc).

get_best_score(Name, Filedesc) ->
  {ok, Line} = file:read_line(Filedesc),
  case string:tokens(Line, ":") of
    [Name, Score] -> erlang:list_to_integer(trim_str(Score));
    _ -> get_best_score(Name, Filedesc)
  end.

%% @doc Trims spaces and new line characters.
trim_str(Str) ->
  string:strip(string:strip(Str, both, $\n)).
