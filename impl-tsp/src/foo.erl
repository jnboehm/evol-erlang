-module(foo).
-compile(export_all).

make_atsp_graph(Filename) ->
  {ok, Filedesc} = file:open(Filename, [read]),
  parse_graph_data(Filedesc).

parse_graph_data(Filedesc) ->
  parse_opts(Filedesc).
  %parse_graph(Filedesc).

parse_opts(Filedesc) ->
  parse_opts(Filedesc, orddict:new()).
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

parse_graph(_Filedesc) ->
  unimplemented.

trim_str(Str) ->
  string:strip(string:strip(Str, both, $\n)).
