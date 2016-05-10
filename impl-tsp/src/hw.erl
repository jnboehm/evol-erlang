-module(hw).
-export([read_tsp_file/1, get_lines/1, get_lines/2, parse_line/3,
         parse_matrix/2]).


read_tsp_file(FileName) ->
  {ok, IODevice} = file:open(FileName, [read]),
  try get_lines(IODevice)
    after file:close(IODevice)
  end.

%% Return 2-Tuple { options, matrix }
%% options = [{ key, value }, ...]
%% matrix = NxN
get_lines(IODevice) ->
  get_lines(IODevice, {orddict:new(), digraph:new()}).


%% Return 2-Tuple { options, matrix }
%% options = [{ key, value }, ...]
%% matrix = NxN
get_lines(IODevice, Result) ->
  case io:get_line(IODevice) of
    eof -> Result;
    Data -> {Options, Graph} = Result,
            case parse_line(string:str(Data, "EDGE_WEIGHT_SECTION"), Data,
                            IODevice) of
              {Key, Value} -> Options = orddict:append(Key, Value, Options);
              ok -> ok
            end
  end.

%% Return 2-Tuple
parse_line(0, Data, IODevice) ->
  io:format("bnla1%"),
  ok.
%% Return Matrix
parse_line(N, Data, IODevice, Matrix) ->
  ok.

%% Return Matrix
parse_matrix(IODevice, Matrix) -> 
  ok.






















  %case io:get_line(IODevice, "") of
  %  eof -> [];
  %  Data -> get_lines('option',IODevice)
  %end.


% check_list(string:str(Data, ""), Data, get_lines(IODevice))

%%append_options_tuple(0, Head, Tail) ->
%  Tail;
%append_matrix(1, Head, Tail) when N > 0 ->
%  [Head|Tail].
