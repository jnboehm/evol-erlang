-module(ets_gc).
-compile(export_all).

init() ->
  Pid = spawn(ets_gc, handle, [dict:new()]),
  register(ets_gc, Pid),
  {ok, Pid}.

handle(GraphDict) ->
  receive
    {del, Graph} ->
      %% lists:filter(fun(G) -> G =:= Graph end, GraphList),
      G = dict:find(Graph, GraphDict),
      case G of
        error -> digraph:delete(Graph);           % noone uses this graph
        {ok, Val} -> if Val > 1 -> handle(dict:update_counter(Graph, -1, GraphDict));
                        Val =< 1  -> digraph:delete(Graph)
                     end
      end;
    {use, Graph} ->       G = dict:find(Graph, GraphDict),
      case G of
        error -> handle(dict:store(Graph, 1, GraphDict));
        {ok, _} -> handle(dict:update_counter(Graph, 1, GraphDict))
      end;
    Unexpected -> io:format("WTF is this hist: ~p?~n", [Unexpected]),
                  error(wrong_msg_format)
  end.
