-module(ets_gc).
-compile(export_all).

init() ->
  Pid = spawn(ets_gc, handle, [dict:new()]),
  register(ets_gc, Pid),
  {ok, Pid}.

handle(GraphDict) ->
  receive
    {del, Graph} ->
      G = dict:find(Graph, GraphDict),
      case G of
        error -> digraph:delete(Graph),
                 NewD = GraphDict;           % noone uses this graph
        {ok, Val} -> if Val > 1 -> NewD = dict:update_counter(Graph, -1, GraphDict);
                        Val =< 1  -> digraph:delete(Graph),
                                     NewD = dict:erase(Graph, GraphDict)
                     end
      end,
      handle(NewD);
    {use, Graph} ->
      G = dict:find(Graph, GraphDict),
      case G of
        error -> NewD = dict:store(Graph, 1, GraphDict);
        {ok, _} ->  NewD = dict:update_counter(Graph, 1, GraphDict)
      end,
      handle(NewD);
    Unexpected -> io:format("WTF is this hist: ~p?~n", [Unexpected]),
                  error(wrong_msg_format)
  end.
