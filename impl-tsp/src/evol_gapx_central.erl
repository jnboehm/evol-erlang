-module(evol_gapx_central).
-compile(export_all).


init(FileName, Nodes, PopSize, ProcessNum, NSize) ->
  random:seed(erlang:now()),
  optmove3:init_nif(),
  {GraphOpts, Graph} = parse_tsp_file:make_atsp_graph(FileName),
  OptList = [{pop_size, PopSize}, {initial_neigh_size, NSize},
             {proc_num, ProcessNum}],
  Opts = orddict:merge(fun(_,_,_) -> ok end, GraphOpts, orddict:from_list(OptList)),
  PidL = spawn_slaves(ProcessNum),
  lists:foreach(fun(Node) -> net_adm:ping(Node) end, Nodes),
  spawn(?MODULE, master_proc, [Graph, Opts, PidL]).

spawn_slaves(1) ->
  [spawn(?MODULE, slave_handle, [])];
spawn_slaves(N) ->
  Pid = spawn(?MODULE, slave_handle, []),
  [Pid | spawn_slaves(N - 1)].

master_proc(Graph, Opts, Pids) ->
  register(evol_master, self()),
  RndVertexList = evol_gapx:get_rnd_vertexlist(digraph:vertices(Graph),
                                               orddict:fetch(pop_size, Opts)),
  InitPop = evol_gapx:get_fitness_pairs(evol_gapx:pop_init(RndVertexList, Graph,
                                                           orddict:fetch(initial_neigh_size, Opts))),
  F = fun(Pid) -> Pid ! {make_offspring,
                         {Graph, InitPop, orddict:fetch(initial_neigh_size, Opts)}} end,
  lists:foreach(F, Pids),
  master_loop(Graph, Opts, Pids, length(Pids), 1, InitPop, []).

slave_handle() ->
  receive
    {make_offspring, {Graph, Pop, NSize}} ->
      Offspring = evol_gapx:crossover_loop(Graph, Pop, NSize),
      evol_master ! {offspring, Offspring},
      slave_handle();
    stop -> ok
  end.

master_loop(Graph, Opts, Pids, 0, Gen, Pop, Offsprings) ->
  NewPop = lists:keymerge(2, Pop, lists:keysort(2, Offsprings)),
  lists:foreach(fun(Node) -> {evol_master, Node} ! {other_node, hd(NewPop)} end, nodes()),
  master_loop(Graph, Opts, Pids, length(Pids), Gen + 1, NewPop, []);
master_loop(Graph, Opts, Pids, N, Gen, Pop, Offsprings) ->
  receive
    {other_node, O} ->
      io:format("got smthing~n", []),
      case evol_gapx:verify_graph(Offsprings, O) of
        unique -> master_loop(Graph, Opts, Pids, N, Gen, Pop, [O | Offsprings]);
        duplicate -> master_loop(Graph, Opts, Pids, N, Gen, Pop, Offsprings)
      end;
    {offspring, O} ->
      case evol_gapx:verify_graph(Offsprings, O) of
        unique -> master_loop(Graph, Opts, Pids, N - 1, Gen, Pop, [O | Offsprings]);
        duplicate -> master_loop(Graph, Opts, Pids, N - 1, Gen, Pop, Offsprings)
      end;
    {Pid, info} -> Pid ! {ok, {Gen, hd(Pop), lists:last(Pop)}},
                   master_loop(Graph, Opts, Pids, N, Gen, Pop, Offsprings)
  end.

get_info() ->
  evol_master ! {self(), info},
  receive
      {ok, Msg} -> Msg
  end.
