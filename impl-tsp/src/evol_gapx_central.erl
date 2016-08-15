-module(evol_gapx_central).
-compile(export_all).


init(FileName, PopSize, ProcessNum, NSize) ->
  random:seed(erlang:now()),
  optmove3:init_nif(),
  {GraphOpts, Graph} = parse_tsp_file:make_atsp_graph(FileName),
  OptList = [{pop_size, PopSize}, {initial_neigh_size, NSize},
             {proc_num, ProcessNum}],
  Opts = orddict:merge(fun(_,_,_) -> ok end, GraphOpts, orddict:from_list(OptList)),
  PidL = spawn_slaves(ProcessNum),
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
  master_loop(Graph, Opts, Pids, length(Pids), Gen + 1, NewPop, []);
master_loop(Graph, Opts, Pids, N, Gen, Pop, Offsprings) ->
  receive
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
