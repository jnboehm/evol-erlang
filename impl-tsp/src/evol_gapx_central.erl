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

%% @doc Spawns N workers and creates a list containing the pid for
%% each of them.
spawn_slaves(1) ->
  [spawn(?MODULE, slave_handle, [])];
spawn_slaves(N) ->
  Pid = spawn(?MODULE, slave_handle, []),
  [Pid | spawn_slaves(N - 1)].

%% @doc this function registers the main process, initializes the
%% population and sends the workers the first call to action.
master_proc(Graph, Opts, Pids) ->
  register(evol_master, self()),
  RndVertexList = evol_gapx:get_rnd_vertexlist(digraph:vertices(Graph),
                                               orddict:fetch(pop_size, Opts)),
  InitPop = evol_gapx:get_fitness_pairs(evol_gapx:pop_init(RndVertexList, Graph,
                                                           orddict:fetch(initial_neigh_size, Opts))),
  F = fun(Pid) -> Pid ! {make_offspring, self(),
                         {Graph, InitPop, orddict:fetch(initial_neigh_size, Opts)}} end,
  lists:foreach(F, Pids),
  master_loop(Graph, Opts, Pids, length(Pids), 1, InitPop, []).

%% @doc the workers.  They create a roundtrip and send it back to the
%% master process.  The tables which the graph is made of have to have
%% their ownership transferred to the master so he can actually free
%% them (else it would error out).
slave_handle() ->
  receive
    {make_offspring, Recipient, {Graph, Pop, NSize}} ->
      {{digraph, V, E, N, _}, _} = Offspring = evol_gapx:crossover_loop(Graph, Pop, NSize),
      lists:foreach(fun(Tid) -> ets:give_away(Tid, Recipient, ok) end, [V, E, N]),
      evol_master ! {offspring, Offspring},
      slave_handle();
    stop -> ok
  end.

master_loop(Graph, Opts, Pids, 0, Gen, Pop, Offsprings) ->
  NewPop = evol_gapx:update_population(Pop, Offsprings, length(Pop)),
  lists:foreach(fun(Node) -> {G, _} = hd(NewPop), L = graph_utils:roundtrip_to_list(G),
                             {evol_master, Node} ! {other_node, L} end, nodes()),
  lists:foreach(fun(Pid) -> Pid ! {make_offspring, self(),
                             {Graph, NewPop, orddict:fetch(initial_neigh_size, Opts)}} end, Pids),
  master_loop(Graph, Opts, Pids, length(Pids), Gen + 1, NewPop, []);
master_loop(Graph, Opts, Pids, N, Gen, Pop, Offsprings) ->
  receive
    {other_node, OList} ->
      NodeGraph = graph_utils:list_to_graph(OList, Graph),
      case evol_gapx:verify_graph(Offsprings, NodeGraph) of
        unique -> O = {NodeGraph, graph_utils:get_fitness_graph(NodeGraph)},
                  master_loop(Graph, Opts, Pids, N, Gen, Pop, [O | Offsprings]);
        duplicate -> digraph:delete(NodeGraph),
                     master_loop(Graph, Opts, Pids, N, Gen, Pop, Offsprings)
      end;
    {offspring, {G, _} = O} ->
      case evol_gapx:verify_graph(Offsprings, O) of
        unique -> master_loop(Graph, Opts, Pids, N - 1, Gen, Pop, [O | Offsprings]);
        duplicate -> digraph:delete(G),
                     master_loop(Graph, Opts, Pids, N - 1, Gen, Pop, Offsprings)
      end;
    {Pid, info} -> Pid ! {ok, {Gen, hd(Pop), lists:last(Pop)}},
                   master_loop(Graph, Opts, Pids, N, Gen, Pop, Offsprings);
    {'ETS-TRANSFER',_,_,ok} ->                  % do nothing
      master_loop(Graph, Opts, Pids, N, Gen, Pop, Offsprings);
    {Pid, stop} -> lists:foreach(fun(P) -> P ! stop end, Pids),
                   Pid ! {ok, {Gen, hd(Pop), lists:last(Pop)}}
  end.

get_info() ->
  evol_master ! {self(), info},
  receive
      {ok, Msg} -> Msg
  end.
