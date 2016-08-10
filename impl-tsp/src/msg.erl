-module(msg).
-compile(export_all).

master_spawn() ->
  master_spawn("../data/br17.atsp", 10).

%% How many calls to spawn/3?  where can I look up cores or something
%% like that?  Would benchmarking it work?

%% Spawns all the processes and then returns the first and last pid of
%% the created ones.  The processes each have the pid of the one they
%% spawn and the last one gets the pid of the first process.
master_spawn(Filename, ProcNum) when ProcNum > 0 andalso is_integer(ProcNum) ->
  {Opts, Graph} = evol:init(10, Filename),
  Firstpid = spawn(?MODULE, slave_spawn, [self(), Filename, Opts, Graph, ProcNum]),
  receive
    {procs_spawned, Pid} -> Pid ! {firstpid, Firstpid},	% complete the circle
			    {Firstpid, Pid}
  end.

slave_spawn(MasterPid, _Filename, _Opts, _Graph, 0) ->
  MasterPid ! {procs_spawned, self()},
  receive
    {firstpid, Pid} -> ok
  end,
  %% Pid needs to be passed to the evol module in order to send the
  %% roundtrips around.
  get_msg(Pid, _Graph);
slave_spawn(MasterPid, Filename, Opts, Graph, ProcNum) ->
  Pid = spawn(?MODULE, slave_spawn, [MasterPid, Filename, Opts, Graph, ProcNum - 1]),
  %% We actually need to call the runner function, which still needs to be written.
  %% evol:init(10, Filename)
  get_msg(Pid, Graph).

get_msg(Pid, PNum) ->
  Pid ! {self(), digraph:no_edges(PNum)},
  receive
    {From, Msg} ->
      io:format("~p got a msg from ~p: ~p~n", [self(), From, Msg])
  after 10000 ->
      io:format("~p did not receive a message in 10 secs~n", [self()])
  end.

send_trip(Pid, Roundtrip) ->
  Pid ! {self(), Roundtrip}.
