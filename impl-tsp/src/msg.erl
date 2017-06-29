-module(msg).
-compile(export_all).

%% Spawns all the processes and then returns the first and last pid of
%% the created ones.  The processes each have the pid of the one they
%% spawn and the last one gets the pid of the first process.
master_spawn(Graph, Opts) ->
  Firstpid = spawn(?MODULE, slave_spawn, [self(), Opts, Graph, orddict:fetch(proc_num, Opts)]),
  register(evol_master, self()),
  receive
    {procs_spawned, Pid} -> Pid ! {firstpid, Firstpid}	% complete the circle
  end,
  spawn(?MODULE, handle, []).

slave_spawn(MasterPid, Opts, Graph, 0) ->
  MasterPid ! {procs_spawned, self()},
  receive
    {firstpid, Pid} -> ok
  end,
  %% Pid needs to be passed to the evol module in order to send the
  %% roundtrips around.
  evol_gapx:run(Graph, orddict:store(pid, Pid, Opts));
slave_spawn(MasterPid, Opts, Graph, ProcNum) ->
  Pid = spawn(?MODULE, slave_spawn, [MasterPid, Opts, Graph, ProcNum - 1]),
  %% We actually need to call the runner function, which still needs to be written.
  evol_gapx:run(Graph, orddict:store(pid, Pid, Opts)).

get_msg(Pid, PNum) ->
  Pid ! {self(), PNum},
  receive
    {From, Msg} ->
      io:format("~p got a msg from ~p: ~p~n", [self(), From, Msg])
  after 10000 ->
      io:format("~p did not receive a message in 10 secs~n", [self()])
  end.

send_trip(Pid, Roundtrip) ->
  Pid ! {self(), Roundtrip}.
