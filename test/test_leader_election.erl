-module(test_leader_election).

-behaviour(leader_election).
-export([ 
	start/0, 
	prepare_participant/1,
	get_participant_weight/1,
	participant_alive/1,
	send_message/2,
	leader_elected/2,
	quorum_failed/1
]).

participant_alive({W,_}) ->
	main_pid ! { weight_enabled, W, self() },
	receive
		{ weight_enabled, Value } -> Value
	end.

send_message({_,P}, Msg) ->
	P ! Msg.

get_participant_weight({W,_}) ->
	W.

start() ->
	register(main_pid, self()),
	run_test("Test1", [100,200,300,400,500,600], [100,200,300,400,500,600], [100],         600),
	run_test("Test2", [100,200,300,400,500,600], [100,200,300,400,500,600], [100,200],     600),
	run_test("Test3", [100,200,300,400,500,600], [100,200,300,400,500,600], [100,200,300], 600),
	run_test("Test4", [100,200,300,400,500,600], [100,200,300,400],         [100], 400),
	run_test("Test5", [100,200,300,400,500,600], [100,200,300], [100,200,300], 300),
	init:stop().

run_test(Name, Weights, EnabledWeights, StartElectionWeights, ExpectedLeader) ->
	Ws     = lists:sort(Weights),
	Pids   = lists:map(fun(W) -> spawn(?MODULE, prepare_participant , [W]) end, Ws),
	Tuples = lists:zip(Ws, Pids),
	lists:foreach(fun(Pid) -> Pid ! { participants, Tuples } end, Pids),

	ActiveMembers = lists:filter(fun({W,_}) -> lists:member(W, StartElectionWeights) end, Tuples),
	lists:foreach(fun({_,P}) -> P ! { start_election } end, ActiveMembers),
	test_loop(Name, EnabledWeights, ExpectedLeader, length(StartElectionWeights)).

prepare_participant(W) ->
	receive
		{participants, List} ->
			leader_election:start(?MODULE, W, List)
	end.

test_loop(Name, _, _, 0) ->
	error_logger:info_msg("~s TEST OK~n", [ Name ]);

test_loop(Name, EnabledWeights, ExpectedLeader, ExpectedElections) ->
	receive
		{ leader_elected, Leader } ->
			if Leader == ExpectedLeader ->
				test_loop(Name, EnabledWeights, ExpectedLeader, ExpectedElections - 1);
			true ->
				error_logger:info_msg("~s TEST FAILED~n", [ Name ])
			end;

		{ weight_enabled, W, ReplyTo } ->
			ReplyTo ! { weight_enabled, lists:member(W, EnabledWeights) },
			test_loop(Name, EnabledWeights, ExpectedLeader, ExpectedElections)
	after 3000 ->
		error_logger:info_msg("~s Timeout. TEST FAILED~n", [ Name ])
	end.

leader_elected(Leader, _Path) ->
	main_pid ! {leader_elected, Leader}.

quorum_failed(_NumFound) ->
	true.
