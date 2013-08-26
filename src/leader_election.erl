-module(leader_election).

-export([
	behaviour_info/1,
	start/3]).

-ifdef(debug).
-define(idbg(FmtStr, Err), error_logger:info_msg("~p (line ~p, pid ~p): " FmtStr "~n", [?MODULE, ?LINE, self() | Err])).
-define(rdbg(Term), error_logger:info_report(Term)).
-else.
-define(idbg(_FmtStr, _Err), void).
-define(rdbg(_Term), void).
-endif.

start(Module, W, Ps) ->
	participant_loop(Module, W, rearrange_ring(Module, W, Ps)).

behaviour_info(callbacks) -> % returned list with all needed callbacks
    [
		{ participant_alive,      1 },
    	{ send_message,           2 },
		{ get_participant_weight, 1 },
		{ leader_elected,         2 },
		{ quorum_failed,          1 }
	];
behaviour_info(_Other) ->
    undefined.

rearrange_ring(Module, Weight, Participants) ->
	{LT, GE} = lists:partition(fun(P) -> Module:get_participant_weight(P) < Weight end, Participants),
	GE ++ LT.

find_first_alive(_Module, []) ->
	{ not_found };
find_first_alive(Module, [H | T]) ->
	Alive = Module:participant_alive(H),
	if Alive -> 
		[ H | T ];
	true -> 
		find_first_alive(Module, T)
	end.

construct_new_path(Module, [P|T], Me) ->
	W1 = Module:get_participant_weight(P),
	W2 = Module:get_participant_weight(Me),
	if W2 > W1 -> 
		[Me];
	true -> 
		[P|T] ++ [Me]
	end.

forward_election_message(Module, W, Ps, NewPath) ->
	?idbg("Forward: ~p ~p ~p", [ W, Ps, NewPath ]),

	[ _ | Others ] = Ps,
	case find_first_alive(Module, Others) of
		{ not_found } ->
			%% io:format("Damn! Nobody around is alive");
			Module:quorum_failed(0);

		[ NextAlive | _ ] ->
			?idbg("Next alive: ~p", [ NextAlive ]),
			Module:send_message(NextAlive, { election, NewPath }),
			participant_loop(Module, W, Ps)
	end.

participant_loop(Module, W, Ps) ->
	?idbg("Participants: ~p", [ Ps ]),
	[ Me | _ ] = Ps,
	receive
		{start_election} -> 
			forward_election_message(Module, W, Ps, [ Me ]);

		{election, CurrentPath} ->
			?idbg("Me ~p Path: ~p", [ Me, CurrentPath ]),
			[ Initial | _ ] = CurrentPath,

			if Initial == Me ->
				if length(CurrentPath) * 2 > length(Ps) ->
					?idbg("Im a leader ~p", [ Me ]),
					Module:leader_elected(W, CurrentPath);
				true ->
					?idbg("Quorum failed ~p", [ Me ]),
					Module:quorum_failed(length(CurrentPath) - 1)
				end,
				participant_loop(Module, W, Ps);
			true ->
				NewPath = construct_new_path(Module, CurrentPath, Me),
				forward_election_message(Module, W, Ps, NewPath)
			end
	end.
