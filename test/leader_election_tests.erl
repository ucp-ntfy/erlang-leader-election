-module(leader_election_tests).
-include_lib("eunit/include/eunit.hrl").

-export([start_node/2]).

start_node(Node, Participants) ->
	NodeString = atom_to_list(Node),
	ConfigFile = NodeString ++ ".config",
	PsStrings  = lists:map(fun(A) -> atom_to_list(A) end, Participants),
	file:write_file(ConfigFile, "[{leader_election, [ {participants, [ " ++ string:join(PsStrings, ",") ++ " ]} ]}]."),
	?cmd("erl -name " ++ NodeString ++ " -config " ++ ConfigFile ++ " -s leader_election_link start_link").

leader_election_simple_test() ->
	Participants = [ns1@localhost, ns2@localhost],
	rpc:call(ns1@localhost, init, stop, []),
	rpc:call(ns2@localhost, init, stop, []),
	P1 = spawn_link(?MODULE, start_node, [ns1@localhost, Participants]),
%	P2 = spawn_link(?MODULE, start_node, ["ns2@localhost", Participants]),

	timer:sleep(5000),

	Master = node(),
	register(?MODULE, self()),

	Pid = spawn_link(ns1@localhost, fun() ->
		{ Master, ?MODULE } ! test_completed
	end),

	io:format("~p~n", Pid),

	receive
		test_completed ->
			io:format("SUCCESS~n")
	after 3000 ->
		io:format("timeout~n")
	end,
	ok.
