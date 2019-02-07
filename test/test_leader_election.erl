#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname test1@localhost -pz ebin -setcookie cookie1

wait_node(_Node, pang) ->
        ok;
wait_node(Node, pong) ->
        wait_node(Node, net_adm:ping(Node)).

stop_node(N) ->
	ok = rpc:call(N, init, stop, []),
	wait_node(N, pong).

check_elect(Nodes, ExpectedMaster) ->
	Masters = lists:map(
		fun(N) ->
			case rpc:call(N, leader_election, elect, [ 2000 ]) of
				{leader_elected,M,_} -> M;
				quorum_failed        -> quorum_failed
			end
		end,
		Nodes),
	io:format("~p~n", [ Masters ]),
	true = lists:all(fun(M) -> M == ExpectedMaster end, Masters),
	io:format("Test OK~n", []).

test(test1, Nodes) ->
	[Ns1|_] = Nodes,
	check_elect(Nodes, Ns1);

test(test2, Nodes) ->
	[Ns1|Rem1] = Nodes,
	[Ns2|_   ] = Rem1,
	stop_node(Ns1),
	check_elect(Rem1, Ns2);

test(test3, Nodes) ->
	N1 = lists:nth(2, Nodes),
	N2 = lists:nth(4, Nodes),
	stop_node(N1),
	stop_node(N2),
	check_elect(lists:delete(N1, lists:delete(N2, Nodes)), lists:nth(1, Nodes));

test(test4, Nodes) ->
	N1 = lists:nth(2, Nodes),
	N2 = lists:nth(4, Nodes),
	N3 = lists:nth(5, Nodes),
	stop_node(N1),
	stop_node(N2),
	stop_node(N3),
	check_elect(lists:delete(N3, lists:delete(N1, lists:delete(N2, Nodes))), quorum_failed).

main([]) ->
	io:format("Pass test-atom and nodes a parameters");

main([Test|SNodes]) ->
	Nodes = lists:map(fun(N) -> list_to_atom(N) end, SNodes),
	lists:map(fun(N) -> rpc:call(N, application, start, [ leader_election ]) end, Nodes),

	test(list_to_atom(Test), Nodes).

