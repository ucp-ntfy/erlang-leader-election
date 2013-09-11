#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -name test3 -pz ebin

main([]) ->
	io:format("Pass node name as a parameter");

main([Node|_]) ->
	N = list_to_atom(Node),
	rpc:call(N, init, stop, []),
	wait_node(N, pong).

wait_node(_Node, pang) ->
	ok;
wait_node(Node, pong) ->
	wait_node(Node, net_adm:ping(Node)).

