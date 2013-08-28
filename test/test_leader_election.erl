#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -name test1 -pz ebin

main(_) ->
	Nodes = ['ns1@eknote.site', 'ns2@eknote.site', 'ns3@eknote.site'],
	Res   = lists:map(fun(N) -> rpc:call(N, application, start, [ leader_election ]) end, Nodes),
	io:format("~p~n", [ Res ]),
	
	Res2  = lists:map(fun(N) -> rpc:call(N, leader_election, elect, [ 2000 ]) end, Nodes),
	io:format("~p~n", [ Res2 ]).

%%	Res = rpc:call('ns1@eknote.site', application, start, [ leader_election ]),
%%	io:format("~p~n", [ Res ]).
%%	io:format("~p~n", [ spawn_link('ns1@eknote.site', fun() ->
%%		application:start(leader_election)
%%	end) ]).

