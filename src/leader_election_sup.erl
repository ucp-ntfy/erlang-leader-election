-module(leader_election_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link(?MODULE, []).

init(_Args) ->
	{ok, {{one_for_one, 60, 60},
		[{leader_election, {leader_election, start_link, []},
			permanent, brutal_kill, worker, [leader_election]}]}}.
