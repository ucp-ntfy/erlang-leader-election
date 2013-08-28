-module(leader_election_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	leader_election_sup:start_link().

stop(_State) ->
	ok.

