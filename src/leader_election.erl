-module(leader_election).
-behavior(gen_server).

-define(SERVER, ?MODULE).

-include("logging.hrl").

-export([elect/1]).
-export([start_link/0]).
-export([stop/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

elect(Timeout) ->
	gen_server:call(?SERVER, start_election, Timeout).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

init([]) ->
	{ok, {}}.

node_alive(pong) ->
	true;
node_alive(pang) ->
	false;
node_alive(N) ->
	?idbg("node alive ~p", [ N ]),
	node_alive(net_adm:ping(N)).

%% @private
handle_call(stop, _From, _State) ->
	{stop, normal, stopped, _State};

handle_call(start_election, _From, _State) ->
	{ok, Ps}   = application:get_env(participants),

	?idbg("start_election participants: ~p", [ Ps ]),

	AliveNodes = lists:filter(fun(N) -> node_alive(N) end, Ps),
	MinNode    = lists:min(AliveNodes),

	if node() == MinNode ->
		if length(AliveNodes) * 2 > length(Ps) ->
			{reply, node(), _State};
		true ->
			{reply, quorum_failed, _State}
		end;
	true ->
		Response = gen_server:call({?SERVER, MinNode}, start_election),
		{reply, Response, _State}
	end;

handle_call(_Request, _From, _State) ->
	{reply, ignored, _State}.

%% @private
handle_cast(_Msg, _State) ->
	{noreply, _State}.

%% @private
handle_info(_Info, _State) ->
	{noreply, _State}.

%% @private
terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, _State, _Extra) ->
	{ok, _State}.
