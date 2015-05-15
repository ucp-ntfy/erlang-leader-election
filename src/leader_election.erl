-module(leader_election).
-behavior(gen_server).

-define(SERVER, ?MODULE).

-include("logging.hrl").

-export([elect/1]).
-export([start_link/0]).
-export([stop/0]).
-export([ping/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

safe_gen_server_call(Server, Message, Timeout, DefaultValue) ->
	case catch gen_server:call(Server, Message, Timeout) of
		{'EXIT',_} ->
			DefaultValue;
		Response ->
			Response
	end.

elect(Timeout) ->
	safe_gen_server_call(?SERVER, start_election, Timeout, quorum_failed).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

ping() ->
	pong.

init([]) ->
	{ok, {}}.

% node_alive(pong) ->
%	true;
%node_alive(pang) ->
%	false;
node_alive(N) ->
	?idbg("node alive ~p", [ N ]),
	case catch rpc:block_call(N, ?MODULE, ping, [], 3000) of
%	case catch gen_server:call({?SERVER, N}, ping) of
		pong -> true;
		_    -> false
	end.

find_first_alive([]) ->
    not_found;
find_first_alive([H | T]) ->
    Alive = node_alive(H),
    if Alive ->
        [ H | T ];
    true ->
        find_first_alive(T)
    end.

%% returns array where first element is current node and the rest is ring of nodes
make_ring(Ps) ->
	Node = node(),
	true = lists:member(Node, Ps),
	{LT,GT} = lists:partition(fun(P) -> P < Node end, Ps),
	GT ++ LT.

forward_next(Me, Others, Path, Ps, From, State) ->
	case find_first_alive(Others) of
		not_found ->
			{reply, quorum_failed, State};
		[ NextAlive | _ ] ->
			?idbg("Next alive: ~p", [ NextAlive ]),
			spawn_link(fun() ->
				Reply = safe_gen_server_call({?SERVER, NextAlive}, {election, Path ++ [ Me ], Ps}, 3000, quorum_failed),
				gen_server:reply(From, Reply)
			end),
			{noreply, State}
	end.

handle_call(stop, _From, _State) ->
	{stop, normal, stopped, _State};

handle_call({election, Path, Ps}, From, State) ->
	?idbg("election with Path ~p, Participants: ~p", [ Path, Ps ]),

	[ Me | Others ] = make_ring(Ps),

	case Path of
		[ PathInit | _ ] ->
			if Me == PathInit ->
				if length(Path) * 2 > length(Ps) ->
					?idbg("Im a leader ~p", [ Me ]),
					{ reply, { leader_elected, Me, Path }, State };
				true ->
					?idbg("Quorum failed ~p", [ Me ]),
					{ reply, quorum_failed, State }
				end;
			true ->
				if Me < PathInit ->
					forward_next(Me, Others, [], Ps, From, State);
				true ->
					forward_next(Me, Others, Path, Ps, From, State)
				end
			end;

		[] ->
			forward_next(Me, Others, Path, Ps, From, State)
	end;

handle_call(start_election, _From, _State) ->
	{ok, Ps} = application:get_env(participants),
	?idbg("start_election participants: ~p, me: ~p", [ Ps, node() ]),
	true = lists:member(node(), Ps),
	if length(Ps) == 1 ->
		{ reply, {leader_elected, node(), [ node() ]}, _State };
	true ->
		handle_call({election, [], Ps}, _From, _State)
	end;

handle_call(ping, _From, State) ->
	{reply, pong, State}
	;

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
