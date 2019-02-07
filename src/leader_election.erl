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

-define(QFAILED, quorum_failed).
-define(PING_TIMEOUT, 500).

safe_gen_server_call(Server, Message, Timeout, DefaultValue) ->
	case catch gen_server:call(Server, Message, Timeout) of
		Err = {'EXIT',_} ->
			?idbg("error ~p ~p ~p", [Server, Message, Err]),
			DefaultValue;
		Response ->
			?idbg("response ~p", [Response]),
			Response
	end.

elect(Timeout) ->
	Id = {node(), erlang:system_time(1000000)},
	?idbg("Starting election with id ~p", [Id]),
	safe_gen_server_call(?SERVER, {start_election, Timeout, Id}, Timeout, ?QFAILED).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

init([]) ->
	{ok, {}}.

node_alive(N) ->
	?idbg("pinging node ~p...", [ N ]),

	Ref = make_ref(),
	{?MODULE, N} ! {ping, self(), Ref},

	receive
		{pong, Ref} ->
			?idbg("node ~p is alive", [N]),
			true

	after ?PING_TIMEOUT ->
			?idbg("node ~p is not alive", [N]),
			false
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

forward_next(Me, Others, Path, Ps, From, State, Id, Timeout) ->
	spawn_link(fun() ->
		?idbg("Id: ~p", [Id]),

		case find_first_alive(Others) of
			not_found ->
				?idbg("~p Quorum failed", [ Id ]),
				gen_server:reply(From, ?QFAILED);

			[ NextAlive | _ ] ->
				?idbg("~p Next alive: ~p", [ Id, NextAlive ]),
				Reply = safe_gen_server_call({?SERVER, NextAlive}, {election, Path ++ [ Me ], Ps}, Timeout, ?QFAILED),
				gen_server:reply(From, Reply)
		end
	end),
	{noreply, State}.

handle_call(stop, _From, _State) ->
	{stop, normal, stopped, _State};

handle_call({election, Path, Ps, Id, Timeout}, From, State) ->
	?idbg("election with Id ~p Path ~p, Participants: ~p", [ Id, Path, Ps ]),

	[ Me | Others ] = make_ring(Ps),

	case Path of
		[ PathInit | _ ] ->
			if Me == PathInit ->
				if length(Path) * 2 > length(Ps) ->
					?idbg("~p Im a leader ~p", [ Id, Me ]),
					{ reply, { leader_elected, Me, Path }, State };
				true ->
					?idbg("~p Quorum failed ~p", [ Id, Me ]),
					{ reply, ?QFAILED, State }
				end;
			true ->
				if Me < PathInit ->
					forward_next(Me, Others, [], Ps, From, State, Id, Timeout);
				true ->
					forward_next(Me, Others, Path, Ps, From, State, Id, Timeout)
				end
			end;

		[] ->
			forward_next(Me, Others, Path, Ps, From, State, Id, Timeout)
	end;

handle_call({election, Path, Ps}, From, State) ->
  handle_call({election, Path, Ps, undefined, 3000}, From, State);

handle_call({start_election, Timeout, Id}, _From, _State) ->
	{ok, Ps} = application:get_env(participants),
	?idbg("start_election ~p participants: ~p, me: ~p", [Id, Ps, node()]),

	true = is_list(Ps),

	if Ps == [self] orelse Ps == [node()] ->
		{reply, {leader_elected, node(), [node()]}, _State};

	true ->
		true = lists:member(node(), Ps),
		handle_call({election, [], Ps, Id, Timeout}, _From, _State)
	end;

handle_call(start_election, From, State) ->
	handle_call({start_election, 60000, undefined}, From, State);

handle_call(ping, _From, State) ->
	{reply, pong, State}
	;

handle_call(_Request, _From, _State) ->
	{reply, ignored, _State}.

%% @private
handle_cast(_Msg, _State) ->
	{noreply, _State}.

%% @private
handle_info({ping, ReplyTo, Ref}, State) ->
	ReplyTo ! {pong, Ref},
	{noreply, State};
handle_info(_Info, _State) ->
	{noreply, _State}.

%% @private
terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, _State, _Extra) ->
	{ok, _State}.

% vim:noexpandtab
