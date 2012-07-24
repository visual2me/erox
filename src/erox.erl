-module(erox).
-behavior(gen_server).
-export([start_link/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-vsn(1.0).
-author("Tony Wan - lintao.wan@emc.com").
-date("2012/06/12 13:52:02").

-record(state, {listener, acceptor, module}).

start_link([ListenPort]) ->
    io:format("erox starting...~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ListenPort], []).

init([ListenPort]) ->
    io:format("in erox init: listen_port=~p~n", [ListenPort]),
    process_flag(trap_exit, true),
    case gen_tcp:listen(ListenPort, [binary, {packet, 0}, {reuseaddr, true}, {active, false}, {keepalive, true}, {backlog, 10}]) of
	{ok, ListenSocket} ->
	    case prim_inet:async_accept(ListenSocket, -1) of
		{ok, Ref} ->
		    io:format("async_accept~n"),
		    {ok, #state{listener = ListenSocket, acceptor = Ref, module = secure_channel}};
		{error, Reason} ->
		    io:format("listen failed: ~p~n", [Reason]),
		    {stop, Reason}
	    end;
	{error, Reason} ->
	    io:format("listen failed: ~p~n", [Reason]),
	    {stop, Reason};
	_ ->
	    io:format("what's going on?~n")
    end.

terminate(_Reason, State) ->
    gen_tcp:close(State#state.listener),
    ok.

handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({inet_async, ListenSocket, Ref, {ok, ClientSocket}}, #state{listener=ListenSocket, acceptor=Ref, module=Module} = State) ->
    case set_sockopt(ListenSocket, ClientSocket) of
	ok -> ok;
	{error, Reason} -> exit({set_sockopt, Reason})
    end,
    {ok, Pid} = sc_sup:start_sc(),
    gen_tcp:controlling_process(ClientSocket, Pid),
    Module:set_socket(Pid, ClientSocket),
    case prim_inet:async_accept(ListenSocket, -1) of
	{ok, NewRef} -> ok;
	{error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
    end,
    {noreply, State#state{acceptor = NewRef}};
handle_info({inet_async, ListenSocket, Ref, Error}, #state{listener=ListenSocket, acceptor=Ref} = State) ->
    {stop, Error, State};
handle_info(_Info, State) ->
    {noreply, State}.

set_sockopt(ListenSocket, ClientSocket) ->
    true = inet_db:register_socket(ClientSocket, inet_tcp),
    case prim_inet:getopts(ListenSocket, [active, nodelay, keepalive, delay_send, priority, tos]) of
	{ok, Opts} ->
	    case prim_inet:setopts(ClientSocket, Opts) of
		ok -> ok;
		Error ->
		    get_tcp:close(ClientSocket),
		    Error
	    end;
	Error ->
	    gen_tcp:close(ClientSocket),
	    Error
    end.
