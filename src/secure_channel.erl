-module(secure_channel).
-behavior(gen_fsm).
-export([start_link/0, set_socket/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3]).
-export([wait_for_socket/2, wait_for_hello/2, wait_for_features_reply/2, wait_for_of10_msg/2]).
-vsn(1.0).
-author("Tony Wan - lintao.wan@emc.com").
-date("2012/06/21 13:55:34").

-include("openflow.hrl").

-record(state, {socket, dpid, xid=0}).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []). 

init([]) ->
    process_flag(trap_exit, true),
    {ok, wait_for_socket, #state{}}.

terminate(_Reason, _StateName, #state{socket=Socket, dpid=Dpid}) ->
    if
	is_binary(Dpid) -> switch_man:del(Dpid)
    end,
    gen_tcp:close(Socket),
    ok.

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

wait_for_socket({socket_ready, Socket}, State) when is_port(Socket) ->
    inet:setopts(Socket, [binary, {packet, 0}, {active, once}]),
    % {ok, {IP, _Port}} = inet:peername(Socket),
    send_of10_hello_msg(Socket, 0),
    {next_state, wait_for_hello, State#state{socket=Socket}, ?DEF_TIMEOUT};
wait_for_socket(Other, State) ->
    % error_logger:error_msg("State: wait_for_socket. Unexpected message: ~p~n", [Other]),
    io:format("State: wait_for_socket, msg: ~p~n", [Other]),
    {next_state, wait_for_socket, State}.


wait_for_hello({msg, <<?OFP10_VERSION:8, ?OFPT_HELLO:8, 8:16/big-integer, _RecvXid:32/big-integer>>}, #state{socket=Socket, xid=Xid} = State) ->
    send_of10_features_request_msg(Socket, Xid),
    {next_state, wait_for_features_reply, State#state{xid=generate_xid(Xid)}, ?DEF_TIMEOUT};
wait_for_hello(timeout, State) ->
    io:format("State: wait_for_hello, timeouted!~n"),
    {stop, normal, State};
wait_for_hello(_Other, State) ->
    io:format("State: wait_for_hello, msg: ~p~n", [_Other]),
    {next_state, wait_for_hello, State, ?DEF_TIMEOUT}.


wait_for_features_reply({msg, <<?OFP10_VERSION:8, ?OFPT_FEATURES_REPLY:8, _Length:16/big-integer, _RecvXid:32/big-integer, Dpid:64/bits, _Nbuffers:32/big-integer, _Ntables:8/integer, _Pad:24/bits, _Capabilities:32/bits, _Actions:32/bits, _PhyPorts/binary>>}, #state{socket=Socket, xid=Xid} = State) ->
    switch_man:add(Dpid),
    send_of10_msg(Socket, ?OFPT_SET_CONFIG, Xid, <<?OFPC_FRAG_NORMAL:16/big-integer, 16#ffff:16/big-integer>>),
    {next_state, wait_for_of10_msg, State#state{dpid=Dpid, xid=generate_xid(Xid)}, ?DEF_TIMEOUT};
wait_for_features_reply({msg, <<?OFP10_VERSION:8, ?OFPT_ECHO_REQUEST:8, 8:16/big-integer, RecvXid:32/big-integer>>}, #state{socket=Socket} = State) ->
    send_of10_echo_reply_msg(Socket, RecvXid),
    {next_state, wait_for_features_reply, State, ?DEF_TIMEOUT};
wait_for_features_reply(timeout, State) ->
    io:format("State: wait_for_features_reply, timeouted!~n"),
    {stop, normal, State};
wait_for_features_reply(_Other, State) ->
    io:format("State: wait_for_features_reply, msg: ~p~n", [_Other]),
    {next_state, wait_for_features_reply, State, ?DEF_TIMEOUT}.


wait_for_of10_msg({msg, Binary}, #state{socket=Socket, dpid=Dpid, xid=Xid} = State) ->
    case Binary of
	<<?OFP10_VERSION:8, ?OFPT_HELLO:8, 8:16/big-integer, _RecvXid:32/big-integer>> ->
	    % Why HELLO msg now?
	    send_of10_hello_msg(Socket, Xid),
	    {next_state, wait_for_of10_msg, State#state{xid=generate_xid(Xid)}, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_ERROR:8, Length:16/big-integer, RecvXid:32/big-integer, Payload/binary>> ->
	    io:format("ERROR message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    % TODO continue or exit?
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_ECHO_REQUEST:8, 8:16/big-integer, RecvXid:32/big-integer>> ->
	    send_of10_echo_reply_msg(Socket, RecvXid),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_ECHO_REPLY:8, 8:16/big-integer, RecvXid:32/big-integer>> ->
	    io:format("ECHO REPLY message(xid=~p) from switch(~p)~n", [RecvXid, Dpid]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_VENDOR:8, Length:16/big-integer, RecvXid:32/big-integer, Payload/binary>> ->
	    % TODO how to handle VENDOR msg?
	    io:format("VENDOR message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_FEATURES_REQUEST:8, 8:16/big-integer, RecvXid:32/big-integer>> ->
	    % A controller-to-switch meg, so should not be received by the controller.
	    io:format("FEATURES REQUEST message(xid=~p) from switch(~p)~n", [RecvXid, Dpid]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_FEATURES_REPLY:8, Length:16/big-integer, RecvXid:32/big-integer, RecvDpid:64/bits, Nbuffers:32/big-integer, Ntables:8/integer, _Pad:24/bits, Capabilities:32/bits, Actions:32/bits, PhyPorts/binary>> ->
	    if
		Dpid /= RecvDpid -> io:format("Payloadpath ID not match: original=~p, received=~p~n", [Dpid, RecvDpid])
	    end,
	    io:format("FEATURES REPLY message(xid=~w) from switch(~p): length=~w, n_buffers=~w, n_tables=~w, capabilities=~w, actions=~w~n", [RecvXid, Dpid, Length, Nbuffers, Ntables, Capabilities, Actions]),
	    print_phyports(PhyPorts),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_GET_CONFIG_REQUEST:8, 8:16/big-integer, RecvXid:32/big-integer>> ->
	    % A controller-to-switch meg, so should not be received by the controller.
	    io:format("GET CONFIG REQUEST message(xid=~p) from switch(~p)~n", [RecvXid, Dpid]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_GET_CONFIG_REPLY:8, Length:16/big-integer, RecvXid:32/big-integer, Payload/binary>> ->
	    io:format("GET CONFIG REPLY message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_SET_CONFIG:8, Length:16/big-integer, RecvXid:32/big-integer, Payload/binary>> ->
	    % A controller-to-switch meg, so should not be received by the controller.
	    io:format("SET CONFIG message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_PACKET_IN:8, Length:16/big-integer, RecvXid:32/big-integer, Payload/binary>> ->
	    io:format("PACKET IN message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_FLOW_REMOVED:8, Length:16/big-integer, RecvXid:32/big-integer, Payload/binary>> ->
	    io:format("FLOW REMOVED message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_PORT_STATUS:8, Length:16/big-integer, RecvXid:32/big-integer, Payload/binary>> ->
	    io:format("PORT STATUS message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_PACKET_OUT:8, Length:16/big-integer, RecvXid:32/big-integer, Payload/binary>> ->
	    % A controller-to-switch meg, so should not be received by the controller.
	    io:format("PACKET OUT message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_FLOW_MOD:8, Length:16/big-integer, RecvXid:32/big-integer, Payload/binary>> ->
	    io:format("FLOW MOD message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_PORT_MOD:8, Length:16/big-integer, RecvXid:32/big-integer, Payload/binary>> ->
	    io:format("PORT MOD message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_STATS_REQUEST:8, Length:16/big-integer, RecvXid:32/big-integer, Payload/binary>> ->
	    % A controller-to-switch meg, so should not be received by the controller.
	    io:format("STATS REQUEST message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_STATS_REPLY:8, Length:16/big-integer, RecvXid:32/big-integer, Payload/binary>> ->
	    io:format("STATS REPLY message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_BARRIER_REQUEST:8, Length:16/big-integer, RecvXid:32/big-integer, Payload/binary>> ->
	    % A controller-to-switch meg, so should not be received by the controller.
	    io:format("BARRIER REQUEST message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_BARRIER_REPLY:8, Length:16/big-integer, RecvXid:32/big-integer, Payload/binary>> ->
	    io:format("BARRIER REPLY message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_QUEUE_GET_CONFIG_REQUEST:8, Length:16/big-integer, RecvXid:32/big-integer, Payload/binary>> ->
	    % A controller-to-switch meg, so should not be received by the controller.
	    io:format("QUEUE GET CONFIG REQUEST message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	<<?OFP10_VERSION:8, ?OFPT_QUEUE_GET_CONFIG_REPLY:8, Length:16/big-integer, RecvXid:32/big-integer, Payload/binary>> ->
	    io:format("QUEUE GET CONFIG REPLY message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT};
	_Msg ->
	    io:format("State:wait_for_of10_msg, Unknown Msg:~p~n", [_Msg]),
	    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT}
    end;
wait_for_of10_msg(timeout, State) ->
    io:format("State: wait_for_of10_msg, timeouted!~n"),
    {stop, normal, State};
wait_for_of10_msg(_Other, State) ->
    io:format("State:wait_for_of10_msg, Unknown Event:~p~n", [_Other]),
    {next_state, wait_for_of10_msg, State, ?DEF_TIMEOUT}.


handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({msg, Bin}, StateData);
handle_info({tcp_closed, Socket}, _StateName, #state{socket=Socket} = StateData) ->
    {stop, normal, StateData};
handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.


generate_xid(Xid) when Xid < 16#ffffffff, Xid >= 0 ->
    Xid + 1;
generate_xid(_Xid) ->
    0.

send_of10_msg(Socket, Type, Xid, Payload) ->
    Length = 8 + byte_size(Payload),
    gen_tcp:send(Socket, <<?OFP10_VERSION:8, Type:8, Length:16/big-integer, Xid:32/big-integer, Payload/binary>>).

send_of10_hello_msg(Socket, Xid) ->
    send_of10_msg(Socket, ?OFPT_HELLO, Xid, <<>>).

send_of10_echo_reply_msg(Socket, Xid) ->
    send_of10_msg(Socket, ?OFPT_ECHO_REPLY, Xid, <<>>).

send_of10_features_request_msg(Socket, Xid) ->
    send_of10_msg(Socket, ?OFPT_FEATURES_REQUEST, Xid, <<>>).

print_phyports(<<PhyPort:48/binary, Binary/binary>>) ->
    io:format("phyport: ~p~n", [PhyPort]),
    print_phyports(Binary);
print_phyports(<<_:0/binary>>) ->
    io:format("~n", []).
