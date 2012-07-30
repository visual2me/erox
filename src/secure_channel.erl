-module(secure_channel).
-behavior(gen_fsm).
-export([start_link/0, set_socket/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3]).
-export([wait_for_socket/2, wait_for_hello/2, wait_for_features_reply/2, wait_for_of_msg/2]).
-vsn(1.0).
-author("Tony Wan - visual2me@gmail.com").
-date("2012/06/21 13:55:34").

-include("openflow.hrl").

-record(state, {socket, dpid, xid=0, ofv=1}).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []). 

init([]) ->
    process_flag(trap_exit, true),
    {ok, wait_for_socket, #state{}}.

terminate(_Reason, _StateName, #state{socket=Socket, dpid=Dpid}) ->
    if
	is_binary(Dpid) -> switch_man:del(Dpid);
	true -> false
    end,
    gen_tcp:close(Socket),
    ok.

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

wait_for_socket({socket_ready, Socket}, State) when is_port(Socket) ->
    inet:setopts(Socket, [binary, {packet, 0}, {active, once}]),
    % {ok, {IP, _Port}} = inet:peername(Socket),
    send_of_hello_msg(Socket, ?OFP10_VERSION, 0),
    {next_state, wait_for_hello, State#state{socket=Socket}, get_timeout()};
wait_for_socket(Other, State) ->
    % error_logger:error_msg("State: wait_for_socket. Unexpected message: ~p~n", [Other]),
    lager:info("State: wait_for_socket, msg: ~p~n", [Other]),
    {next_state, wait_for_socket, State}.


wait_for_hello({msg, <<Version:8, ?OFPT_HELLO:8, 8:16, _RecvXid:32>>}, #state{socket=Socket, xid=Xid} = State) ->
    case supported_version(Version) of
	true ->
	    send_of_features_request_msg(Socket, Version, Xid),
	    {next_state, wait_for_features_reply, State#state{xid=generate_xid(Xid), ofv=Version}, get_timeout()};
	false ->
	    lager:error("State: wait_for_hello, not supported version: ~p~n", [Version]),
	    {stop, normal, State}
    end;
wait_for_hello(timeout, State) ->
    lager:error("State: wait_for_hello, timeouted!~n"),
    {stop, normal, State};
wait_for_hello(_Other, State) ->
    lager:info("State: wait_for_hello, msg: ~p~n", [_Other]),
    {next_state, wait_for_hello, State, get_timeout()}.


wait_for_features_reply({msg, <<Version:8, ?OFPT_FEATURES_REPLY:8, _Length:16, _RecvXid:32, Dpid:64/bits, _Nbuffers:32, _Ntables:8, _Pad:24/bits, _Capabilities:32/bits, _Actions:32/bits, _PhyPorts/binary>>}, #state{socket=Socket, xid=Xid} = State) ->
    case supported_version(Version) of
	true ->
	    switch_man:add(Dpid, {self()}),
	    send_of_msg(Socket, Version, ?OFPT_SET_CONFIG, Xid, <<?OFPC_FRAG_NORMAL:16, 16#ffff:16>>), 
	    {next_state, wait_for_of_msg, State#state{dpid=Dpid, xid=generate_xid(Xid)}, get_timeout()};
	false ->
	    lager:error("State: wait_for_features_reply, not supported version: ~p~n", [Version]),
	    {stop, normal, State}
    end;
wait_for_features_reply({msg, <<Version:8, ?OFPT_ECHO_REQUEST:8, 8:16, RecvXid:32>>}, #state{socket=Socket} = State) ->
    case supported_version(Version) of
	true ->
	    send_of_echo_reply_msg(Socket, Version, RecvXid),
	    {next_state, wait_for_features_reply, State, get_timeout()};
	false ->
	    lager:error("State: wait_for_features_reply, not supported version: ~p~n", [Version]),
	    {stop, normal, State}
    end;
wait_for_features_reply(timeout, State) ->
    lager:error("State: wait_for_features_reply, timeouted!~n"),
    {stop, normal, State};
wait_for_features_reply(_Other, State) ->
    lager:info("State: wait_for_features_reply, msg: ~p~n", [_Other]),
    {next_state, wait_for_features_reply, State, get_timeout()}.



wait_for_of_msg({msg, Binary}, State) ->
    <<Version:8, _/binary>> = Binary,
    case supported_version(Version) of
	true ->
	    handle_of_msg(Binary, State);
	false ->
	    lager:error("State: wait_for_of_msg, not supported version: ~p~n", [Version]),
	    {stop, normal, State}
    end;
wait_for_of_msg({cmd, Cmd}, State) ->
    case handle_cmd(Cmd, State) of
	{ok, NewState} -> {next_state, wait_for_of_msg, NewState, get_timeout()};
	{error, Reason} ->
	    lager:error("State: wait_for_of_msg, handle_cmd error: ~p!~n", [Reason]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	{exit, Reason} ->
	    lager:error("State: wait_for_of_msg, handle_cmd error, exits with reason: ~p!~n", [Reason]),
	    {stop, normal, State}
    end;
wait_for_of_msg(timeout, State) ->
    lager:error("State: wait_for_of_msg, timeouted!~n"),
    {stop, normal, State};
wait_for_of_msg(_Other, State) ->
    lager:warning("State:wait_for_of_msg, Unknown Event:~p~n", [_Other]),
    {next_state, wait_for_of_msg, State, get_timeout()}.


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

handle_of_msg(Msg, #state{socket=Socket, dpid=Dpid, xid=Xid} = State) ->
    case Msg of
	<<Version:8, ?OFPT_HELLO:8, 8:16, _RecvXid:32>> ->
	    % Why HELLO msg now?
	    send_of_hello_msg(Socket, Version, Xid),
	    {next_state, wait_for_of_msg, State#state{xid=generate_xid(Xid)}, get_timeout()};
	<<_:8, ?OFPT_ERROR:8, Length:16, RecvXid:32, Payload/binary>> ->
	    lager:error("ERROR message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    % TODO continue or exit?
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<Version:8, ?OFPT_ECHO_REQUEST:8, 8:16, RecvXid:32>> ->
	    send_of_echo_reply_msg(Socket, Version, RecvXid),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_ECHO_REPLY:8, 8:16, RecvXid:32>> ->
	    lager:info("ECHO REPLY message(xid=~p) from switch(~p)~n", [RecvXid, Dpid]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_VENDOR:8, Length:16, RecvXid:32, Payload/binary>> ->
	    % TODO how to handle VENDOR msg?
	    lager:info("VENDOR message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_FEATURES_REQUEST:8, 8:16, RecvXid:32>> ->
	    % A controller-to-switch meg, so should not be received by the controller.
	    lager:info("FEATURES REQUEST message(xid=~p) from switch(~p)~n", [RecvXid, Dpid]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_FEATURES_REPLY:8, Length:16, RecvXid:32, RecvDpid:64/bits, Nbuffers:32, Ntables:8, _Pad:24/bits, Capabilities:32/bits, Actions:32/bits, PhyPorts/binary>> ->
	    if
		Dpid /= RecvDpid -> lager:info("Payloadpath ID not match: original=~p, received=~p~n", [Dpid, RecvDpid]);
		true -> false
	    end,
	    lager:info("FEATURES REPLY message(xid=~w) from switch(~p): length=~w, n_buffers=~w, n_tables=~w, capabilities=~w, actions=~w~n", [RecvXid, Dpid, Length, Nbuffers, Ntables, Capabilities, Actions]),
	    print_phyports(PhyPorts),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_GET_CONFIG_REQUEST:8, 8:16, RecvXid:32>> ->
	    % A controller-to-switch meg, so should not be received by the controller.
	    lager:info("GET CONFIG REQUEST message(xid=~p) from switch(~p)~n", [RecvXid, Dpid]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_GET_CONFIG_REPLY:8, Length:16, RecvXid:32, Payload/binary>> ->
	    lager:info("GET CONFIG REPLY message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_SET_CONFIG:8, Length:16, RecvXid:32, Payload/binary>> ->
	    % A controller-to-switch meg, so should not be received by the controller.
	    lager:info("SET CONFIG message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_PACKET_IN:8, _/binary>> ->
	    handle_packet_in(Msg, State);
	<<_:8, ?OFPT_FLOW_REMOVED:8, Length:16, RecvXid:32, Payload/binary>> ->
	    lager:info("FLOW REMOVED message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_PORT_STATUS:8, Length:16, RecvXid:32, Payload/binary>> ->
	    lager:info("PORT STATUS message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_PACKET_OUT:8, Length:16, RecvXid:32, Payload/binary>> ->
	    % A controller-to-switch meg, so should not be received by the controller.
	    lager:info("PACKET OUT message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_FLOW_MOD:8, Length:16, RecvXid:32, Payload/binary>> ->
	    lager:info("FLOW MOD message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_PORT_MOD:8, Length:16, RecvXid:32, Payload/binary>> ->
	    lager:info("PORT MOD message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_STATS_REQUEST:8, Length:16, RecvXid:32, Payload/binary>> ->
	    % A controller-to-switch meg, so should not be received by the controller.
	    lager:info("STATS REQUEST message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_STATS_REPLY:8, Length:16, RecvXid:32, Payload/binary>> ->
	    lager:info("STATS REPLY message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_BARRIER_REQUEST:8, Length:16, RecvXid:32, Payload/binary>> ->
	    % A controller-to-switch meg, so should not be received by the controller.
	    lager:info("BARRIER REQUEST message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_BARRIER_REPLY:8, Length:16, RecvXid:32, Payload/binary>> ->
	    lager:info("BARRIER REPLY message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_QUEUE_GET_CONFIG_REQUEST:8, Length:16, RecvXid:32, Payload/binary>> ->
	    % A controller-to-switch meg, so should not be received by the controller.
	    lager:info("QUEUE GET CONFIG REQUEST message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	<<_:8, ?OFPT_QUEUE_GET_CONFIG_REPLY:8, Length:16, RecvXid:32, Payload/binary>> ->
	    lager:info("QUEUE GET CONFIG REPLY message(xid=~p) from switch(~p): length=~p, payload=~p~n", [RecvXid, Dpid, Length, Payload]),
	    {next_state, wait_for_of_msg, State, get_timeout()};
	_Msg ->
	    lager:warning("State:wait_for_of_msg, Unknown Msg:~p~n", [_Msg]),
	    {next_state, wait_for_of_msg, State, get_timeout()}
    end.

handle_packet_in(<<_:8, ?OFPT_PACKET_IN:8, Length:16, RecvXid:32, BuffId:32, PacketLen:16, InPort:16, Reason:8, _:8, Frame/binary>>, #state{socket=Socket, dpid=Dpid, xid=Xid} = State) ->
    lager:info("PACKET IN message(xid=0x~8.16.0B) from Port#0x~4.16.0B@Switch~p: length=~p", [RecvXid, InPort, Dpid, Length]),
    case Frame of
	<<DstMAC:6/binary, SrcMAC:6/binary, 16#81, 16#00, Priority:3, DE:1, Vid:12, Type:16, Packet/binary>> -> % 802.1Q header
	    lager:info("802.1Q header", []);
	<<DstMAC:6/binary, SrcMAC:6/binary, Type:16, Packet/binary>> ->
	    case net_tools:ether_type(Type) of
		'IPv4' ->
		    case Packet of
			<<4:4, HeadLen:4, ToS:8, TotalLen:16, Id:16, Flag:3, FragOffset:13, TTL:8, Protocol:8, Chksum:16, SrcIP:4/binary, DstIP:4/binary, IPRest/binary>> ->
			    OptLen = (HeadLen - 5) * 4,
			    <<Options:OptLen/binary, DataGram/binary>> = IPRest,
			    lager:info("IPv4 packet(~s): ~s -> ~s", [net_tools:ip_protocol(Protocol), net_tools:binary_to_ipv4str(SrcIP), net_tools:binary_to_ipv4str(DstIP)]);
			_ ->
			    lager:info("Invalid IPv4 packet: packet=~p", [Packet])
		    end;
		'IPv6' ->
		    lager:info("IPv6 packet", []);
		'ARP' ->
		    case Packet of
			<<HardwareType:16, ProtocolType:16, HardwareAddrLen:8, ProtocolAddrLen:8, Operation:16, SenderMAC:6/binary, SenderIP:4/binary, TargetMAC:6/binary, TargetIP:4/binary>> ->
			    case net_tools:arp_operation(Operation) of
				arp_request ->
				    lager:info("ARP request: Host(~s/~s) asks who has ~s", [net_tools:binary_to_ipv4str(SenderIP), net_tools:binary_to_macstr(SenderMAC), net_tools:binary_to_ipv4str(TargetIP)]);
				arp_reply ->
				    lager:info("ARP reply: Hi host(~s/~s), ~s has ~s", [net_tools:binary_to_ipv4str(SenderIP), net_tools:binary_to_macstr(SenderMAC), net_tools:binary_to_macstr(TargetMAC), net_tools:binary_to_ipv4str(TargetIP)]);
				unknown ->
				    lager:error("Invalid ARP packet: operation=~p.", [Operation])
			    end;

			_ ->
			    lager:error("Invalid ARP packet.")
		    end;
		'RARP' ->
		    case Packet of
			<<HardwareType:16, ProtocolType:16, HardwareAddrLen:8, ProtocolAddrLen:8, Operation:16, SenderMAC:6/binary, SenderIP:4/binary, TargetMAC:6/binary, TargetIP:4/binary>> ->
			    case Operation of
				3 ->
				    lager:info("ARP request: Host(~s/~s) asks who has ~s", [net_tools:binary_to_ipv4str(SenderIP), net_tools:binary_to_macstr(SenderMAC), net_tools:binary_to_macstr(TargetMAC)]);
				4 ->
				    lager:info("ARP reply: Hi host(~s/~s), ~s has ~s", [net_tools:binary_to_ipv4str(SenderIP), net_tools:binary_to_macstr(SenderMAC), net_tools:binary_to_ipv4str(TargetIP), net_tools:binary_to_macstr(TargetMAC)]);
				_->
				    lager:error("Invalid ARP packet")
			    end;

			_ ->
			    lager:error("Invalid ARP packet.")
		    end;
		other -> % DONOT CARE
		    lager:info("Packets not covered yet: eth-type=~p", [Type])
	    end;
	_ ->
	    lager:info("PACKET_IN: Non Ethernet frame --> ~p", [Frame])
    end,
    {next_state, wait_for_of_msg, State, get_timeout()};
handle_packet_in(Msg, State) ->
    lager:warning("PACKET_IN: Unknown packet --> ~p", [Msg]),
    {next_state, wait_for_of_msg, State, get_timeout()}.

handle_cmd(Cmd, State) ->
    lager:info("Switch ~p received cmd ~p", [State#state.dpid, Cmd]),
    {ok, State}.


generate_xid(Xid) when Xid < 16#ffffffff, Xid >= 0 ->
    Xid + 1;
generate_xid(_Xid) ->
    0.

send_of_msg(Socket, Version, Type, Xid, Payload) ->
    Length = 8 + byte_size(Payload),
    gen_tcp:send(Socket, <<Version:8, Type:8, Length:16, Xid:32, Payload/binary>>).

send_of_hello_msg(Socket, Version, Xid) ->
    send_of_msg(Socket, Version, ?OFPT_HELLO, Xid, <<>>).

send_of_echo_reply_msg(Socket, Version, Xid) ->
    send_of_msg(Socket, Version, ?OFPT_ECHO_REPLY, Xid, <<>>).

send_of_features_request_msg(Socket, Version, Xid) ->
    send_of_msg(Socket, Version, ?OFPT_FEATURES_REQUEST, Xid, <<>>).

print_phyports(<<PhyPort:48/binary, Binary/binary>>) ->
    io:format("phyport: ~p~n", [PhyPort]),
    print_phyports(Binary);
print_phyports(<<_:0/binary>>) ->
    io:format("~n", []).

supported_version(Version) ->
    lists:member(Version, [?OFP10_VERSION, ?OFP11_VERSION, ?OFP12_VERSION]).

get_timeout() ->
    case application:get_env(erox, timeout) of
	{ok, [Timeout|_]} ->
	    if
		Timeout > 0 -> Timeout;
		Timeout == 0 -> infinity;
		true -> ?DEF_TIMEOUT
	    end;
	_ -> 
	    ?DEF_TIMEOUT
    end.
