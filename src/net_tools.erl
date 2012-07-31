-module(net_tools).
-export([ether_type/1, ip_protocol/1, arp_operation/1, icmp_msg/2, tcp_port/1, udp_port/1, binary_to_ipv4str/1, binary_to_macstr/1]).
-vsn(1.0).
-author("Tony Wan - visual2me@gmail.com").
-date("2012/07/30 10:19:56").

ether_type(Type) ->
    case Type of
	16#0800	-> 'IPv4';
	16#86DD -> 'IPv6';
	16#0806 -> 'ARP';
	16#0835 -> 'RARP';
	_	-> other
    end.

ip_protocol(Protocol) ->
    case Protocol of
	1	-> 'ICMP';
	2	-> 'IGMP';
	6	-> 'TCP';
	9	-> 'IGRP';
	17	-> 'UDP';
	47	-> 'GRE';
	50	-> 'ESP';
	51	-> 'AH';
	57	-> 'SKIP';
	88	-> 'EIGRP';
	89	-> 'OSPF';
	115	-> 'L2TP';
	_	-> other
    end.

arp_operation(Operation) ->
    case Operation of
	1	-> arp_request;
	2	-> arp_reply;
	3	-> rarp_request;
	4	-> rarp_reply;
	_	-> unknown
    end.

icmp_msg(Type, Code) ->
    case Type of
	0	-> echo_reply;
	3	-> % Destination Unreachable
	    case Code of
		0	-> net_unreachable;
		1	-> host_unreachable;
		2	-> protocol_unreachable;
		3	-> port_unreachable;
		4	-> fragmentation_needed; % but DF set.
		5	-> source_route_failed;
		6	-> destination_network_unknown;
		7	-> destionation_host_unknown;
		8	-> source_host_isolated;
		9	-> network_administratively_prohibited;
		10	-> host_administratively_prohibited;
		11	-> network_unreachable_for_tos;
		12	-> host_unreachable_for_tos;
		13	-> communication_administratively_prohibited;
		_	-> destination_unreachable
	    end;
	4	-> source_quench;
	5	-> % Redirect
	    case Code of
		0	-> redirect_for_network;
		1	-> redirect_for_host;
		2	-> redirect_for_tos_and_network;
		3	-> redirect_for_tos_and_host;
		_	-> redirect
	    end;
	8	-> echo;
	9	-> router_advertisement;
	10	-> router_selection;
	11	-> % Time Exceeded
	    case Code of
		0	-> ttl_exceeded;
		1	-> fragment_reassembly_time_exceeded;
		_	-> time_exceeded
	    end;
	12	-> % Parameter Problem
	    case Code of
		0	-> pointer_indicates_the_error;
		1	-> missing_required_option;
		2	-> bad_length;
		_	-> parameter_problem
	    end;
	13	-> timestamp;
	14	-> timestamp_reply;
	15	-> infomation_request;
	16	-> infomation_reply;
	17	-> address_mask_request;
	18	-> address_mask_reply;
	30	-> traceroute;
	_	-> unknown
    end.

tcp_port(Port) ->
    case Port of
	7	-> echo;
	19	-> chargen;
	20	-> ftp-data;
	21	-> ftp-control;
	22	-> ssh;
	23	-> telnet;
	25	-> smtp;
	53	-> domain;
	79	-> finger;
	80	-> http;
	110	-> pop3;
	111	-> sunrpc;
	119	-> nntp;
	139	-> netbios-ssn;
	143	-> imap;
	179	-> bgp;
	389	-> ldap;
	443	-> https;
	445	-> microsoft-ds;
	1080	-> socks;
	_	-> unknown
    end.

udp_port(Port) ->
    case Port of
	7	-> echo;
	19	-> chargen;
	37	-> time;
	53	-> domain;
	67	-> bootps;
	68	-> bootpc;
	69	-> tftp;
	137	-> netbios-ns;
	138	-> netbios-dgm;
	161	-> snmp;
	162	-> snmp-trap;
	500	-> isakmp;
	514	-> syslog;
	520	-> rip;
	33434	-> traceroute;
	_	-> unknown
    end.

binary_to_ipv4str(<<Byte1:8, Byte2:8, Byte3:8, Byte4:8>>) ->
    lists:concat([Byte1, '.', Byte2, '.', Byte3, '.', Byte4]);
binary_to_ipv4str(_) ->
    invalid_ip.

binary_to_macstr(<<Byte1:8, Byte2:8, Byte3:8, Byte4:8, Byte5:8, Byte6:8>>) ->
    lists:concat([lists:flatten(io_lib:format("~2.16.0B", [Byte1])), ':', lists:flatten(io_lib:format("~2.16.0B", [Byte2])), ':', lists:flatten(io_lib:format("~2.16.0B", [Byte3])), ':', lists:flatten(io_lib:format("~2.16.0B", [Byte4])), ':', lists:flatten(io_lib:format("~2.16.0B", [Byte5])), ':', lists:flatten(io_lib:format("~2.16.0B", [Byte6]))]);
binary_to_macstr(_) ->
    invalid_mac.
