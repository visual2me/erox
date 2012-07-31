% Definitions for OpenFlow Specification.

% Default timeout for ECHO reply -- 60 seconds
-define(DEF_TIMEOUT, 60000).

% Version number:
% Non-experimental versions released: 0x01, 0x02, 0x03
% Experimental versions released: 0x81 -- 0x99
-define(OFP10_VERSION, 1).
-define(OFP11_VERSION, 2).
-define(OFP12_VERSION, 3).

% Port numbering. Physical ports are numbered starting from 1.
% Maximum number of physical switch ports
-define(OFPP_MAX, 16#ff00).
% Send the packet out the input port.
% This virtual port must be explicitly used 
% in order to send back out of the input port.
-define(OFPP_IN_PORT, 16#fff8).
% Perform actions in flow table.
% NB: This can only be the destination port for packet-out messages.
-define(OFPP_TABLE, 16#fff9).
% Process with normal L2/L3 switching.
-define(OFPP_NORMAL, 16#fffa).
% All physical ports except input port and those disabled by STP.
-define(OFPP_FLOOD, 16#fffb).
% All physical ports except input port.
-define(OFPP_ALL, 16#fffc).
% Send to controller.
-define(OFPP_CONTROLLER, 16#fffd).
% Local openflow "port".
-define(OFPP_LOCAL, 16#fffe).
% Not associated with a physical port.
-define(OFPP_NONE, 16#ffff).


% OpenFlow config flags
% Handling of IP fragments
% No special handling for fragments.
-define(OFPC_FRAG_NORMAL, 0).
% Drop fragments.
-define(OFPC_FRAG_DROP, 1).
% Reassemble (only if OFPC_IP_REASM set).
-define(OFPC_FRAG_REASM, 2).
-define(OFPC_FRAG_MASK, 3).


% Messages on the security channel between controller and switch
% Symmetric messages
-define(OFPT_HELLO, 			0).
-define(OFPT_ERROR, 			1).
-define(OFPT_ECHO_REQUEST,		2).
-define(OFPT_ECHO_REPLY,		3).
-define(OFPT_VENDOR,			4).
% Controller/switch messages
-define(OFPT_FEATURES_REQUEST,		5).
-define(OFPT_FEATURES_REPLY,		6).
-define(OFPT_GET_CONFIG_REQUEST,	7).
-define(OFPT_GET_CONFIG_REPLY, 		8).
-define(OFPT_SET_CONFIG, 		9).
% Async messages
-define(OFPT_PACKET_IN, 	       10).
-define(OFPT_FLOW_REMOVED, 	       11).
-define(OFPT_PORT_STATUS,	       12).
% Controller/switch messages
-define(OFPT_PACKET_OUT, 	       13).
-define(OFPT_FLOW_MOD, 		       14).
-define(OFPT_PORT_MOD, 		       15).
-define(OFPT_STATS_REQUEST,	       16).
-define(OFPT_STATS_REPLY,	       17).
-define(OFPT_BARRIER_REQUEST,	       18).
-define(OFPT_BARRIER_REPLY,	       19).
-define(OFPT_QUEUE_GET_CONFIG_REQUEST, 20).
-define(OFPT_QUEUE_GET_CONFIG_REPLY,   21).

% Capabilities supported by the datapath
-define(OFPC_FLOW_STATS,	2#00000001). % Flow statistics.
-define(OFPC_TABLE_STATS,	2#00000010). % Table statistics.
-define(OFPC_PORT_STATS,	2#00000100). % Port statistics.
-define(OFPC_STP,		2#00001000). % 802.1d spanning tree.
-define(OFPC_RESERVED,		2#00010000). % Reserved, must be zero.
-define(OFPC_IP_REASM,		2#00100000). % Can reassemble IP fragments.
-define(OFPC_QUEUE_STATS,	2#01000000). % Queue statistics.
-define(OFPC_ARP_MATCH_IP,	2#10000000). % Match IP address in ARP packets.

% Port config.
% Flags to indicate behavior of the physical port.
-define(OFPPC_PORT_DOWN,	2#00000001). % Port is administratively down.
-define(OFPPC_NO_STP,		2#00000010). % Disable 802.1D spanning tree on port.
-define(OFPPC_NO_RECV,		2#00000100). % Drop all packets except 802.1D spanning.
-define(OFPPC_NO_RECV_STP,	2#00001000). % Drop received 802.1d STP packets.
-define(OFPPC_NO_FLOOD,		2#00010000). % Do not include this port when flooding.
-define(OFPPC_NO_FWD,		2#00100000). % Drop packets forwarded to port.
-define(OFPPC_NO_PACKET_IN,	2#01000000). % Do not send packet-in msgs for port.

% Current state of the physical port.
-define(OFPPS_LINK_DOWN,	2#0000000001). % No physical link present.
-define(OFPPS_STP_LISTEN,	2#0000000000). % Not learning or relaying frames.
-define(OFPPS_STP_LEARN,	2#0100000000). % Learning but not relaying frames.
-define(OFPPS_STP_FORWARD,	2#1000000000). % Learning and relaying frames.
-define(OFPPS_STP_BLOCK,	2#1100000000). % Not part of spanning tree.
-define(OFPPS_STP_MASK,		2#1100000000). % Bit mask for OFPPS_STP_* values.

% Features of physical ports available in a datapath.
-define(OFPPF_10MB_HD,		2#000000000001). % 10Mb half-duplex.
-define(OFPPF_10MB_FD,		2#000000000010). % 10Mb full-deplex.
-define(OFPPF_100MB_HD,		2#000000000100). % 100Mb half-duplex.
-define(OFPPF_100MB_FD,		2#000000001000). % 100Mb full-duplex.
-define(OFPPF_1GB_HD,		2#000000010000). % 1Gb half-duplex.
-define(OFPPF_1GB_FD,		2#000000100000). % 1Gb full-duplex.
-define(OFPPF_10GB_FD,		2#000001000000). % 10Gb full-duplex.
-define(OFPPF_COPPER,		2#000010000000). % Copper medium.
-define(OFPPF_FIBER,		2#000100000000). % Fiber medium.
-define(OFPPF_AUTONEG,		2#001000000000). % Auto-negotiation.
-define(OFPPF_PAUSE,		2#010000000000). % Pause.
-define(OFPPF_PAUSE_ASYM,	2#100000000000). % Asymmetric pause.

% What changed about the physical port.
-define(OFPPR_ADD,	0).
-define(OFPPR_DELETE,	1).  
-define(OFPPR_MODIFY,	2).  

% Reasons for PACKET_IN message
-define(OFPR_NO_MATCH,	0).
-define(OFPR_ACTION,	1).

%  Action types.
-define(OFPAT_OUTPUT,		0). % Output to switch port.
-define(OFPAT_SET_VLAN_VID,	1). % Set 802.1q VLAN id.
-define(OFPAT_SET_VLAN_PCP,	2). % Set 802.1q priority.
-define(OFPAT_STRIP_VLAN,	3). % Strip 802.1q header.
-define(OFPAT_SET_DL_SRC,	4). % Ethernet src address.
-define(OFPAT_SET_DL_DST,	5). % Ethernet dst address.
-define(OFPAT_SET_NW_SRC,	6). % IP src address.
-define(OFPAT_SET_NW_DST,	7). % IP dst address.
-define(OFPAT_SET_NW_TOS,	8). % IP ToS (DSCP field, 6 bits).
-define(OFPAT_SET_TP_SRC,	9). % TCP/UDP src port.
-define(OFPAT_SET_TP_DST,      10). % TCP/UDP dst port.
-define(OFPAT_ENQUEUE,	       11). % Output to queue.
-define(OFPAT_VENDOR,	  16#ffff).


% FLOW_MOD commands.
-define(OFPFC_ADD,		0). % New flow.
-define(OFPFC_MODIFY,		1). % Modify all matching flows.
-define(OFPFC_MODIFY_STRICT,	2). % Modify entry strictly matching wildcards.
-define(OFPFC_DELETE,		3). % Delete all matching flows.
-define(OFPFC_DELETE_STRICT,	4). % Strictly match wildcards and priority.


% Flow wildcards.
-define(OFPFW_IN_PORT,	2#00000001). % Switch input port.
-define(OFPFW_DL_VLAN,	2#00000010). % VLAN id.
-define(OFPFW_DL_SRC,	2#00000100). % Ethernet src address.
-define(OFPFW_DL_DST,	2#00001000). % Ethernet dst address.
-define(OFPFW_DL_TYPE,	2#00010000). % Ethernet frame type.
-define(OFPFW_NW_PROTO,	2#00100000). % IP protocol.
-define(OFPFW_TP_SRC,	2#01000000). % TCP/UDP src port.
-define(OFPFW_TP_DST,	2#10000000). % TCP/UDP dst port.

% FLOW_MOD flags.
-define(OFPFF_SEND_FLOW_REM,	2#001). % Send flow removed message when flow expires or is deleted.
-define(OFPFF_CHECK_OVERLAP,	2#010). % Check for overlapping entries first.
-define(OFPFF_EMERG,		2#100). % Remark this is for emergency.

% Reasons for flow removed.
-define(OFPRR_IDLE_TIMEOUT,	0). % Flow idle time exceeded idle_timeout.
-define(OFPRR_HARD_TIMEOUT,	1). % Time exceeded hard_timeout.
-define(OFPRR_DELETE,		2). % Evicted by a DELETE flow mod.

% Error type in ofp_error_msg.
-define(OFPET_HELLO_FAILED,	0). % Hello protocol failed.
-define(OFPET_BAD_REQUEST,	1). % Request was not understood.
-define(OFPET_BAD_ACTION,	2). % Error in action description.
-define(OFPET_FLOW_MOD_FAILED,	3). % Problem modifying flow entry.
-define(OFPET_PORT_MOD_FAILED,	4). % Port mod request failed.
-define(OFPET_QUEUE_OP_FAILED,	5). % Queue operation failed.

% ofp_error_msg 'code' for OFPET_HELLO_FAILED.
-define(OFPHFC_INCOMPATIBLE,	0). % No compatible version.
-define(OFPHFC_EPERM,		1). % Permission error.

% ofp_error_msg 'code' for OFPET_BAD_REQUEST.
-define(OFPBRC_BAD_VERSION,	0). % ofp_header.version not supported.
-define(OFPBRC_BAD_TYPE,	1). % ofp_header.type not supported.
-define(OFPBRC_BAD_STAT,	2). % ofp_stats_request.type not supported.
-define(OFPBRC_BAD_VENDOR,	3). % Vendor not supported (in ofp_vendor_header * or ofp_stats_request or ofp_stats_reply).
-define(OFPBRC_BAD_SUBTYPE,	4). % Vendor subtype not supported.
-define(OFPBRC_EPERM,		5). % Permissions error.
-define(OFPBRC_BAD_LEN,		6). % Wrong request length for type.
-define(OFPBRC_BUFFER_EMPTY,	7). % Specified buffer has already been used.
-define(OFPBRC_BUFFER_UNKNOWN,	8). % Specified buffer does not exist.

% ofp_error_msg 'code' for OFPET_BAD_ACTION.
-define(OFPBAC_BAD_TYPE,	0). % Unknown action type.
-define(OFPBAC_BAD_LEN,		1). % Length problem in actions.
-define(OFPBAC_BAD_VENDOR,	2). % Unknown vendor id specified.
-define(OFPBAC_BAD_VENDOR_TYPE,	3). % Unknown action type for vendor id.
-define(OFPBAC_BAD_OUT_PORT,	4). % Problem validating output action.
-define(OFPBAC_BAD_ARGUMENT,	5). % Bad action argument.
-define(OFPBAC_EPERM,		6). % Permission error.
-define(OFPBAC_TOO_MANY,	7). % Can't handle so many actions.
-define(OFPBAC_BAD_QUEUE,	8). % Problem validating output queue.

% ofp_error_msg 'code' for OFPET_FLOW_MOD_FAILED.

-define(OFPFMFC_ALL_TABLES_FULL,	0). % Flow not added because of full tables.
-define(OFPFMFC_OVERLAP,		1). % Attempted to add overlapping flow with CHECK_OVERLAP flag set.
-define(OFPFMFC_EPERM,			2). % Permissions error.
-define(OFPFMFC_BAD_EMERG_TIMEOUT,	3). % Flow not added because of non-zero idle/hard timeout.
-define(OFPFMFC_BAD_COMMAND,		4). % Unknown command.
-define(OFPFMFC_UNSUPPORTED, 		5). % Unsupported action list - cannot process in the order specified.

% ofp_error_msg 'code' for OFPET_PORT_MOD_FAILED.
-define(OFPPMFC_BAD_PORT,	0). % Specified port does not exist.
-define(OFPPMFC_BAD_HW_ADDR,	1). % Specified hardware address is wrong.

% ofp_error_msg 'code' for OFPET_QUEUE_OP_FAILED.
-define(OFPQOFC_BAD_PORT,	0). % Invalid port (or port does not exist).
-define(OFPQOFC_BAD_QUEUE,	1). % Queue does not exist.
-define(OFPQOFC_EPERM,		2). % Permissions error.

% Statistics types.
-define(OFPST_DESC,		0). % Description of this OpenFlow switch.
-define(OFPST_FLOW,		1). % Individual flow statistics.
-define(OFPST_AGGREGATE,	2). % Aggregate flow statistics.
-define(OFPST_TABLE,		3). % Flow table statistics.
-define(OFPST_PORT,		4). % Physical port statistics.
-define(OFPST_QUEUE,		5). % Queue statistics for a port.
-define(OFPST_VENDOR,	  16#ffff). % Vendor extension.

% Flag: more replies to follow.
-define(OFPSF_REPLY_MORE,	1).

% Queue properties.
-define(OFPQT_NONE,	0). % No property defined for queue (default).
-define(OFPQT_MIN_RATE,	1). % Minimum datarate guaranteed.
% -define(OFPQT_MAX_RATE, *). % other types, e.g. procedence, etc.
