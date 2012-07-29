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
-define(OFPT_HELLO, 0).
-define(OFPT_ERROR, 1).
-define(OFPT_ECHO_REQUEST, 2).
-define(OFPT_ECHO_REPLY, 3).
-define(OFPT_VENDOR, 4).
% Controller/switch messages
-define(OFPT_FEATURES_REQUEST, 5).
-define(OFPT_FEATURES_REPLY, 6).
-define(OFPT_GET_CONFIG_REQUEST, 7).
-define(OFPT_GET_CONFIG_REPLY, 8).
-define(OFPT_SET_CONFIG, 9).
% Async messages
-define(OFPT_PACKET_IN, 10).
-define(OFPT_FLOW_REMOVED, 11).
-define(OFPT_PORT_STATUS, 12).
% Controller/switch messages
-define(OFPT_PACKET_OUT, 13).
-define(OFPT_FLOW_MOD, 14).
-define(OFPT_PORT_MOD, 15).
-define(OFPT_STATS_REQUEST, 16).
-define(OFPT_STATS_REPLY, 17).
-define(OFPT_BARRIER_REQUEST, 18).
-define(OFPT_BARRIER_REPLY, 19).
-define(OFPT_QUEUE_GET_CONFIG_REQUEST, 20).
-define(OFPT_QUEUE_GET_CONFIG_REPLY, 21).
