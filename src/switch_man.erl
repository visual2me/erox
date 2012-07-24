-module(switch_man).
-behavior(gen_server).
-export([start_link/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([stop/0, add/1, del/1, show/0, count/0]).
-vsn(1.0).
-author("Tony Wan - lintao.wan@emc.com").
-date("2012/06/19 23:10:31").


start_link() ->
    io:format("switch_man starting...~n"),
    % register(?MODULE, spawn_link(?MODULE, init, [])).
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, []}.

terminate(_Reason, _LoopData) ->
    % TODO 
    ok.

stop() ->
    gen_server:cast(?MODULE, stop).

handle_cast(stop, SwitchList) ->
    {stop, normal, SwitchList}.

add(Dpid) ->
    gen_server:call(?MODULE, {add, Dpid}).

del(Dpid) ->
    gen_server:call(?MODULE, {del, Dpid}).

count() ->
    gen_server:call(?MODULE, {count}).

show() ->
    gen_server:call(?MODULE, {show}).

handle_call({add, Dpid}, _From, SwitchList) ->
    {reply, success, [Dpid|SwitchList]};
handle_call({del, Dpid}, _From, SwitchList) ->
    {reply, success, lists:delete(Dpid, SwitchList)};
handle_call({count}, _From, SwitchList) ->
    {reply, length(SwitchList), SwitchList};
handle_call({show}, _From, SwitchList) ->
    {reply, SwitchList, SwitchList};
handle_call(_Msg, _From, SwitchList) ->
    {reply, unkown, SwitchList}.

handle_info(Msg, State) ->
    io:format("Unknown Msg for switch_man: ~p~n", [Msg]),
    {noreply, State}.
