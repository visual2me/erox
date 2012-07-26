-module(switch_man).
-behavior(gen_server).
-export([start_link/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([stop/0, add/2, del/1, show/0, count/0]).
-vsn(1.0).
-author("Tony Wan - visual2me@gmail.com").
-date("2012/06/19 23:10:31").


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    ets:new(switches, [set,private,named_table]),
    {ok, []}.

terminate(_Reason, _LoopData) ->
    ets:delete(switches),
    ok.

stop() ->
    gen_server:cast(?MODULE, stop).

handle_cast(stop, SwitchList) ->
    {stop, normal, SwitchList}.

add(Dpid, Data) ->
    gen_server:call(?MODULE, {add, {Dpid, Data}}).

del(Dpid) ->
    gen_server:call(?MODULE, {del, Dpid}).

count() ->
    gen_server:call(?MODULE, {count}).

show() ->
    gen_server:call(?MODULE, {show}).

handle_call({add, Switch}, _From, State) ->
    ets:insert(switches, Switch),
    {reply, success, State};
handle_call({del, Dpid}, _From, State) ->
    ets:delete(switches, Dpid),
    {reply, success, State};
handle_call({count}, _From, State) ->
    {reply, ets:info(switches, size), State};
handle_call({show}, _From, State) ->
    {reply, ets:match(switches, {'$0', '_'}), State};
handle_call(_Msg, _From, State) ->
    {reply, unkown, State}.

handle_info(Msg, State) ->
    lager:warning("Unknown Msg for switch_man: ~p~n", [Msg]),
    {noreply, State}.
