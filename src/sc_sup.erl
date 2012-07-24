-module(sc_sup).
-behavior(supervisor).
-export([init/1, start_sc/0]).
-vsn(1.0).
-author("Tony Wan - lintao.wan@emc.com").
-date("2012/06/21 13:47:30").
 
-define(MAX_RESTART, 3).
-define(MAX_TIME, 60).

init(_Args) ->
    io:format("sc_sup starting...~n"),
    {ok,
	{
	    {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
	    [{underfined, {secure_channel, start_link, []}, temporary, 3000, worker, [secure_channel]}]
	}
    }.

start_sc() ->
    supervisor:start_child(sc_sup, []).
