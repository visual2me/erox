-module(erox_sup).
-behavior(supervisor).
-export([start_link/1]).
-export([init/1]).
-vsn(1.0).
-author("Tony Wan - visual2me@gmail.com").
-date("2012/06/19 17:33:47").

-define(MAX_RESTART, 3).
-define(MAX_TIME, 60).

start_link(ListenPort) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort]).

init(ListenPort) ->
    {ok, {{one_for_all, ?MAX_RESTART, ?MAX_TIME}, [
		{switch_man, {switch_man, start_link, []}, permanent, 5000, worker, [switch_man]},
		% {switch_man, {gen_event, start_link, [{local, switch_man}]}, permanent, 5000, worker, []},
		{sc_sup, {supervisor, start_link, [{local, sc_sup}, sc_sup, []]}, permanent, infinity, supervisor, [sc_sup]},
		{erox, {erox, start_link, [ListenPort]}, permanent, 5000, worker, [erox]}
	    ]}}.
