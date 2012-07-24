-module(erox_app).
-behavior(application).
-export([start/2, stop/1]).
-vsn(1.0).
-author("Tony Wan - lintao.wan@emc.com").
-date("2012/06/19 17:33:47").

-define(DEF_PORT, 6633).

start(_Type, _StartArgs) ->
    erox_sup:start_link(get_app_env(listen_port, ?DEF_PORT)).

stop(_State) ->
    ok.

get_app_env(Opt, DefValue) ->
    case application:get_env(application:get_application(), Opt) of
	{ok, Value} -> Value;
	_ ->
	    case init:get_argument(Opt) of
		[[Value | _]] -> Value;
		error -> DefValue
	    end
    end.
