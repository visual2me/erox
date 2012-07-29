-module(erox_app).
-behavior(application).
-export([start/2, stop/1]).
-vsn(1.0).
-author("Tony Wan - visual2me@gmail.com").
-date("2012/06/19 17:33:47").

-define(DEF_PORT, 6633).

start(_Type, _StartArgs) ->
    case application:get_env(erox, listen_port) of
	{ok, [Port|_]} ->
	    if
		Port > 0 -> true;
		true -> Port = ?DEF_PORT
	    end;
	_ -> 
	    Port = ?DEF_PORT
    end,
    erox_sup:start_link(get_app_env(listen_port, Port)).

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
