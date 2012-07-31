-module(learning_switch).
-behaviour(gen_event).
-export([]).
-vsn(1.0).
-author("Tony Wan - visual2me@gmail.com").
-date("2012/07/31 16:56:03").

init(_Args) ->
    {ok, []}.

handle_event(Event, State) ->
    % learning switch behavior.
    {ok, State}.

terminate(_Args, _State) ->
    ok.
