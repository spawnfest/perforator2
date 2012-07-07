%% @doc Various stuff.

%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_utils).

-export([
    timestamp/0,
    get_env/3
]).

%% ============================================================================
%% API
%% ============================================================================

-spec timestamp() -> perforator_ci_types:timestamp().
timestamp() ->
    {Mega, Sec, Micro} = erlang:now(),
    (Mega * 1000000 + Sec) * 1000000 + Micro.

%% @doc Returns application env variable or default unless it exists.
get_env(App, Key, Default) ->
    case application:get_env(App, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.
