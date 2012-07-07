%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci).

-export([start/0, stop/0, init/0]).

%% ============================================================================

start() ->
    application:start(compiler),
    application:start(syntax_tools),
    application:start(lager),
    application:start(mnesia),
    application:start(cowboy),
    application:start(perforator_ci).

stop() ->
    application:stop(perforator_ci),
    application:stop(cowboy),
    application:stop(mnesia),
    application:stop(lager),
    application:stop(syntax_tools),
    application:stop(compiler).

%% @doc See perforator_ci_db:init/0 for more info.
init() ->
    perforator_ci_db:init().
