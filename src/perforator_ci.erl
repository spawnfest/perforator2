%% @author Martynas <martynas@numeris.lt>

-module(perforator_ci).

-export([start/0, init/0]).

%% ============================================================================

start() ->
    application:start(compiler),
    application:start(syntax_tools),
    application:start(lager),
    application:start(cowboy),
    application:start(perforator_ci).


init() -> ok.
