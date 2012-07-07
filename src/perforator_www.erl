%% @author Martynas <martynas@numeris.lt>

-module(perforator_www).

-export([start/0, init/0]).

%% ============================================================================

start() ->
    application:start(compiler),
    application:start(syntax_tools),
    %application:start(lager),
    application:start(sasl),
    application:start(inets),
    application:start(cowboy),
    application:start(perforator_www).


init() -> ok.
