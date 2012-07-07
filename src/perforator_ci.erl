%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci).

%% API
-export([
    create_and_start_project/3
]).

-export([start/0, stop/0, init/0]).

%% ============================================================================
%% API
%% ============================================================================

-spec create_and_start_project(perforator_ci_types:project_name(), binary(),
        perforator_ci_types:polling_strategy()) ->
        perforator_ci_types:project_id().
create_and_start_project(Name, Repo, Polling) ->
    ID = perforator_ci_db:create_project(Name, Repo, Polling),

    case perforator_ci_project:is_project_running(ID) of
        true -> ok; % do nothing, already started
        false -> {ok, _} = perforator_ci_project_sup:start_project(ID)
    end,

    ID.

%% ============================================================================

start() ->
    application:start(compiler),
    application:start(syntax_tools),
    application:start(lager),
    application:start(mnesia),
    application:start(gproc),
    application:start(cowboy),
    application:start(perforator_ci).

stop() ->
    application:stop(perforator_ci),
    application:stop(cowboy),
    application:stop(gproc),
    application:stop(mnesia),
    application:stop(lager),
    application:stop(syntax_tools),
    application:stop(compiler).

%% @doc See perforator_ci_db:init/0 for more info.
init() ->
    perforator_ci_db:init().
