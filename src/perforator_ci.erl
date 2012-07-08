%% API
%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci).

-include("perforator_ci.hrl").

%% API
-export([
    create_and_start_project/1,
    update_project/1,
    get_builders/0
]).

-export([start/0, stop/0, init/0]).

%% ============================================================================
%% API
%% ============================================================================

%% @doc Creates project (in DB) and starts project handler process.
-spec create_and_start_project({
        perforator_ci_types:project_name(), perforator_ci_types:repo_url(),
        perforator_ci_types:branch(), perforator_ci_types:repo_backend(),
        perforator_ci_types:polling_strategy(),
        perforator_ci_types:build_instructions(), list()}) ->
            perforator_ci_types:project_id().
create_and_start_project({Name, RepoUrl, Branch, RepoBackend, Polling,
        BuildInstr, Info}) ->
    ID = perforator_ci_db:create_project({Name, RepoUrl, Branch, RepoBackend,
        Polling, BuildInstr, Info}),

    RepoBackend:clone(RepoUrl, perforator_ci_utils:repo_path(ID)), 

    case perforator_ci_project:is_project_running(ID) of
        true -> ok; % do nothing, already started
        false -> {ok, _} = perforator_ci_project_sup:start_project(ID)
    end,

    ID.

%% @doc Updates project and corresponding process by pushing new data to DB and
%% restarting the process.
-spec update_project({
        perforator_ci_types:project_id(),
        perforator_ci_types:project_name(), perforator_ci_types:repo_url(),
        perforator_ci_types:branch(), perforator_ci_types:repo_backend(),
        perforator_ci_types:polling_strategy(),
        perforator_ci_types:build_instructions(), list()}) ->
            ok.
update_project({ID, Name, RepoUrl, Branch, RepoBackend, Polling,
        BuildInstr, Info}) ->
    ok = perforator_ci_db:update_project({ID, Name, RepoUrl, Branch,
        RepoBackend, Polling, BuildInstr, Info}),

    exit(perforator_ci_project:get_pid(ID), '$restart'),

    ok.

%% @doc Returns builders list with their queue size.
-spec get_builders() -> [{node(), integer()}].
get_builders() ->
    [{B, perforator_ci_builder:get_queue_size(B)} ||
        B <-perforator_ci_builder:get_builders()].

%% ============================================================================

start() ->
    ?mute(begin
        application:start(compiler),
        application:start(syntax_tools),
        application:start(lager)
    end),
    ?silent(error, begin
        pg2:start(),
        application:start(mnesia),
        application:start(gproc),
        application:start(cowboy),
        application:start(perforator_ci)
    end).

stop() ->
    ?silent(error, begin
        application:stop(perforator_ci),
        application:stop(cowboy),
        application:stop(gproc),
        application:stop(mnesia)
    end),
    ?mute(begin
        application:stop(lager),
        application:stop(syntax_tools),
        application:stop(compiler)
    end).

%% @doc See perforator_ci_db:init/0 for more info.
init() ->
    perforator_ci_db:init().
