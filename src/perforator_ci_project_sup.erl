%% @doc Project worker supervisor.

%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_project_sup).

-behavior(supervisor).

-include("perforator_ci.hrl").

%% API
-export([
    start_link/0,
    start_project/1
]).

%% supervisor callbacks
-export([init/1]).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    Return = supervisor:start_link({local, ?MODULE}, ?MODULE, []),

    % Recover
    [supervisor:start_child(?MODULE, [ID]) || #project{id=ID} <-
        perforator_ci_db:get_projects()],

    Return.

%% @doc Starts project (#project entry should exist in DB!).
-spec start_project(perforator_ci_types:project_id()) -> {ok, term()}.
start_project(ProjectID) ->
    supervisor:start_child(?MODULE, [ProjectID]).

%% =============================================================================
%% Supervisor callbacks
%% =============================================================================

init([]) ->
    {ok, {{simple_one_for_one, 1, 30}, [
        {perforator_ci_project, {perforator_ci_project, start_link, []},
        transient, 500, worker, [perforator_ci_project]}
    ]}}.
