%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_types).

-export_type([
    project_id/0,
    project_name/0,
    build_id/0,
    commit_id/0,
    timestamp/0,
    polling_strategy/0
]).

%% ============================================================================

-type project_id() :: integer().
-type project_name() :: integer().
-type build_id() :: integer().
-type commit_id() :: binary().
-type timestamp() :: binary().
-type polling_strategy() :: {time, integer()} | on_demand. % Time is in ms!
