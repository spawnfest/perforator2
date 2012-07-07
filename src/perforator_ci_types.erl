%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_types).

-export_type([
    project_id/0,
    build_id/0,
    commit_id/0
]).

%% ============================================================================

-type project_id() :: binary().
-type build_id() :: integer().
-type commit_id() :: binary().
