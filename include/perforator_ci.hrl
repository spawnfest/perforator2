%% ============================================================================
%% Record definitions
%% ============================================================================

-record(project, {
    id=0 :: perforator_ci_types:project_id(),
    name :: perorator_ci_types:project_name(),
    repo :: binary(),
    repo_backend=perforator_ci_git :: atom(),
    polling=on_demand :: perorator_ci_types:polling_strategy()
}).

-record(project_build, {
    id=0 :: perforator_ci_types:build_id(), % global id
    local_id=0 :: perforator_ci_types:build_id(), % local (in project) id
    project_id=0 :: perforator_ci_types:project_id(),
    timestamp :: perforator_ci_types:timestamp(),
    commit_id :: perorator_ci_types:commit_id(),
    info=[] :: list() % @todo specify
}).

%% ============================================================================
%% Defaults
%% ============================================================================

-define(HTTP_PORT, 8080).
-define(LIST_COUNT, 10).

%% ============================================================================
%% Log utils
%% ============================================================================

% Use these only for pretty prints in tests/console!
-define(DEFAULT_INFO(Msg), [
    {pid, self()},
    {source, ?FILE ++ ":" ++ integer_to_list(?LINE)},
    {message, Msg}
]).

-define(error(Msg, Opts),
    error_logger:error_report(?DEFAULT_INFO(Msg) ++ Opts)).

-define(warning(Msg, Opts),
    error_logger:warning_report(?DEFAULT_INFO(Msg) ++ Opts)).

-define(info(Msg, Opts),
    error_logger:info_report(?DEFAULT_INFO(Msg) ++ Opts)).

% Mute chatty expressions
-define(silent(Level, Expr), ( % Level = info | warning | error | alert ...
    fun() ->
        Lager_OldLevel = lager:get_loglevel(lager_console_backend),
        lager:set_loglevel(lager_console_backend, Level),
        error_logger:tty(false),
        try
            timer:sleep(1),
            Expr
        after
            lager:set_loglevel(lager_console_backend, Lager_OldLevel),
            error_logger:tty(true)
        end
    end)()).
