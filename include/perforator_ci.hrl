%% ============================================================================
%% Record definitions
%% ============================================================================

-record(project, {
    id=0 :: perforator_ci_types:project_id(), % unique
    name :: perorator_ci_types:project_name(), % unique

    repo_url :: perforator_ci_types:repo_url(),
    branch :: perforator_ci_types:branch(),
    repo_backend=git :: perforator_ci_types:repo_backend(),

    polling=on_demand :: perforator_ci_types:polling_strategy(),
    build_instructions=[] :: list(),

    info=[] :: list() % random information
}).

-record(project_build, {
    id=0 :: perforator_ci_types:build_id(), % global id, unique
    local_id=0 :: perforator_ci_types:build_id(), % local (in project) id
    project_id=0 :: perforator_ci_types:project_id(),

    timestamp :: perforator_ci_types:timestamp(),
    commit_id :: perorator_ci_types:commit_id(), % most likely unique

    info=[] :: list(), % @todo specify
    finished=false :: boolean()
}).

%% ============================================================================
%% Defaults
%% ============================================================================

-define(HTTP_PORT, 8080).
-define(LIST_COUNT, 10).
-define(REPOS_DIR, "priv/repos").

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
            timer:sleep(10),
            Expr,
            timer:sleep(10)
        after
            lager:set_loglevel(lager_console_backend, Lager_OldLevel),
            error_logger:tty(true)
        end
    end)()).

-define(mute(Expr), (
    fun () ->
        error_logger:tty(false),
        timer:sleep(10),
        Expr,
        error_logger:tty(true)
    end)()).

%% ============================================================================
%% Stuff
%% ============================================================================

-define(FMT(Msg, Args), lists:flatten(io_lib:format(Msg, Args))).
