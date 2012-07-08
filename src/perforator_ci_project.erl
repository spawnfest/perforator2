%% @doc Project process handler.
%% The handler is responsible for querying repo (that it clones to local dir),
%% checking for changes and asking builder to build them.
%% Also, the handler stores build results (i.e. `perforator` statistics).
%%
%% WARNING: all API calls can't be done from remote nodes, because we use
%% non-distributed `gproc` for identifying process handler processes.

%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_project).

-behaviour(gen_server).

-include("perforator_ci.hrl").

%% API
-export([
    is_project_running/1,
    start_link/1,
    build_finished/4,
    get_pid/1,
    build_now/1
]).

%% gen_server callbacks
-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-record(state, {
    project_id :: perforator_ci_types:project_id(),

    repo_url :: perforator_ci_types:repo_url(),
    branch :: perforator_ci_types:branch(),
    repo_backend :: perforator_ci_types:repo_backend(),

    polling=on_demand :: perforator_ci_types:polling_strategy(),
    % there are two strategies: either build on demand or poll after some time

    last_build_id=0 :: perforator_ci_types:build_id(),
    last_commit_id=undefined :: perforator_ci_types:commit_id()
}).

-ifdef(TEST).
-compile(export_all).
-endif.

%% ============================================================================
%% API
%% ============================================================================

%% @doc Starts project handler process.
%% Used only by `perforator_ci_project_sup`.
-spec start_link(perforator_ci_types:project_id()) -> term().
start_link(ProjectID) ->
    gen_server:start_link(?MODULE, [ProjectID], []).

%% @doc Returns project handler pid.
%% @throws {project_process_not_found, ProjectID}.
-spec get_pid(perforator_ci_types:project_id()) -> pid().
get_pid(ProjectID) ->
    try
        gproc:lookup_pid({n, l, ProjectID})
    catch
        error:badarg -> % not so imformative exception...
            throw({project_process_not_found, ProjectID})
    end.

%% @doc Checks if project handler is alive.
-spec is_project_running(perforator_ci_types:project_id()) -> boolean().
is_project_running(ProjectID) ->
    try
        get_pid(ProjectID),
        true
    catch
        throw:{project_process_not_found, ProjectID} -> false
    end.

%% @doc Informs projects about finished build (callee is builder process).
%% Parameter Success indicates if build was built, because built instructions
%% can break a build.
%% Pid is a pid of project handler.
%% Results is opaque which is interpreted by DB module and WEB.
%% If build is failed, Results contains either a tuple term() or
%% {Error, Details} which can be parsed.
-spec build_finished(pid(), perforator_ci_types:build_id(), term(),
        boolean()) -> ok.
build_finished(Pid, BuildID, Results, Success) ->
    gen_server:call(Pid, {build_finished, BuildID, Results, Success}).

%% @doc Asks project handler to ask builder to do a build run.
-spec build_now(perforator_ci_types:project_id()) -> ok.
build_now(ProjectID) ->
    gen_server:cast(get_pid(ProjectID), build_now).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

%% @todo Check if it doesn't take way toooo long to init. Maybe do async init.
init([ProjectID]) ->
    try
        % Register process name
        true = gproc:reg({n, l, ProjectID}),

        % Restore state data (it's safe, because we always create #project
        % entry before starting its handler, see
        % perforator_ci:create_and_start_project/1.
        #project{repo_url=RepoUrl, branch=Branch, repo_backend=RepoBackend,
            polling=Polling}=Project = perforator_ci_db:get_project(ProjectID),

        State0 = #state{
            project_id = ProjectID,
            repo_url = RepoUrl,
            branch = Branch,
            repo_backend = RepoBackend,
            polling = Polling
        },

        % If projects has some builds, update the values bellow:
        State1 =
            case perforator_ci_db:get_last_build(ProjectID) of
                #project_build{id=BID, commit_id=CID} ->
                    State0#state{
                        last_build_id = BID,
                        last_commit_id = CID
                    };
                undefined -> State0 % nothing has been built
            end,

        % Ask to build unfinished builds:
        [ok = perforator_ci_builder:build(Project, B) ||
            B <- perforator_ci_db:get_unfinished_builds(ProjectID)],

        % Set timer for polling (if needed)
        ok = start_timer(State1),

        {ok, State1}
    catch
        error:badarg -> % Most likely process already started, gproc #2
            {stop, project_already_started}
    end.

%% Receives notification from builder, so have to save build results and
%% broadcast 'build_finished' event:
handle_call({build_finished, BuildID, Results, Success}, _,
        #state{project_id=ID}=State) ->
    ok = perforator_ci_db:finish_build(BuildID, Results, Success),

    lager:info("Project(~p): build ~p finished~n", [ID, BuildID]),

    ok = perforator_ci_pubsub:broadcast(perforator_ci_project,
        {build_finished,
            {ID, BuildID, Success, perforator_ci_utils:timestamp()}}),

    {reply, ok, State};

handle_call(_, _, State) ->
    {reply, ok, State}.

%% Someone asked to build specific commit, so send request to a builder.
%% Also store build (not finished), just in a case, ifproject process crashes.
handle_cast({build, CommitID}, #state{project_id=ID}=State) ->
    lager:info("Project(~p): requests to build commit ~p~n",
        [ID, CommitID]),

    TS = perforator_ci_utils:timestamp(),
    #project_build{id=BuildID}=Build = perforator_ci_db:create_build({ID, TS,
        CommitID, []}),
    Project = perforator_ci_db:get_project(ID), % @todo Restore from #state
    % Store to DB:
    ok = perforator_ci_pubsub:broadcast(perforator_ci_project,
        {build_init, {ID, BuildID, CommitID, TS}}),
    % Create job for a builder
    ok = perforator_ci_builder:build(Project, Build),
    % Update last commit and build values:
    {noreply, State#state{last_commit_id=CommitID, last_build_id=BuildID}};

%% Build now request:
handle_cast(build_now, 
    #state{project_id=ID, repo_backend=Mod, branch=B,
            last_commit_id=CID}=State) ->
    CID1 = case Mod:check_for_updates(
            perforator_ci_utils:repo_path(ID), B, CID) of
        undefined -> CID ;
        NewCID when is_binary(NewCID) -> NewCID
    end,

    gen_server:cast(self(), {build, CID1}),

    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

% Timer ping, check if there is any new commit:
handle_info(ping,
        #state{project_id=ID, repo_backend=Mod, branch=B,
            last_commit_id=CID}=State) ->
    case Mod:check_for_updates(perforator_ci_utils:repo_path(ID), B, CID) of
        undefined -> ok; % do nothing
        NewCID when is_binary(NewCID) ->
            gen_server:cast(self(), {build, NewCID})
    end,

    ok = start_timer(State),

    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%% =============================================================================
%% Helpers
%% =============================================================================

%% @doc Start timer for polling (if needed, i.e. if polling strategy is not
%% 'on_demand').
-spec start_timer(#state{}) -> ok.
start_timer(#state{polling=on_demand}) -> ok;

start_timer(#state{polling={time, After}}) ->
    erlang:send_after(After, self(), ping),

    ok.
