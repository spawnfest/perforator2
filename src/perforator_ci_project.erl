%% @doc Project worker responsible for project repo polling, asking for a
%% build.
%%
%% WARNING: API calls can't be done from remote nodes.

%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_project).

-behaviour(gen_server).

-include("perforator_ci.hrl").

%% API
-export([
    is_project_running/1,
    start_link/1
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
    repo :: binary(), % remote repo url
    repo_backend :: atom(),
    polling=on_demand :: perforator_ci_types:polling_strategy(),
    last_build_id=0 :: perforator_ci_types:build_id(),
    last_commit_id= <<"undef">> :: perforator_ci_types:commit_id()
}).

-ifdef(TEST).
-compile(export_all).
-endif.

%% ============================================================================
%% API
%% ============================================================================

-spec is_project_running(perforator_ci_types:project_id()) -> boolean().
is_project_running(ProjectID) ->
    try
        % Register
        true = gproc:lookup_pid({n, l, ProjectID}),

        % Restore state data
        #project{repo=Repo, repo_backend=RepoBackend, polling=Polling} =
            perforator_ci_db:get_project(ProjectID),

        State0 = #state{
            project_id = ProjectID,
            repo = Repo,
            repo_backend = RepoBackend,
            polling = Polling
        },

        State1 =
            case perforator_ci_db:get_last_build(ProjectID) of
                #project_build{id=BID, commit_id=CID} ->
                    State0#state{
                        last_build_id = BID,
                        last_commit_id = CID
                    };
                undefined -> State0 % nothing has been built
            end,

        {ok, State1}
    catch
        error:badarg -> % shame on gproc #2
            false
    end.

%% @doc Starts project.
-spec start_link(perforator_ci_types:project_id()) -> term().
start_link(ProjectID) ->
    gen_server:start_link(?MODULE, [ProjectID], []).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init([ProjectID]) ->
    try
        true = gproc:reg({n, l, ProjectID}),

        {ok, #state{}}
    catch
        error:badarg -> % Most likely process already started, shame on gproc
            {stop, project_already_started}
    end.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.
