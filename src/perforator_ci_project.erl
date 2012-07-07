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

    repo_url :: perforator_ci_types:repo_url(),
    branch :: perforator_ci_types:branch(),
    repo_backend :: perforator_ci_types:repo_backend(),

    polling=on_demand :: perforator_ci_types:polling_strategy(),

    last_build_id=0 :: perforator_ci_types:build_id(),
    last_commit_id=undefined :: perforator_ci_types:commit_id()
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
        gproc:lookup_pid({n, l, ProjectID}),
        true
    catch
        error:badarg -> % gproc shame #2
            false
    end.

%% @doc Starts project.
-spec start_link(perforator_ci_types:project_id()) -> term().
start_link(ProjectID) ->
    gen_server:start_link(?MODULE, [ProjectID], []).

get_pid(ProjectID) ->
    gproc:lookup_pid({n, l, ProjectID}).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

%% @todo Check if it doesn't take way toooo long to init. Maybe do async init.
init([ProjectID]) ->
    try
        % Register
        true = gproc:reg({n, l, ProjectID}),

        % Restore state data
        #project{repo_url=RepoUrl, branch=Branch, repo_backend=RepoBackend,
            polling=Polling} = perforator_ci_db:get_project(ProjectID),

        State0 = #state{
            project_id = ProjectID,
            repo_url = RepoUrl,
            branch = Branch,
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
       
        % Ask to build unfinished builds:
        [ok = perforator_ci_builder:build(ProjectID, C, B) ||
            #project_build{id=B, commit_id=C} <-
                perforator_ci_db:get_unfinished_builds(ProjectID)],

        % Set timer for polling (if needed)
        ok = start_timer(State1),

        {ok, State1}
    catch
        error:badarg -> % Most likely process already started, shame on gproc
            {stop, project_already_started}
    end.

handle_call({build_finished, BuildID, Info}, _, State) ->
    ok = perforator_ci_db:finish_build(BuildID, Info),

    % @todo pubsub

    {reply, ok, State};

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(build_now,
        #state{project_id=ID, repo_backend=Mod, last_commit_id=CID}=State) ->
    case Mod:check_for_updates(ID, CID) of
        undefined ->
            gen_server:cast({build, CID}); % rebuild old commit
        NewCommitID when is_binary(NewCommitID) ->
            gen_server:cast({build, NewCommitID})
    end,

    {noreply, State};

handle_cast({build, CommitID}, #state{project_id=ID}=State) ->
    lager:info("Project (~p): request for build commit ~p~n",
        [ID, CommitID]),
    {BuildID, _} = perforator_ci_db:create_build({ID,
        perforator_ci_utils:timestamp(), CommitID, []}),

    % @todo pubsub

    % Create job for a builder
    ok = perforator_ci_builder:build(ID, CommitID, BuildID),
    
    {noreply, State#state{last_commit_id=CommitID, last_build_id=BuildID}};

handle_cast(_, State) ->
    {noreply, State}.

handle_info(ping,
        #state{project_id=ID, repo_backend=Mod, last_commit_id=CID}=State) ->
    case Mod:check_for_updates(ID, CID) of
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

%% @doc Start timer for polling (if needed).
-spec start_timer(#state{}) -> ok.
start_timer(#state{polling=on_demand}) -> ok;

start_timer(#state{polling={time, After}}) ->
    erlang:send_after(After, self(), ping),

    ok.
