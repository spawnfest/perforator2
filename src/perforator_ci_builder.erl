%% @doc Projects builder. Only one per app because of obvious reasons (would be
%% non-sense to share some system resources between performance tests).

%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_builder).

-behaviour(gen_server).

-include("perforator_ci.hrl").

%% API
-export([
    start_link/0,
    build/2,
    get_queue_size/1,
    get_builders/0,

    run_build/1
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

-type queue_item() :: {pid(), #project{}, #project_build{}}.

-record(state, {
    build_queue=[] :: [queue_item()],
    worker :: pid() | undefined % pid of process responsible for handling build % undefined means that currently there is no job running
}).

-ifdef(TEST).
-compile(export_all).
-endif.

-define(SERV_NAME, {global, {perforator_ci_builder, node()}}).
-define(BUILD_REQUEST_TIMEOUT, 15000).

%% ============================================================================
%% API
%% ============================================================================

%% @doc Starts builder.
%% Builder name is global and a form of {perforator_ci_builder, node()}.
-spec start_link() -> {ok, term()}.
start_link() ->
    gen_server:start_link(?SERV_NAME, ?MODULE, [], []).

%% @doc Adds build request (which will be eventually handled) to builder queue.
-spec build(#project{}, #project_build{}) -> ok.
build(Project, Build) ->
    Builder = pick_builder(),
    true = link(global:whereis_name(Builder)), % link for request,
    % all persistent part is built at perforator_ci_project side
    gen_server:call({global, Builder},
        {build, Project, Build}, ?BUILD_REQUEST_TIMEOUT).

%% @doc Used for testing.
get_queue() ->
    gen_server:call(?SERV_NAME, get_queue).

%% @doc Used for testing.
get_worker() ->
    gen_server:call(?SERV_NAME, get_worker).

%% @doc Returns builder queue size.
get_queue_size(Node) ->
    length(gen_server:call({global, {perforator_ci_builder, Node}}, get_queue)).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init([]) ->
    process_flag(trap_exit, true), % we don't want to let a process die like
    % the rest of the world (perforator_ci_project that links to
    % perforator_ci_builder before doing request).

    {ok, #state{}}.

handle_call(get_queue, _, #state{build_queue=Q}=S) ->
    {reply, Q, S};

handle_call(get_worker, _, #state{worker=P}=S) ->
    {reply, P, S};

handle_call({build, Project, Build}, {Pid, _},
        #state{build_queue=Q}=State) ->
    Q1 = enqueue({Pid, Project, Build}, Q),

    gen_server:cast(self(), ping), % ping!

    {reply, ok, State#state{build_queue=Q1}};

handle_call(_, _, State) ->
    {reply, ok, State}.

% empty queue, do nothing
handle_cast(ping, #state{build_queue=[]}) ->
    {noreply, #state{}};

% busy, do nothing
handle_cast(ping, #state{worker=P}=S) when is_pid(P) ->
    {noreply, S};

% build!
handle_cast(ping, #state{build_queue=[Item|_]}=S) ->
    Pid = spawn_link(
        fun() ->
            {Pid, _, #project_build{id=BuildID}} = Item,
            {Success, Result} =
                try
                    {true, ?MODULE:run_build(Item)}
                catch
                    throw:R ->
                        {false, R}
                end,

            % inform
            ok = perforator_ci_project:build_finished(Pid, BuildID, Result,
                Success),
            % exit
            exit('$work_is_done')
        end
    ),

    {noreply, S#state{worker=Pid}};

handle_cast(_, State) ->
    {noreply, State}.

% worker done!
handle_info({'EXIT', Pid, '$work_is_done'},
        #state{worker=Pid, build_queue=[_|T]}) ->
    ok = perforator_ci_pubsub:broadcast(perforator_ci_builder,
        {queue_size, {node(), length(T)}}),
    gen_server:cast(self(), ping),

    {noreply, #state{build_queue=T}};

% worker died ;-( restart it
handle_info({'EXIT', Pid, _}, #state{worker=Pid, build_queue=Q}) ->
    gen_server:cast(self(), ping),

    {noreply, #state{build_queue=Q}};

% project died, delete all its queue items
handle_info({'EXIT', Pid, _}, #state{build_queue=Q, worker=P}) ->
    % kill current worker if its handling died project build:
    P1 = case Q of
        [{Pid, _, _}|_] ->
            if
                is_pid(P) ->
                    erlang:exit(P, '$project_died'),
                    undefined;
                true ->
                    undefined
            end;
        _ -> P
    end,

    % filter out queue items
    Q1 = [Item || {Pid0, _, _}=Item <- Q, Pid0 =/= Pid],

    ok = perforator_ci_pubsub:broadcast(perforator_ci_builder,
        {queue_size, {node(), length(Q1)}}),

    {noreply, #state{build_queue=Q1, worker=P1}};

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%% =============================================================================
%% Helpers
%% =============================================================================

%% @doc Picks randomly a builder.
pick_builder() ->
    Builders = [B ||
        {perforator_ci_builder, _X}=B <- global:registered_names()],

    lists:nth(random:uniform(length(Builders)), Builders).

%% @doc Returns nodes that run builders.
get_builders() ->
    [B || {perforator_ci_builder, B} <- global:registered_names()].

%% @doc Adds item to an queue if item with given build id doesn't exist.
-spec enqueue(queue_item(), [queue_item()]) -> [queue_item()].
enqueue({_, _, #project_build{id=BuildID}}=Item, Q) ->
    NotExists = lists:all(
        fun
            ({_, _, #project_build{id=BID}}) when BID =:= BuildID -> false;
            (_) -> true
        end,
        Q
    ),
    
    if
        NotExists -> Q ++ [Item];
        true -> Q
    end.

%% @doc Run a job.
run_build({_Pid, #project{id=ProjectID, repo_url=RepoUrl,
        repo_backend=Mod, build_instructions=Instructions},
        #project_build{commit_id=CommitID}}) ->
    % Check if repo exist. If not, clone it
    RepoDir = repo_path(ProjectID),
    case filelib:is_dir(RepoDir) of
        true -> ok;
        false ->
            ok = Mod:clone(RepoUrl, RepoDir)
    end,

    % Fetch and checkout
    ok = Mod:checkout(RepoDir, CommitID),

    lists:foreach(
        fun (C) ->
            try
                perforator_ci_utils:sh(C, [{cd, RepoDir}])
            catch
                throw:{exec_error, {_, _, Reason}} ->
                    throw({C, Reason})
            end
        end,
        Instructions
    ),

    case perforator_ci_results:read(RepoDir) of
        [] -> % @todo Clean:
            throw(perf_test_not_found);
        Res -> Res
    end.

%% @doc Returns builder repo_path.
%% @todo DRY
repo_path(ProjectID) ->
    filename:join(
        perforator_ci_utils:get_env(perforator_ci,
            builder_repos_path, ?BUILDER_REPOS_DIR),
        integer_to_list(ProjectID)
    ).
