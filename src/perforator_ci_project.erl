%% @doc Project worker responsible for project repo polling, asking for a
%% build.

%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_project).

-behaviour(perforator_ci_project).

%% API
-export([
    start_link/3
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

-type polling_strategy() :: {time, integer()} | on_demand.

-record(state, {
    project_id :: perforator_ci_types:project_id(),
    repo :: binary(), % remote repo url
    repo_backend=perforator_ci_git :: atom(), % maybe add CVS support one day
    polling=on_demand :: polling_strategy(),
    last_build_id=0 :: perforator_ci_types:build_id(),
    last_commit_id= <<"undef">> :: perorator_ci_types:commit_id()
}).

-ifdef(TEST).
-compile(export_all).
-endif.

%% ============================================================================
%% API
%% ============================================================================

%% @doc Starts project.
-spec start_link(perforator_ci_types:project_id(), binary(),
        polling_strategy()) -> {ok, pid()}.
start_link(ProjectID, Repo, Polling) ->
    gen_server:start_link(?MODULE, [ProjectID, Repo, Polling], []).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init([ProjectID, Repo, Polling]) ->
    {ok, #state{}}.

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
