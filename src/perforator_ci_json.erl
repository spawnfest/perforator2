%% @doc JSON intermediate format (jiffy) (de)serializer to Erlang terms.

%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_json).

-include("perforator_ci.hrl").

-export([
    from/2,
    to/2
]).

%% ============================================================================
%% From jiffy intermediate to Erlang term()
%% ============================================================================

from(project_new, {Data}) ->
    Polling =
        case proplists:get_value(<<"polling_strategy">>, Data) of
            <<"ondemand">> -> on_demand;
            {[{<<"time">>, T}]} -> {time, T}
        end,
    BuildInstr = [binary_to_list(I) || I <-
        proplists:get_value(<<"build_instructions">>, Data)],

    {
        proplists:get_value(<<"name">>, Data),
        binary_to_list(proplists:get_value(<<"repo_url">>, Data)),
        binary_to_list(proplists:get_value(<<"branch">>, Data)),
        perforator_ci_git, % @todo clean dirty hack
        Polling,
        BuildInstr,
        []
    };

from(project_update, {Data}) ->
    list_to_tuple(
        [proplists:get_value(<<"id">>, Data) |
            tuple_to_list(from(project_new, {Data}))]
    );

from(project, ProjectID) ->
    ProjectID;

from(builds, ProjectID) ->
    ProjectID;

from(build_now, ProjectID) ->
    ProjectID;

from(build, ProjectID) ->
    ProjectID;

from(previous_build, BuildID) ->
    BuildID.


%% ============================================================================
%% To jiffy intermediate from Erlang term()
%% ============================================================================

to(project_new, ProjectID) ->
    ProjectID;

to(project_update, _) ->
    null;

to(project, #project{id=ID, name=Name, repo_url=RepoURL, branch=Branch,
        polling=Polling, build_instructions=BuildInstr}) ->
    Polling1 = case Polling of
        on_demand -> ?BIN(ondemand);
        {time, N} -> {[{time, N}]}
    end,
    BuildInstr1 = [?BIN(I) || I <- BuildInstr],

    {[
        {id, ID},
        {name, ?BIN(Name)},
        {repo_url, ?BIN(RepoURL)},
        {branch, ?BIN(Branch)},
        {polling_strategy, Polling1},
        {build_instructions, BuildInstr1}
    ]};

to(projects, Projects) ->
    [to(project, P) || P <- Projects];

to(builders, Builders) ->
    [{[{name, ?BIN(N)}, {queue_size, Q}]} || {N, Q} <- Builders];

to(build_init, {ProjectID, BuildID, CommitID, TS}) ->
    {[
        {project_id, ProjectID},
        {build_id, BuildID},
        {commit_id, CommitID},
        {timestamp, TS}
    ]};

to(build_finished, {ProjectID, BuildID, Success, TS}) ->
    {[
        {project_id, ProjectID},
        {build_id, BuildID},
        {success, Success},
        {timestamp, TS}
    ]};

to(queue_size, {Node, Size}) ->
    {[
        {name, ?BIN(Node)},
        {queue_size, Size}
    ]};

to(builds, Builds) ->
    lists:map(
        fun (#project_build{id=ID, finished=Fin,
                timestamp=TS, info=Info, commit_id=CID}) ->
            {Finished, Success} =
                case Fin of
                    failure -> {true, false};
                    true -> {true, true};
                    false -> {false, false}
                end,

            Info1 =
                if
                    is_list(Info) ->
                        proplists:get_value(totals, Info, []);
                    true ->
                        []
                end,

            {[
                {id, ID},
                {succeeded, Success},
                {finished, Finished},
                {started, TS},
                {time, proplists:get_value(duration, Info1, 0)},
                {modules, proplists:get_value(suite_count, Info1, 0)},
                {tests, proplists:get_value(test_count, Info1, 0)},
                {commit_id, CID}
            ]}
        end,
        Builds);

to(build, #project_build{finished=failure, info=Info}) ->
    throw(Info);
to(build, #project_build{info=TestInfo}) ->
    Suites = proplists:get_value(suites, TestInfo),
    lists:map(fun ({SuiteName, SuiteData}) ->
        TestCases = proplists:get_value(test_cases, SuiteData),
        {[
            {name, SuiteName},
            {test_cases, lists:map(fun ({CaseName, CaseData}) ->
                    force_objects([{name, CaseName}|
                        proplists:delete(runs, CaseData)])
                end,
                TestCases)}
        ]}
    end, Suites);

to(previous_build, Data) ->
    case Data of
        undefined -> null;
        N when is_integer(N) -> N
    end;

to(build_now, _) -> null.

%% ============================================================================
%% Helpers
%% ============================================================================

%% @hack: We know that all our test data is proplists, so we force it into JSON
%% objects for Jiffy to be happy.
force_objects({Key, Val}) ->
    {Key, force_objects(Val)};
force_objects(PropList) when is_list(PropList) ->
    {[force_objects(Elem) || Elem <- PropList]};
force_objects(A) ->
    A.
