%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_json_tests).

-include_lib("eunit/include/eunit.hrl").

-include("perforator_ci.hrl").

-compile(export_all).

-define(FROM(Type, Data), perforator_ci_json:from(Type, Data)).
-define(DEC(JSON), jiffy:decode(JSON)).

%% ============================================================================

from_project_new_test() ->
    JSON = <<"
        {
            \"name\": \"name\",
            \"branch\" : \"branch\",
            \"repo_url\" : \"url\",
            \"build_instructions\" : [ \"one\", \"two\" ],
            \"polling_strategy\": {\"time\": 10}
        }
    ">>,

    ?assertEqual(
        {<<"name">>, "url", "branch", perforator_ci_git, {time, 10},
            ["one", "two"], []},
        ?FROM(project_new, ?DEC(JSON))).

from_project_update_test() ->
    JSON = <<"
        {
            \"id\": \"id\",
            \"name\": \"name\",
            \"branch\" : \"branch\",
            \"repo_url\" : \"url\",
            \"build_instructions\" : [ \"one\", \"two\" ],
            \"polling_strategy\": \"ondemand\"
        }
    ">>,

    ?assertEqual(
        {<<"id">>, <<"name">>, "url", "branch", perforator_ci_git, on_demand,
            ["one", "two"], []},
        ?FROM(project_update, ?DEC(JSON))).

to_build_test() ->
    Data = [{suites, [{<<"test_suite_1">>, [{test_cases,
        [{<<"test_case_1">>, [
            {successful, true},
            {result, [
                {failures, 1},
                {duration, [
                    {min, 3},
                    {max, 4},
                    {mean, 6}
                ]}
            ]}
        ]}]
    }]}]}],
    JSON = perforator_ci_json:to(build, #project_build{info=Data}),
    _Enc = jiffy:encode(JSON). %% it happens -- good enough.

to_test_runs_test() ->
    Data = [{1,[{failures,1},{duration,[{min,3},{max,4},{mean,6}]}]}],
    JSON = perforator_ci_json:to(test_runs, Data),
    _Enc = jiffy:encode(JSON). %% it happens -- good enough.

