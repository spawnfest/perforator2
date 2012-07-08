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
