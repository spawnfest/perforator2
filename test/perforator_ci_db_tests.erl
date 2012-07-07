%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_db_tests).

-include_lib("eunit/include/eunit.hrl").

-include("perforator_ci.hrl").

-compile(export_all).

%% ============================================================================

db_test_() ->
    {foreach, 
        fun () ->
            perforator_ci:init(),
            perforator_ci:start()
        end,
        fun (_) -> perforator_ci:stop() end,
        [
            {"Create project", fun test_create_project/0},
            {"Get projects", fun test_get_projects/0}
        ]
    }.

%% ============================================================================

test_create_project() ->
    ?assertEqual(
        1,
        perforator_ci_db:create_project(<<"omg">>, <<"repo">>, on_demand)
    ),
    ?assertEqual(
        1,
        perforator_ci_db:create_project(<<"omg">>, <<"repo">>, on_demand)
    ),
    ?assertEqual(
        2,
        perforator_ci_db:create_project(<<"gmo">>, <<"repo">>, on_demand)
    ),
    ?assertMatch(
        #project{id=1, name= <<"omg">>, repo= <<"repo">>,
            polling_strategy=on_demand},
        perforator_ci_db:get_project(1)
    ).

test_get_projects(
    1 = perforator_ci_db:create_project(<<"a">>, <<"b">>, on_demand),

    ?assertMatch([#project{id=1}], perforator_ci_db:get_projects()).
