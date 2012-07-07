%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_project_tests).

-include_lib("eunit/include/eunit.hrl").

-include("perforator_ci.hrl").

-compile(export_all).

%% ============================================================================

project_test_() ->
    {foreach, 
        fun () ->
            perforator_ci:init(),
            perforator_ci:start()
        end,
        fun (_) -> perforator_ci:stop() end,
        [
            {"Create project", fun test_create_project/0}
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
    ).
