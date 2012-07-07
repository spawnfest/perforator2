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
            {"Start project/recovery", fun test_start_project/0}
        ]
    }.

%% ============================================================================

test_start_project() ->
    1 = perforator_ci:create_and_start_project(<<"a">>, <<"b">>, on_demand),
    1 = perforator_ci:create_and_start_project(<<"a">>, <<"b">>, on_demand),

    ?assertMatch(
        [_], % exactly one child is started
        supervisor:which_children(perforator_ci_project_sup)
    ),

    ?silent(error,
        begin
            application:stop(perforator_ci),
            application:start(perforator_ci),
            timer:sleep(50)
        end),

    ?assertMatch(
        [_], % the same child is up
        supervisor:which_children(perforator_ci_project_sup)
    ),
    ?assert(perforator_ci_project:is_project_running(1)).
