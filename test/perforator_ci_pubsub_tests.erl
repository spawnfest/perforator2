%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_pubsub_tests).

-include_lib("eunit/include/eunit.hrl").

-include("perforator_ci.hrl").

-compile(export_all).

%% ============================================================================

builder_test_() ->
    {foreach, 
        fun () ->
            perforator_ci:init(),
            perforator_ci:start()
        end,
        fun (_) ->
            perforator_ci:stop()
        end,
        [
            {"Pubsub workflow", fun test_workflow/0}
        ]
    }.

%% ============================================================================

test_workflow() ->
    ?assertEqual(ok, perforator_ci_pubsub:subscribe(perforator_ci_project)),

    ?assertEqual(ok,
        perforator_ci_pubsub:broadcast(perforator_ci_project, {omg, test})),

    receive
        {perforator_ci_event, perforator_ci_project, {omg, test}} ->
            ?assert(true)
    end.
