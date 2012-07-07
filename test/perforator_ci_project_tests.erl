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
            {"Start project/recovery", fun test_start_project/0},
            {"Ping -> build", fun test_ping_and_build/0}
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

test_ping_and_build() ->
    ok = meck:new(perforator_ci_git, [no_link, passthrough]),
    meck:expect(perforator_ci_git, check_for_updates,
        fun
            (_, <<"random_commit_id">>) -> undefined;
            (_, _) -> <<"random_commit_id">>
        end),
    ok = meck:new(perforator_ci_builder, [no_link, passthrough]),
    ok = meck:expect(perforator_ci_builder, build, 3, ok),

    1 = perforator_ci:create_and_start_project(<<"omg">>, <<"rep">>,
        {time, 50}),
    timer:sleep(100),

    ?assert(meck:validate(perforator_ci_git)),
    ?assert(meck:validate(perforator_ci_builder)),

    ?assertMatch(
        [#project_build{id=1, commit_id= <<"random_commit_id">>}],
        perforator_ci_db:get_unfinished_builds(1)
    ),

    ok = gen_server:call(perforator_ci_project:get_pid(1),
        {build_finished, 1, [omg]}),
    timer:sleep(50),

    ?assertMatch([], perforator_ci_db:get_unfinished_builds(1)),

    ok = meck:unload([perforator_ci_builder, perforator_ci_git]).
