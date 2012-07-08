%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_builder_tests).

-include_lib("eunit/include/eunit.hrl").

-include("perforator_ci.hrl").

-compile(export_all).

-define(REPO, "test.git").
-define(REPOS, "repos").
-define(BUILD_REPOS, "build_repos").


%% ============================================================================

builder_test_() ->
    {foreach, 
        fun () ->
            perforator_ci:init(),
            perforator_ci:start(),

            ok = meck:new(perforator_ci_project, [no_link, passthrough]),
            ok = meck:new(perforator_ci_builder, [no_link, passthrough])
        end,
        fun (_) ->
            timer:sleep(50),
            ok = meck:unload([perforator_ci_builder, perforator_ci_project]),

            perforator_ci:stop()
        end,
        [
            {"Mocked build workflow", fun test_workflow/0},
            {"Callee crashes", fun test_callee_crash/0},
            {"Worker crash", fun test_worker_crash/0},
            {"Real workflow", fun test_real_workflow/0}
        ]
    }.

%% ============================================================================

% normal flow, no one crashes, callee gets response:
test_workflow() ->
    meck:expect(perforator_ci_project, build_finished,
        fun (Pid, BuildID, Results, Success) ->
            Pid ! {build_finished, BuildID, Results, Success},
            ok
        end),

    meck:expect(perforator_ci_builder, run_build, fun (_) -> result end),

    perforator_ci_builder:build(#project{}, #project_build{id=4}),
    receive
        M ->
            ?assertEqual(M, {build_finished, 4, result, true})
    end.

% let it crash: callee crashes, all its items are deleted
test_callee_crash() ->
    meck:expect(perforator_ci_builder, run_build,
        fun (_) -> timer:sleep(3000), result_1 end),

    Pid = spawn(
        fun () ->
            perforator_ci_builder:build(#project{}, #project_build{id=5}),
            perforator_ci_builder:build(#project{}, #project_build{id=6}),
            receive _ -> ok end
        end),

    Pid1 = spawn(
        fun() ->
            perforator_ci_builder:build(#project{}, #project_build{id=7}),
            receive _ -> ok end
        end
    ),

    timer:sleep(1000),

    erlang:exit(Pid, die),
    timer:sleep(50),

    ?assertEqual(
        [{Pid1, #project{}, #project_build{id=7}}],
        perforator_ci_builder:get_queue()
    ).

% let worker crash: worker is restarted
test_worker_crash() ->
    meck:expect(perforator_ci_builder, run_build,
        fun (_) -> timer:sleep(500), a+b end),

    perforator_ci_builder:build(#project{}, #project_build{id=1}),
    timer:sleep(50),
    Pid1 = perforator_ci_builder:get_worker(),
    ?silent(alert, timer:sleep(600)),
    Pid2 = perforator_ci_builder:get_worker(),

    ?assert(is_pid(Pid1)),
    ?assert(is_pid(Pid2)),
    ?assertNot(Pid1 =:= Pid2),

    ?assertEqual(
        [{self(), #project{}, #project_build{id=1}}],
        perforator_ci_builder:get_queue()
    ).

enqueue_test() ->
    Item = {o, m, #project_build{id=1}},
    Q0 = perforator_ci_builder:enqueue(Item, []),

    ?assertEqual([Item], Q0),

    Q1 = perforator_ci_builder:enqueue(Item, Q0),

    ?assertEqual(Q0, Q1).

% Probably the whole CI integration test.
test_real_workflow() ->
    application:load(perforator_ci),
    application:set_env(perforator_ci, repo_path, ?REPOS),
    application:set_env(perforator_ci, builder_repos_path, ?BUILD_REPOS),

    % @todo Clean
    perforator_ci_utils:sh(?FMT("rm -rf ~p", [?REPOS])),
    perforator_ci_utils:sh(?FMT("rm -rf ~p", [?REPO])),
    perforator_ci_utils:sh(?FMT("rm -rf ~p", [?BUILD_REPOS])),
    perforator_ci_utils:sh(?FMT("git init ~p", [?REPO])),
    perforator_ci_utils:sh(?FMT("mkdir ~p", [?BUILD_REPOS])),
    perforator_ci_utils:sh(?FMT("mkdir ~p", [?REPOS])),
    perforator_ci_utils:sh("echo \"o\" > t.txt", [{cd, ?REPO}]),
    perforator_ci_utils:sh("git add t.txt", [{cd, ?REPO}]),
    perforator_ci_utils:sh("git commit -am \"o\"", [{cd, ?REPO}]),

    ok = perforator_ci_pubsub:subscribe(perforator_ci_project),
    ok = perforator_ci_pubsub:subscribe(perforator_ci_builder),

    1 = perforator_ci:create_and_start_project({<<"1">>, ?REPO, "origin/master",
        perforator_ci_git, {time, 50}, ["echo omg"], []}),
    timer:sleep(200),

    receive M0 ->
        ?assertMatch({perforator_ci_event, perforator_ci_project,
            {build_init, {1, 1, _, _}}}, M0)
    end,

    receive M1 ->
        ?assertMatch({perforator_ci_event, perforator_ci_project,
        {build_finished, {1, 1, true, _}}}, M1)
    end,

    ?assertMatch(
        #project_build{id=1, finished=true, info=ok},
        perforator_ci_db:get_build(1)),

    2 = perforator_ci:create_and_start_project({<<"2">>, ?REPO, "origin/master",
        perforator_ci_git, {time, 50}, ["fail fail fail"], []}),
    timer:sleep(300),

    ?assertMatch(
        #project_build{id=2, finished=failure, info={_, _}},
        perforator_ci_db:get_build(2)).
