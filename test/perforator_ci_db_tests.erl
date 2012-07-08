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
            {"Create/update project", fun test_create_project/0},
            {"Create build", fun test_create_build/0}
        ]
    }.

%% ============================================================================

test_create_project() ->
    ?assertEqual(
        1,
        perforator_ci_db:create_project({
            <<"1">>, "r", "b", git, on_demand, [], []})
    ),
    ?assertEqual(
        1,
        perforator_ci_db:create_project({
            <<"1">>, "r", "b", git, on_demand, [], []})
    ),
    ?assertEqual(
        2,
        perforator_ci_db:create_project({
            <<"2">>, "r", "b", git, on_demand, [], []})
    ),
    ?assertMatch(
        #project{id=1, name= <<"1">>,
            repo_url="r", branch="b", repo_backend=git,
            polling=on_demand, build_instructions=[], info=[]},
        perforator_ci_db:get_project(1)
    ),
    ?assertMatch(
        [#project{id=1}, #project{id=2}],
        perforator_ci_db:get_projects()),

    ?assertEqual(
        ok,
        perforator_ci_db:update_project({
            1, <<"1">>, "omg", "b", git, on_demand, [], []})
    ),
    ?assertMatch(
        #project{id=1, repo_url="omg"},
        perforator_ci_db:get_project(1)
    ).

test_create_build() ->
    ?assertMatch(
        #project_build{id=1, local_id=1},
        perforator_ci_db:create_build({42, 123, <<"cid0">>, []})),
    ?assertMatch(
        #project_build{id=1, local_id=1},
        perforator_ci_db:create_build({42, 123, <<"cid0">>, []})),
    ?assertMatch(
        #project_build{id=2, local_id=2},
        perforator_ci_db:create_build({42, 123, <<"cid1">>, []})),
    ?assertMatch(
        #project_build{id=3, local_id=1},
        perforator_ci_db:create_build({666, 123, <<"cid0">>, []})),
    ?assertMatch(
        #project_build{id=4, local_id=2},
        perforator_ci_db:create_build({666, 123, <<"cid3">>, []})),

    ?assertMatch(
        #project_build{commit_id= <<"cid1">>},
        perforator_ci_db:get_last_build(42)),
    ?assertMatch(
        [#project_build{id=3, local_id=1}, #project_build{id=4, local_id=2}],
        perforator_ci_db:get_unfinished_builds(666)),

    ?assertEqual(ok, perforator_ci_db:finish_build(3, [omg], false)),
    ?assertMatch(
        #project_build{id=3, local_id=1, finished=failure, info=[omg]},
        perforator_ci_db:get_build(3)),
    ?assertMatch(
        [#project_build{id=4}],
        perforator_ci_db:get_unfinished_builds(666)).
