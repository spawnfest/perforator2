%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_git_tests).

-include_lib("eunit/include/eunit.hrl").

-include("perforator_ci.hrl").

-compile(export_all).

-define(REPO, "test.git").
-define(REPOS, "repos").


%% ============================================================================

git_test_() ->
    {foreach, 
        fun () ->
            perforator_ci_utils:sh(?FMT("rm -rf ~p", [?REPO])),
            perforator_ci_utils:sh(?FMT("rm -rf ~p", [?REPOS])),

            perforator_ci_utils:sh(?FMT("git init ~p", [?REPO])),
            perforator_ci_utils:sh(?FMT("mkdir ~p", [?REPOS]))
        end,
        fun (_) ->  ok end,
        [
            {"Clone / check for updates", fun test_workflow/0}
        ]
    }.

%% ============================================================================

test_workflow() ->
    ?assertThrow(
        {unable_to_clone, _},
        perforator_ci_git:clone("fail", "fail")
    ),

    Repo = "omg.git",
    ?assertEqual(
        ok, perforator_ci_git:clone(?REPO, Repo)
    ),

    ?assertEqual(
        undefined,
        perforator_ci_git:check_for_updates(Repo, "origin/master", undefined)
    ),

    perforator_ci_utils:sh("echo \"foobar\" > f.txt", [{cd, ?REPO}]),
    perforator_ci_utils:sh("git add f.txt", [{cd, ?REPO}]),
    perforator_ci_utils:sh("git commit -m \"m\"", [{cd, ?REPO}]),

    C1 = perforator_ci_git:check_for_updates(Repo, "origin/master", undefined),

    ?assert(is_binary(C1)),

    ?assertEqual(
        undefined,
        perforator_ci_git:check_for_updates(Repo, "origin/master", C1)
    ),

    perforator_ci_utils:sh("echo \"foobar1\" > f.txt", [{cd, ?REPO}]),
    perforator_ci_utils:sh("git commit -am \"y\"", [{cd, ?REPO}]),

    C2 = perforator_ci_git:check_for_updates(Repo, "origin/master", C1),

    ?assert(is_binary(C2)),
    ?assertNot(C1 == C2).
