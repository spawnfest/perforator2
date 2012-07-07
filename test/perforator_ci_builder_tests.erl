%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_builder_tests).

-include_lib("eunit/include/eunit.hrl").

-include("perforator_ci.hrl").

-compile(export_all).

-define(REPO, "test.git").
-define(REPOS, "repos").

%% ============================================================================

project_test_() ->
    {foreach, 
        fun () ->
            application:load(perforator_ci),
            application:set_env(perforator_ci, repo_path, ?REPOS),

            perforator_ci_utils:sh(?FMT("rm -rf ~p", [?REPO])),
            perforator_ci_utils:sh(?FMT("rm -rf ~p", [?REPOS])),

            perforator_ci_utils:sh(?FMT("git init ~p", [?REPO])),
            perforator_ci_utils:sh(?FMT("mkdir ~p", [?REPOS])),

            perforator_ci:init(),
            perforator_ci:start()
        end,
        fun (_) ->
            perforator_ci:stop()
        end,
        [
        ]
    }.

%% ============================================================================

