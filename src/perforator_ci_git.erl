%% @doc Simple GIT client.

%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_git).

-include("perforator_ci.hrl").

-export([
    clone/2
]).

%% ============================================================================

%% @todo Fix for non bare repos
clone(RepoURL, Path) ->
    perforator_ci_utils:sh(?FMT("rm -rf ~p", [Path])), % dirty hack
    perforator_ci_utils:sh(?FMT("git clone ~p ~p", [RepoURL, Path])).
