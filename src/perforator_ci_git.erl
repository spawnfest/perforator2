%% @doc Simple GIT client.
%%
%% WARNING: all calls are not safe!
%% @todo Add behaviour

%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_git).

-include("perforator_ci.hrl").

-export([
    clone/2
]).

%% ============================================================================

%% @todo Escape input
-spec clone(list(), list()) -> ok.
clone(RepoURL, Path) ->
    perforator_ci_utils:sh(?FMT("rm -rf ~p", [Path])), % dirty hack
    perforator_ci_utils:sh(?FMT("git clone ~p ~p", [RepoURL, Path])),
    ok.

check_for_updates() -> ok.
