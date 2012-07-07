%% @doc Various stuff.

%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_utils).

-include("perforator_ci.hrl").

-export([
    timestamp/0,
    get_env/3,
    sh/1,
    sh/2,
    repo_path/1
]).

%% ============================================================================
%% API
%% ============================================================================

-spec timestamp() -> perforator_ci_types:timestamp().
timestamp() ->
    {Mega, Sec, Micro} = erlang:now(),
    (Mega * 1000000 + Sec) * 1000000 + Micro.

%% @doc Returns application env variable or default unless it exists.
get_env(App, Key, Default) ->
    case application:get_env(App, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

%% @doc Exec given command.
%% @throws {exec_error, {Command, ErrCode, Output}}.
-spec sh(list(), list()) -> list().
sh(Command, Opts0) ->
    Port = open_port({spawn, Command}, Opts0 ++ [
        exit_status, {line, 255}, stderr_to_stdout
    ]),


    case sh_receive_loop(Port, []) of
        {ok, Data} -> Data;
        {error, {ErrCode, Output}} ->
            throw({exec_error, {Command, ErrCode, Output}})
    end.

sh(Command) ->
    sh(Command, []).

sh_receive_loop(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            sh_receive_loop(Port, [Line ++ "\n"|Acc]);
        {Port, {data, {noeol, Line}}} ->
            sh_receive_loop(Port, [Line|Acc]);
        {Port, {exit_status, 0}} ->
            {ok, lists:flatten(lists:reverse(Acc))};
        {Port, {exit_status, E}} ->
            {error, {E, lists:flatten(lists:reverse(Acc))}}
    end.

%% @doc Returns project repository path
repo_path(ProjectID) ->
    filename:join(
        perforator_ci_utils:get_env(perforator_ci, repos_path, ?REPOS_DIR),
        integer_to_list(ProjectID)
    ).
