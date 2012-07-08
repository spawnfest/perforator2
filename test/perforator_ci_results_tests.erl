%% @doc Unit tests why-are-you-reading-this.
%%
%% @author Ignas <i.vysniauskas@gmail.com>

-module(perforator_ci_results_tests).

-include_lib("eunit/include/eunit.hrl").
-include("perforator_ci.hrl").

-compile(export_all).

perforator_ci_results_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            fun test_simple_single_file/1
        ]
    }.


test_simple_single_file(SetupOpts) ->
    fun () ->
        TempDir = get_temp_dir(SetupOpts),
        Results = perforator_ci_results:read(TempDir),
        ?assertEqual([[{foo, bar}]], Results)
    end.


%% ============================================================================
%% Test helpers
%% ============================================================================

setup() ->
    TestDir = "perf_test/",
    TestFile = "foobar_module_perf_results_201212.perf",
    TestData = {foo, bar},
    TestFilePath = filename:join(TestDir, TestFile),
    ok = filelib:ensure_dir(TestFilePath),
    ok = file:write_file(TestFilePath, io_lib:format("~p.~n", [TestData])),
    [{temp_dir, TestDir}, {temp_files, [TestFile]}].

cleanup(SetupOpts) ->
    ok.
    %TempDir = proplists:get_value(temp_dir, SetupOpts),
    %TempFiles = proplists:get_value(temp_files, SetupOpts),
    %lists:foreach(fun (File) ->
    %    file:delete(filename:join(TempDir, File))
    %end, TempFiles),
    %file:del_dir(TempDir).

get_temp_dir(SetupOpts) ->
    proplists:get_value(temp_dir, SetupOpts).
