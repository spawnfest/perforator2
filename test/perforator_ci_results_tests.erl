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
        ?assertMatch([{totals, _}, {suites, [_]}], Results)
    end.


%% ============================================================================
%% Test helpers
%% ============================================================================

setup() ->
    TestDir = "perf_test/",
    TestFile = "foobar_module_perf_results_201212.perf",
    TestData = {<<"foobar_suite">>, [
        {date, 1},
        {totals, [
                {test_count, 4},
                {failure_count, 1}
            ]},
        {test_cases, [
            {<<"test_case_1">>, [
                {success, true},
                {result, [
                    {failures, 1},
                    {average, [
                        {duration, 2}
                       %, {other_stat, 3}
                    ]},
                    {min, [
                        {duration, 2}
                    ]},
                    {max, [
                        {duration, 2}
                    ]}
                ]},
                %% @px: Ignore for now
                {test_conditions, [
                    {run_count, 5},
                    {sleep_time, 1000}
                ]},
                {runs, [
                ]}
            ]}
        ]}
    ]},
    TestFilePath = filename:join(TestDir, TestFile),
    ok = filelib:ensure_dir(TestFilePath),
    ok = file:write_file(TestFilePath, io_lib:format("~p.~n", [TestData])),
    [{temp_dir, TestDir}, {temp_files, [TestFile]}].

cleanup(SetupOpts) ->
    TempDir = proplists:get_value(temp_dir, SetupOpts),
    TempFiles = proplists:get_value(temp_files, SetupOpts),
    lists:foreach(fun (File) ->
        file:delete(filename:join(TempDir, File))
    end, TempFiles),
    file:del_dir(TempDir).

get_temp_dir(SetupOpts) ->
    proplists:get_value(temp_dir, SetupOpts).
