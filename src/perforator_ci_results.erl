%% @doc Reads and parses test results.
%%
%% @author Ignas <i.vysniauskas@gmail.com>

-module(perforator_ci_results).

-include("perforator_ci.hrl").

-define(TEST_RESULT_EXT, ".perf").

%% API
-export([
    read/1
]).

-type suite_result_file() :: {Suite :: atom(), Filepath :: file:filename()}.

%% ============================================================================
%% API
%% ============================================================================

-spec read(Path::file:filename()) -> [perforator_ci_types:test_result()].
read(Path) ->
    %% @hack for now: Looks recursively for ".perf" files.
    ResultFiles = find_result_files(Path),
    parse_result_files(ResultFiles).


-spec parse_result_files([file:filename()]) ->
    [perforator_ci_types:test_result()].
parse_result_files(ResultFiles) ->
    SuitesRes = lists:map(fun parse_result_file/1, ResultFiles),
    [
        {totals, calculate_totals(SuitesRes)},
        {suites, SuitesRes}
    ].

-spec parse_result_file(file:filename()) -> perforator_ci_types:test_result().
parse_result_file(Filepath) ->
    {ok, [Resultdata]} = file:consult(Filepath),
    Resultdata.

-spec find_result_files(Path :: file:filename()) -> [file:filename()].
find_result_files(Path) ->
    SuiteResultList = filelib:fold_files(Path, "_results_.*\.perf", true,
        fun (Filename, Acc) ->
            case suite_from_filename(Filename) of
                {ok, Suite} ->
                    replace_older_results({Suite, Filename}, Acc);
                {error, _} ->
                    Acc
            end
        end,
        []),
    element(2, lists:unzip(SuiteResultList)).

-spec suite_from_filename(Filename :: file:filename()) -> atom().
suite_from_filename(Filename) ->
    BaseName = filename:basename(Filename, ".perf"),
    case re:split(BaseName, "_results_", [{return, list}]) of
        [Suite, _Timestamp] ->
            {ok, list_to_atom(Suite)};
        _ ->
            {error, badly_formated_filename}
    end.

-spec replace_older_results(suite_result_file(), [suite_result_file()]) ->
    [suite_result_file()].
replace_older_results(NewRes={Suite, Filename}, Results) ->
    case proplists:get_value(Suite, Results) of
        undefined ->
            [NewRes|Results];
        Filename2 ->
            case filename:basename(Filename2) > filename:basename(Filename) of
                true -> %% the date timestamp of Filename2 is newer
                    Results;
                false ->
                    [NewRes|Results -- {Suite, Filename2}]
            end
    end.

calculate_totals(SuitesRes) ->
    GenericTotals = lists:foldl(
        fun ({_, SuiteData}, AccTotals) ->
            Totals = proplists:get_value(totals, SuiteData, []),
            sum_properties(Totals, AccTotals)
        end,
        [],
        SuitesRes),
    [{suite_count, length(SuitesRes)}|GenericTotals].

sum_properties(A, B) ->
    MetricTags = proplists:get_keys(A) ++ proplists:get_keys(B),
    [{MetricTag, proplists:get_value(MetricTag, A, 0) +
        proplists:get_value(MetricTag, B, 0)} || MetricTag <- MetricTags].
