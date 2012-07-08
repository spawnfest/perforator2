%% @doc JSON intermediate format (jiffy) (de)serializer to Erlang terms.

%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_json).

-include("perforator_ci.hrl").

-export([
    from/2,
    to/2
]).

%% ============================================================================
%% From jiffy intermediate to Erlang term()
%% ============================================================================

from(project_new, {Data}) ->
    Polling =
        case proplists:get_value(<<"polling_strategy">>, Data) of
            <<"on_demand">> -> on_demand;
            {[{<<"time">>, T}]} -> {time, T}
        end,
    BuildInstr = [binary_to_list(I) || I <-
        proplists:get_value(<<"build_instructions">>, Data)],

    {
        proplists:get_value(<<"name">>, Data),
        binary_to_list(proplists:get_value(<<"repo_url">>, Data)),
        binary_to_list(proplists:get_value(<<"branch">>, Data)),
        perforator_ci_git, % @todo clean dirty hack
        Polling,
        BuildInstr,
        []
    };

from(Schema, Data) ->
    ok.

to(project_new, Data) ->
    Data;

to(Scema, Data) ->
    ok.

