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

from(project_update, {Data}) ->
    list_to_tuple(
        [proplists:get_value(<<"id">>, Data) |
            tuple_to_list(from(project_new, {Data}))]
    );

from(project, ProjectID) ->
    ProjectID.

%% ============================================================================
%% To jiffy intermediate from Erlang term()
%% ============================================================================

to(project_new, ProjectID) ->
    ProjectID;

to(project_update, _) ->
    null;

to(project, #project{id=ID, name=Name, repo_url=RepoURL, branch=Branch,
        polling=Polling, build_instructions=BuildInstr}) ->
    Polling1 = case Polling of
        on_demand -> ?BIN(on_demand);
        {time, N} -> {[{time, N}]}
    end,
    BuildInstr1 = [?BIN(I) || I <- BuildInstr],

    {[
        {id, ID},
        {name, ?BIN(Name)},
        {repo_url, ?BIN(RepoURL)},
        {branch, ?BIN(Branch)},
        {polling_strategy, Polling1},
        {build_instructions, BuildInstr1}
    ]}.

%% ============================================================================


