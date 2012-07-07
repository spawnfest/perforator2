%% @doc Simple pub-sub interface based on pg2
%%
%% Broadcast groups:
%% * perforator_ci_project
%% * perforator_ci_builder
%%
%% Groups members receive messages in a following form:
%% {perforator_ci_event, Group, Msg}.

%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_pubsub).

-export([
    subscribe/1,
    broadcast/2,

    init/0
]).

%% ============================================================================

%% @doc Subscribes to a given group.
-spec subscribe(perforator_ci_types:pubsub_group()) -> ok.
subscribe(Group) ->
    pg2:join(Group, self()).

%% @doc Sends message to group members.
-spec broadcast(perforator_ci_types:pubsub_group(), Msg :: term()) -> ok.
broadcast(Group, Msg) ->
    lists:foreach(
        fun (Pid) ->
            try
                Pid ! {perforator_ci_event, Group, Msg}
            catch
                _ -> ok % user will hit ^r probably
            end
        end,
        pg2:get_members(Group)
    ).

%% ============================================================================

%% @doc Creates groups
init() ->
    pg2:create(perforator_ci_project),
    pg2:create(perforator_ci_builder).
