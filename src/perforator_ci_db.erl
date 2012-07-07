%% @doc Wrapper for mnesia.

%% @author Martynas <martynas@gmail.com>

-module(perforator_ci_db).

-include("perforator_ci.hrl").

%% API
-export([
    create_project/3,
    get_project/1,
    get_projects/0,
    create_build/4,

    wait_for_db/0,
    init/0,
    create_tables/0
]).

%% ============================================================================
%% API
%% ============================================================================

%% @doc Initializes a new project. Should be called before starting project
%% worker.
%%
%% Returns generated project id. If project with given project name exists,
%% will return already generated id.
-spec create_project(perforator_ci_types:project_name(), binary(),
        perforator_ci_types:polling_strategy()) ->
        perforator_ci_types:project_id().
create_project(Name, Repo, Polling) ->
    transaction(
        fun () ->
            case mnesia:index_read(project, Name, #project.name) of
                [#project{id=ID}] -> ID;
                [] ->
                    % Get next id
                    ID =
                        case mnesia:last(project) of
                            '$end_of_table' -> 1;
                            N when is_integer(N) -> N+1
                        end,
                    % Write teh project data
                    ok = mnesia:write(
                        #project{
                            id = ID,
                            name = Name,
                            repo = Repo,
                            polling = Polling
                        }),

                    ID
            end
        end).

%% @doc Creates new build
create_build(ProjectID, TS, CommitID, Info) ->
    transaction(
        fun () ->
            case mnesia:index_read(project_build, CommitID,
                    #project_build.commit_id) of % In a perfect world
                    % collisions don't exist
                [#project_build{id=ID, local_id=LID}] -> {ID, LID};
                [] ->
                    % Get next global and local id
                    ID =
                        case mnesia:last(project_build) of
                            '$end_of_table' -> 1;
                            N when is_integer(N) -> N + 1
                        end,
                    LID =
                        case mnesia:index_read(project_build, ProjectID,
                                #project_build.project_id) of
                            [] -> 1;
                            Bs when is_list(Bs) ->
                                #project_build{local_id=LN} = lists:last(Bs),
                                LN + 1
                        end,
                    % Write
                    ok = mnesia:write(
                        #project_build{
                            id = ID,
                            local_id = LID,
                            project_id = ProjectID,
                            timestamp = TS,
                            commit_id = CommitID,
                            info = Info
                        }),

                    {ID, LID}
            end
        end).

%% @doc Returns #project.
%% @throws {project_not_found, ID}.
-spec get_project(perforator_ci_types:project_id()) -> #project{}.
get_project(ID) ->
    transaction(
        fun () ->
            case mnesia:read(project, ID) of
                [] -> throw({project_not_found, ID});
                [#project{}=P] -> P
            end
        end).

%% @doc Returns #project's.
-spec get_projects() -> [#project{}].
get_projects() ->
    transaction(fun () -> mnesia:match_object(#project{_='_'}) end).

%% @doc Wait till all tables are reachable.
wait_for_db() ->
    mnesia:wait_for_tables([project, project_build], 42000). % @todo Fix

%% ============================================================================
%% DB Init
%% ============================================================================

%% @doc Creates mnesia schema and tables.
%% WARNING: destroys all data!!!
init() ->
    % Schema
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),

    ok = mnesia:start(),

    create_tables().

create_tables() ->
    mnesia:delete_table(project),
    {atomic, ok} = mnesia:create_table(project, [
        {type, ordered_set},
        {attributes, record_info(fields, project)},
        {index, [#project.name]},
        {disc_copies, [node()]}
    ]),

    % @todo Maybe add project_build to #project.builds
    mnesia:delete_table(project_build),
    {atomic, ok} = mnesia:create_table(project_build, [
        {type, ordered_set},
        {attributes, record_info(fields, project_build)},
        {index, [#project_build.project_id, #project_build.commit_id]},
        {disc_copies, [node()]}
    ]),

    ok.

%% ============================================================================
%% Helpers
%% ============================================================================

%% @doc Executes transaction with given funs or fun.
%% @throws {aborted_transaction, term()}.
-spec transaction([fun()] | fun()) -> term().
transaction(Funs) when is_list(Funs) ->
    Fun = fun () -> [F() || F <- Funs] end,
    transaction(Fun);

transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, Return} -> Return;
        {aborted, Reason} -> throw({aborted_transaction, Reason})
    end.
