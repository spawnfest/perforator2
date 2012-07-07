%% @doc Wrapper for mnesia.

%% @author Martynas <martynas@gmail.com>

-module(perforator_ci_db).

-include_lib("stdlib/include/qlc.hrl").
-include("perforator_ci.hrl").

%% API
-export([
    create_project/1,
    get_project/1,
    get_projects/0,
    create_build/4,
    get_last_build/1,
    get_unfinished_builds/1,
    finish_build/2,

    wait_for_db/0,
    init/0,
    create_tables/0,
    dump/1
]).

%% ============================================================================
%% API
%% ============================================================================

%% @doc Initializes a new project. Should be called before starting project
%% worker.
%%
%% Returns generated project id. If project with given project name exists,
%% will return already generated id.
-spec create_project({
        perforator_ci_types:project_name(), perforator_ci_types:repo_url(),
        perforator_ci_types:branch(), perforator_ci_types:repo_backend(),
        perforator_ci_types:polling_strategy(),
        perforator_ci_types:build_instructions(), list()}) ->
            perforator_ci_types:project_id().
create_project({Name, RepoUrl, Branch, RepoBackend, Polling, BuildInstr,
        Info}) ->
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
                            repo_url = RepoUrl,
                            branch = Branch,
                            repo_backend = RepoBackend,
                            polling = Polling,
                            build_instructions = BuildInstr,
                            info = Info
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

%% @doc Returns all unfinished (sorted) builds.
-spec get_unfinished_builds(perforator_ci_types:project_id()) ->
        [#project_build{}].
get_unfinished_builds(ProjectID) ->
    sort_builds(transaction(
        fun () ->
            mnesia:index_match_object(
                #project_build{project_id=ProjectID, finished=false, _='_'},
                #project_build.project_id
            )
        end
    ), asc).

%% @doc Returns last project build.
-spec get_last_build(perforator_ci_types:project_id()) ->
        #project_build{} | undefined.
get_last_build(ProjectID) ->
    transaction(
        fun () ->
            case mnesia:index_read(project_build, ProjectID,
                    #project_build.project_id) of
                [] -> undefined;
                Bs when is_list(Bs) -> hd(sort_builds(Bs, desc))
            end
        end).

%% @doc Updates build status to finished and appends info.
%% @throws {build_not_found, BuildID}.
-spec finish_build(perforator_ci_types:build_id(), list()) -> ok.
finish_build(BuildID, Info) ->
    transaction(
        fun () ->
            case mnesia:read(project_build, BuildID) of
                [#project_build{}=B] ->
                    ok = mnesia:write(
                        B#project_build{
                            finished = true,
                            info = Info
                        });
                [_] ->
                    throw({build_not_found, BuildID})
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

-spec sort_builds([#project_build{}], asc|desc) -> [#project_build{}].
sort_builds(Builds, Order) ->
    Fun =
        case Order of
            asc ->
                fun (#project_build{id=A}, #project_build{id=B}) -> A < B end;
            desc ->
                fun (#project_build{id=A}, #project_build{id=B}) -> A >= B end
        end,

    lists:sort(Fun, Builds).

dump(Table) ->
    Fun = fun() ->
        qlc:eval(qlc:q([R || R <- mnesia:table(Table)]))
    end,
    transaction(Fun).
