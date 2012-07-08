%% @doc
%% REST API (v1) of Perforator CI.
%% See handle_request/2 for possible API calls.
%%
%% Request with POST should send JSON.stringify(Obj)
%% Each response (JSON) should be wrapped into:
%% {
%%      err: null | string()
%%      msg: {} | string() (in error case
%% }.
%% All calls to internal call can throw Error, or {Error, Details}. Otherwise
%% whole exception will be serialized and returned as response to a request.
%%
%% @author Martynas <martynasp@gmail.com> 

-module(perforator_ci_web_handler).

-include("perforator_ci.hrl").

-export([
    init/3,
    handle/2,
    terminate/2
]).

%% ===========================================================================

init({tcp, http}, Req, _Opts) ->
    {ok, Req, []}.

handle(Req, State) ->
    {ok, Req1} =
        case cowboy_http_req:path(Req) of
            {[<<"api">>|[<<"1">>|Path]], _} ->
                % Call request handler if such exists (otherwise return 404)
                try
                    % Get POST param (stringified JSON)
                    {QS, _} = cowboy_http_req:body_qs(Req),
                    Data = case QS of
                        [] -> null;
                        [{D, true}] -> jiffy:decode(D)
                    end,

                    cowboy_http_req:reply(
                        200,
                        [{<<"Content-Type">>, <<"application/json">>}],
                        jiffy:encode(handle_request(Path, Data, Req)),
                        Req
                    )
                catch
                    throw:404 ->
                        cowboy_http_req:reply(404, Req);
                    throw:{error, {_, _}} -> % Most likely from jiffy
                        cowboy_http_req:reply(500, Req)
                end;
            Path -> % Non API call
                lager:warning("Unknown path: ~p~n", [Path]),
                cowboy_http_req:reply(404, Req)
        end,

    {ok, Req1, State}.

terminate(_Req, _State) ->
    ok.

%% ============================================================================
%% Request handlers
%% ============================================================================

%% /project/new
handle_request([<<"project">>], Data, _Req) ->
    wrap_call(project,
        fun () ->
            perforator_ci_db:get_project(
                perforator_ci_json:from(project, Data))
        end);

%% /project/new
handle_request([<<"project">>, <<"new">>], Data, _Req) ->
    wrap_call(project_new,
        fun () ->
            perforator_ci:create_and_start_project(
                perforator_ci_json:from(project_new, Data))
        end);

%% /project/update
handle_request([<<"project">>, <<"update">>], Data, _Req) ->
    wrap_call(project_update,
        fun () ->
            perforator_ci:update_project(
                perforator_ci_json:from(project_update, Data))
        end);

%% /projects
handle_request([<<"projects">>], _Data, _Req) ->
    wrap_call(projects,
        fun () ->
            perforator_ci_db:get_projects()
        end);

%% /builders
handle_request([<<"builders">>], _Data, _Req) ->
    wrap_call(builders, fun () -> perforator_ci:get_builders() end);

%% /builds
handle_request([<<"builds">>], Data, _Req) ->
    wrap_call(builds,
        fun () ->
            perforator_ci_db:get_builds(
                perforator_ci_json:from(builds, Data))
        end);

%% /build_now
handle_request([<<"build_now">>], Data, _Req) ->
    wrap_call(build_now,
        fun () ->
            perforator_ci_project:build_now(
                perforator_ci_json:from(build_now, Data))
        end);

%% /build
handle_request([<<"build">>], Data, _Req) ->
    wrap_call(build,
        fun () ->
            perforator_ci_db:get_build(
                perforator_ci_json:from(build, Data))
        end);

%% /previous_build
handle_request([<<"previous_build">>], Data, _Req) ->
    wrap_call(previous_build,
        fun () ->
            perforator_ci_db:get_previous_build_id(
                perforator_ci_json:from(previous_build, Data))
        end);

handle_request([<<"test_runs">>], Data, _Req) ->
    wrap_call(test_runs,
        fun () ->
            {ProjectID, SuiteName, TestName} =
                perforator_ci_json:from(test_runs, Data),
            perforator_ci_db:get_test_runs(ProjectID, SuiteName, TestName)
        end);

%% 404
handle_request(_, _, _) ->
    throw(404).

%% ============================================================================
%% Helpers
%% ============================================================================

%% Execs and wraps result.
wrap_call(Type, Fun) ->
    try
        {[
            {err, null},
            {msg, perforator_ci_json:to(Type, Fun())}
        ]}
    catch
        throw:{Err, Details} ->
            {[
                {err, ?BIN(Err)},
                {msg, ?BIN(Details)}
            ]};
        throw:Err ->
            {[ 
                {err, ?BIN(Err)},
                {msg, <<"\"\"">>}
            ]}
    end.
