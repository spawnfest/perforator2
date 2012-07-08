%% @doc
%% REST API (v1) of Perforator CI.
%% See handle_request/2 for possible API calls.
%%
%% Request with POST should send "data=JSON.stringify(Obj)"
%% Each response (JSON) should be wrapped into:
%% {
%%      error: null | string()
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

-define(INPUT_KEY, <<"data">>).

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
                    Data = jiffy:decode(
                        proplists:get_value(?INPUT_KEY, QS, <<"null">>)),
                    
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
handle_request([<<"project">>, <<"new">>], Data, _Req) ->
    wrap_call(project_new,
        fun () ->
            perforator_ci:create_and_start_project(
                perforator_ci_json:from(project_new, Data))
        end);

%% 404
handle_request(_, Data, _) ->
    ?info("DATA", [{data, Data}]),
    throw(404).

%% ============================================================================
%% Helpers
%% ============================================================================

wrap_call(Type, Fun) ->
    try
        {[
            {error, null},
            {msg, perforator_ci_json:to(Type, Fun())}
        ]}
    catch
        throw:{Err, Details} ->
            {[
                {error, to_bin(Err)},
                {msg, to_bin(Details)}
            ]};
        throw:Err ->
            {[ 
                {error, to_bin(Err)},
                {msg, <<"\"\"">>}
            ]}
    end.

%% @doc No comments.
to_bin(X) when is_integer(X) ->
    list_to_binary(integer_to_list(X));
to_bin(X) when is_list(X) ->
    list_to_binary(X);
to_bin(X) when is_atom(X) ->
    list_to_binary(atom_to_list(X));
to_bin(X) ->
    ?FMT("~p", [X]).
