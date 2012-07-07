%% @author Martynas <martynas@numeris.lt>

-module(perforator_ci_web_handler).

-export([init/3, handle/2, terminate/2]).

%% ===========================================================================

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(
        200, [{<<"Content-Type">>, <<"application/json">>}],
        handle_request(cowboy_http_req:path(Req)),
        Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

%% ============================================================================
%% Request handlers
%% ============================================================================

handle_request(_) -> <<"ok">>.
