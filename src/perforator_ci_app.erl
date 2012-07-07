%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_app).
-behaviour(application).

-include("perforator_ci.hrl").

-export([start/2, stop/1]).

%% ============================================================================

start(_, _) ->
    Dispatch = [
        {'_', [
            % /static/
            {[<<"static">>, '...'], cowboy_http_static, [
                {directory, {priv_dir, perforator_ci, [<<"www">>]}},
                {mimetypes, [
                    {<<".css">>, [<<"text/css">>]},
                    {<<".js">>, [<<"application/javascript">>]},
                    {<<".png">>, [<<"image/png">>]}
                ]}
            ]},
            % /websocket
            {[<<"websocket">>], perforator_ci_ws, []},
            % /api/1/*
            {[<<"api">>, <<"1">>, '...'], perforator_ci_web_handler, []},
            % /*
            {'_', cowboy_http_static, [
                {directory, {priv_dir, perforator_ci, [<<"www">>]}},
                {file, <<"index.html">>},
                {mimetypes, [{<<".html">>, [<<"text/html">>]}]}
            ]}
        ]}
    ],

    Port = perforator_ci_utils:get_env(perforator_ci, http_port, ?HTTP_PORT),
    LCount = perforator_ci_utils:get_env(perforator_ci, list_count,
        ?LIST_COUNT),
    cowboy:start_listener(perforator_ci_listener, LCount,
        cowboy_tcp_transport, [{port, Port}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ),

    perforator_ci_db:wait_for_db(),

    perforator_ci_sup:start_link().

stop(_) -> ok.
