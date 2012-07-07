%% @author Martynas <martynas@numeris.lt>

-module(perforator_ci_app).
-behaviour(application).

-export([start/2, stop/1]).

%% @todo move back after finishing with testing
-define(INDEX_HTML, 
    [
        {directory, {priv_dir, perforator_ci, [<<"www">>]}},
        {file, <<"index.html">>},
        {mimetypes, [{<<".html">>, [<<"text/html">>]}]}
    ]).

%% ============================================================================

start(_, _) ->
    Dispatch = [
        {'_', [
            % /
            {[], cowboy_http_static, ?INDEX_HTML},
            {[<<"run">>, '...'], cowboy_http_static, ?INDEX_HTML},
            {[<<"test">>, '...'], cowboy_http_static, ?INDEX_HTML},
            % /static/
            {[<<"static">>, '...'], cowboy_http_static, [
                {directory, {priv_dir, perforator_ci, [<<"www">>]}},
                {mimetypes, [
                    {<<".css">>, [<<"text/css">>]},
                    {<<".js">>, [<<"application/javascript">>]}]}
            ]},
            % /*
            {'_', perforator_ci_web_handler, []}
        ]}
    ],

    {ok, Port} = application:get_env(perforator_ci, http_port),
    {ok, ListCount} = application:get_env(perforator_ci, listener_count),
    cowboy:start_listener(perforator_ci_listener, ListCount,
        cowboy_tcp_transport, [{port, Port}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ),

    perforator_ci_sup:start_link().

stop(_) -> ok.
