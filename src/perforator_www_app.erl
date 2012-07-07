%% @doc

%% @author Martynas <martynas@numeris.lt>

-module(perforator_www_app).
-behaviour(application).

-export([start/2, stop/1]).

%% ============================================================================

start(_, _) ->
    Dispatch = [
        {'_', [
            % /
            {[], cowboy_http_static, [
                {directory, {priv_dir, perforator_www, [<<"www">>]}},
                {file, <<"index.html">>},
                {mimetypes, [{<<".html">>, [<<"text/html">>]}]}
            ]},
            % /static/
            {[<<"static">>, '...'], cowboy_http_static, [
                {directory, {priv_dir, perforator_www, [<<"www">>]}},
                {mimetypes, [
                    {<<".css">>, [<<"text/css">>]},
                    {<<".js">>, [<<"application/javascript">>]}]}
            ]},
            % /*
            {'_', perforator_www_handler, []}
        ]}
    ],

    {ok, Port} = application:get_env(perforator_www, http_port),
    {ok, ListCount} = application:get_env(perforator_www, listener_count),
    io:format("OMG STARTING: ~p~p~n", [Port, ListCount]),
    cowboy:start_listener(perforator_www_listener, ListCount,
        cowboy_tcp_transport, [{port, Port}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ),

    perforator_www_sup:start_link().

stop(_) -> ok.
