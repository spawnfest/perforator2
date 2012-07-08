%% @doc WebSockets handler used for catching events pushed through pubsub.
%%
%% Each response has a following form:
%% {
%%     err: null | string(),
%%     type: string(),
%%     msg: {}
%% }

%% @author Martynas <martynas@numeris.lt>

-module(perforator_ci_ws).

-include("perforator_ci.hrl").

-export([init/3]).

-export([
    websocket_init/3,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3
]).

%% ============================================================================

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    % Subscribe:
    ok = perforator_ci_pubsub:subscribe(perforator_ci_project),
    ok = perforator_ci_pubsub:subscribe(perforator_ci_builder),

    {ok, Req, []}.

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({perforator_ci_event, Group, {Type, Data}}, Req, State) ->
    ?info("OMG", [{group, Group}, {type, Type}, {data, Data}]),
    Reply = jiffy:encode({[
        {err, null},
        {type, ?BIN(Type)},
        {msg, perforator_ci_json:to(Type, Data)}
    ]}),

    {reply, {text, Reply}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
