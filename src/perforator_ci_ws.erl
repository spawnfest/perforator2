%% @doc WebSockets handler used for catching events pushed through pubsub.

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

websocket_info({perforator_ci_event, Group, Data}, Req, State) ->
    Reply = handle_event(Group, Data),

    {reply, {text, Reply}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

%% ============================================================================
%% Event handlers
%% ============================================================================

handle_event(Group, Ev) ->
    jiffy:encode(handle_event_1(Group, Ev)).

handle_event_1(perforator_ci_project, {build_init, Data}) ->
    perforator_ci_json:to(build_init, Data);

handle_event_1(perforator_ci_project, {build_finished, Data}) ->
    perforator_ci_json:to(build_finished, Data).
