%% @doc Projects builder. Only one per app because of obvious reasons.

%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_builder).

-behaviour(gen_server).

-include("perforator_ci.hrl").

%% API
-export([
    start_link/0
]).

%% gen_server callbacks
-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-record(state, {
    foobar
}).

-ifdef(TEST).
-compile(export_all).
-endif.

-define(SERV_NAME, {global, {perforator_ci_builder, node()}}).

%% ============================================================================
%% API
%% ============================================================================

%% @doc Starts builder.
%% Builder name is global and a form of {perforator_ci_builder, node()}.
-spec start_link() -> {ok, term()}.
start_link() ->
    gen_server:start_link(?SERV_NAME, ?MODULE, [], []).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%% =============================================================================
%% Helpers
%% =============================================================================
