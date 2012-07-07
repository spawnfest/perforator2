%% App supervisor

%% @author Martynas <martynasp@gmail.com>

-module(perforator_ci_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% ============================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ============================================================================

init([]) ->
    Processes = [
        {
            perforator_ci_project_sup,
            {perforator_ci_project_sup,  start_link, []},
            transient, infinity, supervisor, [perforator_ci_project_sup]
        }
    ],

	{ok, {{one_for_one, 10, 10}, Processes}}.
