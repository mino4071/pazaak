
-module(game_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_game/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_game(PlayerId) ->
    supervisor:start_child(?MODULE, [PlayerId]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(game, worker)]}}.
