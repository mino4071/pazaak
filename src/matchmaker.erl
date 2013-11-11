-module(matchmaker).
-compile(export_all).

-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3, 
	 terminate/2, init/1]).

-behaviour(gen_server).

-define(LOGG(F, P), io:format("[~p:~p]" ++ F ++ "\n", [?MODULE, self()] ++ P)).

%% ===================================================================
%% Startup
%% ===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

%% ===================================================================
%% API functions
%% ===================================================================

matchmake(PlayerId) ->
    ?LOGG("Matchmaking...", []),
    Result = gen_server:call(?MODULE, {matchmake, PlayerId}, infinity),
    ?LOGG("Matchmaking complete", []),
    Result.

%% ===================================================================
%% gen_server callbacks
%% =================================================================== 

handle_call({matchmake, PlayerId}, From, []) ->
    ?LOGG("No match found, starting new game.", []),
    {ok, GameRef} = game:new(PlayerId),
    {noreply, [{From, GameRef}]};
handle_call({matchmake, PlayerId}, _, [{Client,GameRef}|Tl]) ->
    ?LOGG("Match found, joining game.", []),
    game:join(PlayerId, GameRef),
    gen_server:reply(Client, {ok, GameRef}),
    {reply, {ok, GameRef}, Tl}.

 
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
