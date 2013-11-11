-module(playermanager).
-compile(export_all).

-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3, 
	 terminate/2, init/1]).

-behaviour(gen_server).

%% ===================================================================
%% Startup
%% ===================================================================
start_link() ->
    io:format("starting"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, 10}.

%% ===================================================================
%% API functions
%% ===================================================================

login(Username, Password) ->
    gen_server:call(?MODULE, {login, Username, Password}).

%% ===================================================================
%% gen_server callbacks
%% =================================================================== 

handle_call({login, _, _}, _From, ID) ->
    {reply, {ok, ID}, ID + 1}.

 
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
