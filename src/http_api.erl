-module(http_api).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET', [<<"login">>, Name, Password], _Req) ->
    Pid = api:login(Name, Password),
    {ok, [], list_to_binary(integer_to_list(Pid))};

handle('GET',[PlayerId, <<"new_game">>], _Req) ->
    Result = api:matchmake(PlayerId),
    ResultString = io_lib:format("~p", [Result]),
    {ok, [], list_to_binary(ResultString)};

handle('GET', [<<"ping">>, _, <<"ping">>], _Req) ->
    {ok, [], <<"Pong">>};

%handle('GET', [PlayerId, GameId, <<"end_turn">>], _Req) ->
%    {ok, [], <<>>};

%handle('GET', [PlayerId, GameId, <<"play_card">>, Card, Value], _Req) ->
%    {ok, [], <<>>};

%handle('GET', [PlayerId, GameId, <<"stand">>], _Req) ->
%    {ok, [], <<>>};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
