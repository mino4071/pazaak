-module(api).
-compile(export_all).

login(Username, Password) ->
    playermanager:login(Username, Password).

matchmake(PlayerId) ->
    matchmaker:matchmake(PlayerId).

state(PlayerId, GameRef) ->
    game:state(PlayerId, GameRef).

end_turn(PlayerId, GameRef) ->
    game:end_turn(PlayerId, GameRef).

wait_for_opponent(PlayerId, GameRef) ->
    game:wait_for_opponent(PlayerId, GameRef).
