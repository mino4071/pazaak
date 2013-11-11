-module(bot).
-compile(export_all).

start() ->
    spawn(?MODULE, init, []).

init() ->
    {ok, ID} = api:login("Bot", "bot"),
    {ok, GameRef} = api:matchmake(ID),
    {ok, State} = api:state(ID, GameRef),
    loop(GameRef, State).
    

loop(GameRef, State) ->
    print_state(State),
    {Turn,
     {PlayerId,
      _PlayerHand,
      _PlayerTable,
      _PlayerSum},
     {_OpponentId,
      _OpponentTable,
      _OpponentSum}} = State,
    case Turn of
	yours ->
	    io:format("ending turn\n"),
	    {ok, NewState} = api:end_turn(PlayerId, GameRef);
	_ ->
	    io:format("waiting for opponent\n"),
	    {ok, NewState} = api:wait_for_opponent(PlayerId, GameRef)
    end,
    loop(GameRef, NewState).

print_state(State) ->
    {Turn,
     {_PlayerId,
      PlayerHand,
      PlayerTable,
      PlayerSum},
     {_OpponentId,
      OpponentTable,
      OpponentSum}} = State,
    
    io:format("\n"),
    
    case Turn of
	yours ->
	    io:format("Turn: player\n\n");
	_ ->
	    io:format("Turn: Opponent\n\n")
    end,
    
    io:format("Player hand: ~p\n", [PlayerHand]),
    io:format("Player table: ~p (~p)\n\n", [PlayerTable, PlayerSum]),

    io:format("Opponent table: ~p (~p)\n\n", [OpponentTable, OpponentSum]).
