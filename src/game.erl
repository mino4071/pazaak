-module(game).
-compile(export_all).

-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3, 
	 terminate/2, init/1]).

-behaviour(gen_server).

-record(player, {id,
		 hand,
		 table = [],
		 sum = 0}).

-record(state, {player,
		opponent,
		deck,
		waiting}).

logg(F, P) ->
    io:format("[~p] " ++ F, [?MODULE] ++ P).

%% ===================================================================
%% Startup
%% ===================================================================
start_link(PlayerId) ->
    gen_server:start_link(?MODULE, PlayerId, []).

init(PlayerId) ->
    [Card|Deck] = card:main_deck(), 
    
    Player = #player{id = PlayerId,
		     hand = card:player_hand(),
		     table = [Card],
		     sum = Card},
  
    State = #state{player = Player,
		   deck = Deck},
    {ok, State}.

%% ===================================================================
%% API functions
%% ===================================================================

new(PlayerId) ->
    game_sup:start_game(PlayerId).

join(PlayerId, GameRef) ->
    gen_server:call(GameRef, {join, PlayerId}).

state(PlayerId, GameRef) ->       
    gen_server:call(GameRef, {state, PlayerId}).

end_turn(PlayerId, GameRef) ->
    gen_server:call(GameRef, {end_turn, PlayerId}).

wait_for_opponent(PlayerId, GameRef) ->
    gen_server:call(GameRef, {wait_for_opponent, PlayerId}, infinity).

%% ===================================================================
%% gen_server callbacks
%% =================================================================== 

handle_call({join, PlayerId}, _From, State) ->
    Player = #player{id = PlayerId,
		     hand = card:player_hand()},
    NewState = State#state{opponent = Player},
    {reply, ok, NewState};

handle_call({state, PlayerId}, _From, State) ->
    {reply, build_response(PlayerId, State), State};

handle_call({end_turn, PlayerId}, _From, State = #state{player = #player{id = PlayerId}}) ->
    [Card|Deck] = State#state.deck,
    Opponent = State#state.opponent#player
	{table=[Card|State#state.opponent#player.table]},

    NewState = State#state
	{
	  deck = Deck,
	  opponent = Opponent
	},
    
    update_game_state(NewState);
			      
handle_call({wait_for_opponent, PlayerId}, From, State) ->
    case State#state.player#player.id of
	PlayerId ->
	    {reply, build_response(PlayerId, State), State};
	_ ->
	    {noreply, State#state{waiting = From}}
    end;

handle_call(_, _, State) ->
    {reply, illegal_action, State}.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% ===================================================================
%% Helper functions
%% =================================================================== 

update_game_state(State = #state{player=Player, opponent=Opponent}) ->
    PlayerReturnState = build_response(Player#player.id, State),
    OpponentReturnState = build_response(Opponent#player.id, State),
    
    case State#state.waiting of
	Client when is_tuple(Client) ->
	    gen_server:reply(Client, OpponentReturnState);
	_ -> ok
    end,
    
    {reply, PlayerReturnState, State}.


build_response(PlayerId, State) ->
    {Turn, Player, Opponent} = 
	case State#state.player#player.id of
	    PlayerId ->
		{yours, State#state.player, State#state.opponent};
	    _ ->
		{opponents, State#state.opponent, State#state.player}
	    end,

    PlayerState = {Player#player.id,
		   Player#player.hand,
		   Player#player.table,
		   Player#player.sum},
    OpponentState = {Opponent#player.id,
		     Opponent#player.table,
		     Opponent#player.sum},
		   
    {ok, {Turn, PlayerState, OpponentState}}.
    
    
