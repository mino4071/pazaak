-module(card).
-compile(export_all).

card_list() ->
    [{plus, 1},
     {plus, 2},
     {plus, 3},
     {plus, 4},
     {plus, 5},
     {plus, 6},
     {minus, 1},
     {minus, 2},
     {minus, 3},
     {minus, 4},
     {minus, 5},
     {minus, 6}
     %{plus_or_minus, 1},
     %{plus_or_minus, 2},
     %{plus_or_minus, 3},
     %{plus_or_minus, 4},
     %{plus_or_minus, 5},
     %{plus_or_minus, 6},
     %{plus_or_minus, one_or_two},
     %{flip, two_and_four},
     %{flip, three_and_six},
     %{double, 0},
     %{tiebreaker, 0}
    ].

calculate_sum(Table) ->
    calculate_sum(0, Table).
calculate_sum(Sum, []) ->
    Sum;
calculate_sum(Sum, [{plus, Value}|Table]) ->
    calculate_sum(Sum + Value, Table);
calculate_sum(Sum, [{minus, Value} | Table]) ->
    calculate_sum(Sum - Value, Table);
calculate_sum(Sum, [Value|Table]) when is_integer(Value) ->
    calculate_sum(Sum + Value, Table).

player_hand() ->
    player_hand(4, []).

player_hand(0, Hand) ->
    Hand;
player_hand(Cards, Hand) ->
    List = card_list(),
    Length = length(List),
    Random = random:uniform(Length),
    player_hand(Cards - 1, [lists:nth(Random, List)|Hand]).

main_deck() ->
    Deck = generate_main_deck(10, []),
    shuffle(Deck).

generate_main_deck(0, Deck) ->
    Deck;
generate_main_deck(Value, Deck) ->
    generate_main_deck(Value - 1, [Value, Value, Value, Value | Deck]).


shuffle(List) ->
   randomize(round(math:log(length(List)) + 0.5), List).
 
randomize(1, List) ->    
    randomize(List);
randomize(T, List) ->
    lists:foldl(fun(_E, Acc) ->
			randomize(Acc)
		end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
    D = lists:map(fun(A) ->
			  {random:uniform(), A}
		  end, List),
    {_, D1} = lists:unzip(lists:keysort(1, D)), 
    D1.


