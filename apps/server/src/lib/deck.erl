%% Author: Serge Polkovnikov <serge.polkovnikov@gmail.com>
%% Created: Oct 8, 2012
%% Description: The library for manipulations with decks.
-module(deck).


%% Predefined types of deck:
%%    empty - No one element in the deck.
%%
%%    okey - The deck consist of such types of element: {Color, Value} | false_okey.
%%         There are two instances of each element in the deck.
%%              Color = 1..4
%%              Value = 1..13

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
         init_deck/1,
         from_list/1,
         to_list/1,
         shuffle/1,
         pop/2,
         push/2,
         get/2,
         put/3,
         replace/3,
         size/1,
         del_first/2,
         member/2
        ]).

%%
%% API Functions
%%


%% @spec init_deck(Type) -> Deck
%% @doc Creates a deck.
%% @end

init_deck(empty) ->
    [];
init_deck(okey) ->
    [false_okey, false_okey | [{C, V} || C <- [1,2,3,4], V <- lists:seq(1,13), _ <- [1,2]]].


%% @spec from_list(List) -> Deck
%% @doc Creates a deck from the list of elements.
%% @end

from_list(List) ->
    List.


%% @spec to_list(Deck) -> List
%% @doc Convers the deck to a list of elements.
%% @end

to_list(Deck) ->
    Deck.

%% @spec shuffle(Deck1) -> Deck2
%% @doc Shuffles the deck.
%% @end

shuffle(Deck) when is_list(Deck) ->
    shuffle(Deck, deck:size(Deck), []).

shuffle([], 0, Acc) -> Acc;
shuffle(Deck, Size, Acc) ->
    Pos = crypto:rand_uniform(1, Size+1),
    {E, Deck1} = get(Pos, Deck),
    shuffle(Deck1, Size-1, [E |Acc]).


%% @spec pop(Num, Deck1) -> {Deck2, Deck3}
%% @doc Takes specified number of elements from top of the deck.
%% @end

pop(Num, Deck) when Num > 0, is_list(Deck) ->
    lists:split(Num, Deck).


%% @spec push(Deck1, Deck2) -> Deck3.
%% @doc Puts the first deck on the top of the second deck.
%% @end

push(Deck1, Deck2) when is_list(Deck1), is_list(Deck2) ->
    Deck1 ++ Deck2.


%% @spec get(Pos, Deck1) -> {E, Deck2}
%% @doc Draws an element at the position Pos of the deck.
%% Position is counted from top of the deck.
%% @end

get(Pos, Deck) when Pos > 0, is_list(Deck) ->
    {Head, [E | Tail]} = lists:split(Pos - 1, Deck),
    {E, Head ++ Tail}.

%% @spec put(E, Pos, Deck1) -> Deck2
%% @doc Inserts the element to the position Pos of the deck.
%% Position is counted from top of the deck.
%% @end

put(E, Pos, Deck) when Pos > 0, is_list(Deck) ->
    {Head, Tail} = lists:split(Pos - 1, Deck),
    Head ++ [E | Tail].


%% @spec replace(E1, E2, Deck1) -> Deck2
%% @doc Replaces all instances of the element E1 in the deck by the element E2.
%% @end

replace(E1, E2, Deck) ->
    [if E == E1 -> E2; true -> E end || E <- Deck].


%% @spec size(Deck) -> Size
%% @doc Returns a number of elements in the deck.
%% @end

size(Deck) when is_list(Deck) ->
    length(Deck).


%% @spec del_first(E, Deck1) -> {ok, Deck2} | error
%% @doc Deletes the element from the deck.
%% @end

del_first(E, Deck) ->
    case lists:member(E, Deck) of
        true ->
            {ok, lists:delete(E, Deck)};
        false ->
            error
    end.

%% @spec member(E, Deck) -> boolean()
%% @doc Checks is the element a memeber of the deck.
%% @end

member(E, Deck) ->
    lists:member(E, Deck).
%%
%% Local Functions
%%

