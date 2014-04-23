%% Author: Sergei Polkovnikov <serge.polkovnikov@gmail.com>
%% Created: Jan 8, 2010
%% Description: Multi indices dictionary
-module(midict).
-author("Sergei Polkovnikov <serge.polkovnikov@gmail.com>").

%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").
%%
%% Exported Functions
%%
-export([
         new/0,
         store/4,
         find/2,
         fetch/2,
         geti/3,
         erase/2,
         size/1,
         to_list/1,
         from_list/1,
         fetch_keys/1,
         all_values/1
        ]).

-record(midict,
        {
         keyd,
         indds
        }).

-type midict() :: #midict{}.

%%
%% API Functions
%%

-spec new() -> midict().
%% @spec new() -> midict()
%% @doc Creates empty midict.
%% @end

new() ->
    #midict{
            keyd = dict:new(),
            indds = dict:new()
           }.

-spec store(term(), term(), list(), midict()) -> midict().
%% @spec store(Key, Value, Indices, MIDict) -> MIDict2
%% @doc Puts the entity to the midict.
%% Types:
%%     Key = Value = term()
%%     Indices = [{Index, IndValue}]
%%       Index = IndValue = term()
%%     MIDict = MIDict2 = midict()
%% @end

store(Key, Value, Indices, #midict{keyd = KeyD, indds = IndD}) ->
    Obj = {Key, Value, Indices},
    IndD1 = case dict:find(Key, KeyD) of
                 {ok, OldObj} ->
                     del_indices(OldObj, IndD);
                 error ->
                     IndD
             end,
    NewKeyD = dict:store(Key, Obj, KeyD),
    NewIndD = add_indices(Obj, IndD1),
    #midict{keyd = NewKeyD, indds = NewIndD}.

-spec find(term(), midict()) -> {ok, term()} | error.
%% @spec find(Key, MIDict) -> {ok, Value} | error
%% @doc Get an entity from the dict by the key.
%% Types:
%%     Key = Value = term()
%%     MIDict = midict()
%% @end

find(Key, #midict{keyd = KeyD}) ->
    case dict:find(Key, KeyD) of
        {ok, {_, Value, _}} ->
            {ok, Value};
        error ->
            error
    end.

-spec fetch(term(), midict()) -> term().
%% @spec fetch(Key, MIDict) -> Value}
%% @doc Fetch an entity from the dict by the key. An exception
%% will arise if there is no entity with the key.
%% Types:
%%     Key = Value = term()
%%     MIDict = midict()
%% @end

fetch(Key, #midict{keyd = KeyD}) ->
    {_, Value, _} = dict:fetch(Key, KeyD),
    Value.

-spec geti(term(), term(), midict()) -> [term()].
%% @spec geti(IndexVal, Index, MIDict) -> Values
%% @doc Gets entities from the midict by the index.
%% Types:
%%     IndexVal = Index  = term()
%%     MIDict = midict()
%%     Values = [Value]
%%       Value = term()
%% @end

geti(IVal, Index, #midict{indds = IndD}) ->
    case dict:find({Index, IVal}, IndD) of
        {ok, Set} ->
            [Value || {_, Value, _} <- sets:to_list(Set)];
        error ->
            []
    end.

-spec erase(term(), midict()) -> midict().
%% @spec erase(Key, MIDict) -> MIDict2
%% @doc Delete an entity from the midict by the key.
%% Types:
%%     Key = term()
%%     MIDict = MIDict2 = midict()
%% @end

erase(Key, #midict{keyd = KeyD, indds = IndD}) ->
    NewIndD = case dict:find(Key, KeyD) of
                  {ok, Obj} ->
                      del_indices(Obj, IndD);
                  error ->
                      IndD
              end,
    NewKeyD = dict:erase(Key, KeyD),
    #midict{keyd = NewKeyD, indds = NewIndD}.

-spec size(midict()) -> non_neg_integer().
%% @spec size(midict()) -> integer()
%% @doc Returns number of entities in the dict.
%% @end

size(#midict{keyd = KeyD}) ->
    dict:size(KeyD).

-spec to_list(midict()) -> list({term(), term(), list()}).
%% @spec to_list(midict()) -> List
%% @doc Convert the midict to a list.
%% Types:
%%     List = {Key, Value, Indices}
%%       Key = Value = term()
%%       Indices = [{Index, IndValue}]
%%         Index = IndValue = term()
%% @end

to_list(#midict{keyd = KeyD}) ->
    [Obj || {_, Obj} <- dict:to_list(KeyD)].

-spec from_list(list({term(), term(), list()})) -> midict().
%% @spec from_list(List) -> midict()
%% @doc Convert the list to a midict.
%% Types:
%%     List = {Key, Value, Indices}
%%       Key = Value = term()
%%       Indices = [{Index, IndValue}]
%%         Index = IndValue = term()
%% @end

from_list([]) ->
    midict:new();
from_list([{Key, Value, Indices} | Rest]) ->
    midict:store(Key, Value, Indices, from_list(Rest)).

-spec fetch_keys(midict()) -> list(term()).
%% @spec fetch_keys(midict()) -> Keys
%% @doc Fetches all keys of the midict.
%% Types:
%%     Keys = [Key]
%%       Key = term()
%% @end

fetch_keys(#midict{keyd = KeyD}) ->
    dict:fetch_keys(KeyD).

-spec all_values(midict()) -> list(term()).
%% @spec all_values(midict()) -> Values
%% @doc Fetches all enities values of the midict.
%% Types:
%%     Values = [Value]
%%       Value = term()
%% @end

all_values(#midict{keyd = KeyD}) ->
    [Val || {_, {_, Val, _}} <- dict:to_list(KeyD)].

%%
%% Local Functions
%%
add_indices({_, _, Indices} = Obj, IndD) ->
    add_indices(Obj, IndD, Indices).

add_indices(_Obj, IndD, []) ->
    IndD;
add_indices(Obj, IndD, [I | Indices]) when tuple_size(I) == 2->
    CurSet = get_set(I, IndD),
    NewSet = sets:add_element(Obj, CurSet),
    NewIndD = put_set(NewSet, I, IndD),
    add_indices(Obj, NewIndD, Indices).

del_indices({_, _, Indices} = Obj, IndD) ->
    del_indices(Obj, IndD, Indices).

del_indices(_Obj, IndD, []) ->
    IndD;
del_indices(Obj, IndD, [I | Indices]) ->
    CurSet = get_set(I, IndD),
    NewSet = sets:del_element(Obj, CurSet),
    NewIndD = put_set(NewSet, I, IndD),
    del_indices(Obj, NewIndD, Indices).

get_set(Index, IndDs) ->
    case dict:find(Index, IndDs) of
        {ok, Set} -> Set;
        error -> sets:new()
    end.

put_set(Set, Index, IndDs) ->
    case sets:size(Set) of
        0 -> dict:erase(Index, IndDs);
        _ -> dict:store(Index, Set, IndDs)
    end.
%%

test_test_() ->
    [
     {"new",
      [
       ?_assert(begin midict:new(), true end)
      ]},
     {"store",
      [
       ?_assert(begin midict:store("xxx", value, [{ccc, 5}, {ttt,fff}], x()), true end),
       ?_assert(begin midict:store("xxx", value, [{ccc, 5}, {ttt,fff}], x2()), true end)
      ]},
     {"find",
      [
       ?_assertEqual({ok, {record1}}, midict:find("Key1", x5())),
       ?_assertEqual({ok, record3} , midict:find(<<"Key3">>, x5())),
       ?_assertEqual({ok, {record2, hello}} , midict:find("Key2", x5())),
       ?_assertEqual(error , midict:find(<<"UnknownKey">>, x5()))
      ]},
     {"geti",
      [
       ?_assertEqual(lists:sort([{record1}, {record2, hello}]), lists:sort(midict:geti(5, table, x5()))),
       ?_assertEqual(lists:sort([record3, "record4"]), lists:sort(midict:geti({3,1}, table_seat, x5()))),
       ?_assertEqual([{record2, hello}], midict:geti({5, 3}, table_seat, x5())),
       ?_assertEqual(["record4"], midict:geti(true, haha, x5())),
       ?_assertEqual([], midict:geti(4, table_seat, x5())),
       ?_assertEqual([], midict:geti(100500, xxx, x5()))
      ]},
     {"erase",
      [
       ?_assertEqual(error, midict:find("Key2", midict:erase("Key2", x5())))
      ]},
     {"size",
      [
       ?_assertEqual(0, midict:size(x())),
       ?_assertEqual(4, midict:size(x5())),
       ?_assertEqual(3, midict:size(x6()))
      ]},
     {"fetch_keys",
      [
       ?_assertEqual(lists:sort(["Key1", "Key2", <<"Key3">>, key4]), lists:sort(midict:fetch_keys(x5())))
      ]}
    ].

x() -> midict:new().
x2() -> midict:store("Key1", {record1}, [{table, 5}, {table_seat, {5, 2}}], x()).
x3() -> midict:store("Key2", {record2, hello}, [{table, 5}, {table_seat, {5, 3}}], x2()).
x4() -> midict:store(<<"Key3">>, record3, [{table, 3}, {table_seat, {3, 1}}], x3()).
x5() -> midict:store(key4, "record4", [{table, 2}, {table_seat, {3, 1}}, {haha, true}], x4()).
x6() -> midict:erase("Key2", x5()).
