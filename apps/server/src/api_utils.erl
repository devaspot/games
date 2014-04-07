-module(api_utils).

-include("classes.hrl").
-include("logging.hrl").
-include_lib("server/include/log.hrl").

-export([to_known_record/2]).
-export([members/1, members/2, name/1]).
-export([gametype_to_atom/1, gametype_to_binary/1,
         gamemodule_to_gametype/1, gametype_to_gamemodule/1]).

gametype_to_atom(<<"okey">>) -> okey;
gametype_to_atom(<<"king">>) -> king;
gametype_to_atom(<<"tavla">>) -> tavla;
gametype_to_atom(A) when is_atom(A) -> A.

gamemodule_to_gametype(Atom) -> "game_" ++ X = atom_to_list(Atom), list_to_atom(X).
gametype_to_gamemodule(Atom) -> list_to_atom("game_" ++ atom_to_list(Atom)).
gametype_to_binary(GT) -> atom_to_binary(GT, latin1).

members(T) -> members(T, recrunt).
members(T, recrunt) -> Keys = recrunt:fields(T), Values = tl(tuple_to_list(T)), lists:zip(Keys, Values);
members(T, Module) ->
    try
        Keys = Module:fields(T),
        Values = tl(tuple_to_list(T)),
        lists:zip(Keys, Values)
    catch _:_ ->
            members(T, recrunt)
    end.

name(T) -> element(1, T).

to_known_record(Bin, Members) when is_binary(Bin) ->
    Tag = try
              T = list_to_atom(binary_to_list(Bin)),
              recrunt:fields(T),
              T
          catch
              _:_ ->
                  ?INFO("{Bin, Members}: ~p", [{Bin, Members}]),
                  erlang:error(api_error_unknown_call)
          end,
    to_known_record(Tag, Members);

to_known_record(Tag, Members0) when is_atom(Tag) ->
    try
        Names = recrunt:fields(Tag),
%        ?INFO("Names: ~p, Members: ~p",[Names,case Members0 of null -> []; X -> X end]),
        Members = case to_proper_proplists(Members0) of null -> []; Y -> Y end,
%        ?INFO("Members: ~p",[Members]),
        Rev = lists:map(fun(X) ->
                                Res = proplists:get_value(X, Members),
                                %% true = (Res =/= undefined),
                                Res
                        end, Names),
        List = [Tag | Rev],
        list_to_tuple(List)
    catch
        _:_ ->
            ?INFO("{Tag, Members0}: ~p", [{Tag, Members0}]),
            erlang:error(api_error_wrong_params)
    end.

to_proper_proplists([{A, _} | _] = List) when is_binary(A)-> to_proper_proplists(List, []);
to_proper_proplists(List) -> List.
to_proper_proplists([], Acc) -> lists:reverse(Acc);
to_proper_proplists([{A, B} | R], Acc) when is_binary(A)-> to_proper_proplists(R, [{list_to_atom(binary_to_list(A)), B} | Acc]).
