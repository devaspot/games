%%----------------------------------------------------------------------
%% @author Vladimir Baranov <baranoff.vladimir@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc nsm_mq routines
%% @end
%%--------------------------------------------------------------------
-module(nsx_opt).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
         encode/1,
         decode/1
        ]).

-export([opt/3,
         override_opt/3,
         get_env/2,
         get_env/3]).

%%
%% API Functions
%%

encode(Term) ->
    term_to_binary(Term).

decode(Binary) when is_binary(Binary) ->
    binary_to_term(Binary).

%% Get option from list. Option can be either single option, like 'name', or
%% tuple {name, "Name"}. Single options are boolean, but return value can be
%% overwritten with default value. If option not found in options List - returns
%% default value.
opt(Option, List, Default) ->
    case lists:member(Option, List) of
        true ->
            true;
        false ->
            proplists:get_value(Option, List, Default)
    end.

-spec override_opt(atom(), any(), list()) -> list().

override_opt(Option, Value, Options) ->
    CleanOptions = lists:filter(
                     fun({O, _}) when O == Option -> false;
                        (O)      when O == Option -> false;
                        (_)                       -> true
                     end, Options),

    [{Option, Value} | CleanOptions].


%% @doc Get env options from application config. If parameter not set - default
%% value will be returned
-spec get_env(Par::atom(), Default::term()) -> term().

get_env(Par, DefaultValue) ->
    case application:get_env(Par) of
        undefined ->
            DefaultValue;
        {ok, Value} ->
            Value
    end.

get_env(App, Par, DefaultValue) ->
    case application:get_env(App, Par) of
        undefined ->
            DefaultValue;
        {ok, Value} ->
            Value
    end.

