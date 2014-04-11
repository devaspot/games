-ifndef(TABLE_HRL).
-define(TABLE_HRL, "table.hrl").

-include("../../server/include/types.hrl").
-include_lib("kvs/include/kvs.hrl").

-record(game_table, 
        {
          ?ITERATOR(feed),
          name,
          gameid,
          trn_id,
          game_type,
          rounds :: integer() | 'undefined' | '_',
          sets :: integer() | 'undefined' | '_',
          owner :: username_type() | '_',
          timestamp,
          users = [] :: [username_type()] | '_',
          users_options = [] :: [username_type()] | '_',
          game_mode,
          game_options,
          game_speed,
          friends_only,
          invited_users = [] :: [username_type()] | '_',
          private :: boolean() | '_',
          feel_lucky = false :: boolean(),
          creator,
          age_limit,
          groups_only = [] :: [id_type()] | '_',
          gender_limit,
          location_limit = "",
          paid_only,
          deny_robots = false :: boolean() | '_',
          slang,
          deny_observers,
          gosterge_finish = false :: boolean() | '_',
          double_points = 1 :: integer(),
          game_state,
          game_process :: pid() | '_',
          game_module :: atom(),
          pointing_rules :: any() | '_', %% #pointing_rule{}
          pointing_rules_ex :: [] | '_', %% [#pointing_rule{}] - list of additional pointing rules,
          %% for example IFeelLucky for okey game
          game_process_monitor :: reference() | '_',

          tournament_type = simple :: simple | paired | paired_lobby | tournament,
          robots_replacement_allowed = true :: boolean()
        }).

-record(save_game_table,
        {
          ?ITERATOR(feed),
          uid :: username_type() | '_', %% Dialyzer and record MatchSpec warnings http://j.mp/vZ8670
          name,
          create_time,
          settings
         }
       ).
-endif.
