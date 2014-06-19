%% data structures send over the wire
-record('OkeyPiece', {
          color = -1 :: integer(),           %% 1..4
          value = -1 :: integer()            %% 1..13
          %% color set to 1 and value set to zero mean that this is false okey
         }).

-define(FALSE_OKEY, #'OkeyPiece'{value = 0, color = 1}).

-record('OkeyScoringDetail', {
          reason = [] :: list(atom()),
          score  = 0  :: integer()
         }).
-record('OkeyGameR', {
          player_id,           %%
          disconnected = false :: boolean(),
          winner = <<"none">>  :: binary(), %% similar to skill_delta
          good_shot            :: boolean(),
          skill,               %% skill level the start of the game
          skill_delta,         %% 0 for defeat, 0.25 for draw and 1 for win
          score = 0,           %% total accumulated score for this set
          score_delta = 0,     %% delta of okey game points
          breakdown = []       :: list(#'OkeyScoringDetail'{}) %% breakdown of okey game points
         }).
-record('OkeySeriesResult', {
          player_id :: 'PlayerId'(),
          place :: integer(),
          winner = <<"none">> :: binary(),
          score :: integer()
         }).
-record('OkeyGameResults', {
          game_id :: integer(),
          start_datetime = 0 :: integer(),
          end_datetime = 0   :: integer(),
          results = []       :: list(#'OkeyGameR'{}),
          series_results = [] :: list(#'OkeySeriesResult'{})
         }).
-record('PlayerOkeyStats', {
          playerId, %% : int
          playerName, %% : String;
          level, %% : int; Number
          levelName, %% : String;
          badges, %% : Array; Array of int between [1; 5],
          %%from 1 to 5 items in array

          %%Okey Game Stats
          skill,     %% : int;
          score,     %% : int;
          totalWins, %% : int;
          totalLose, %% : int;
          totalDisconnects, %% : int;
          overalSuccessRatio, %% : Number;
          averagePlayDuration, %% : Number;

          %%Okey Game Detailed Stats
          number8Tashes, %% : int;
          numberColor, %% : int;
          numberColorOkey, %% : int;
          numberColorOdd, %% : int;
          numberColorOddOkey, %% : int;
          numberOkey, %% : int;
          numberOdd %% : int;
         }).


%% incoming messages, wrapped in #kaka_game_action
-record(okey_ready, {}).
-record(okey_has_gosterge, {}).
-record(okey_discard, {
          tile :: #'OkeyPiece'{}
         }).
-record(okey_reveal, {
          discarded :: #'OkeyPiece'{},
          hand :: list(#'OkeyPiece'{} | null)
         }).
-record(okey_surrender, {
         }).
-record(okey_take, {
          pile :: integer() %% 0 or 1
         }).
-record(okey_challenge, {
          challenge = false :: boolean()
         }).
-record(okey_i_saw_okey, {}).
-record(okey_i_have_8_tashes, {}).

-record('OkeyTimeouts', {
          speed             :: atom(),      %% [slow, normal, fast, blitz]
          turn_timeout      :: integer(),   %% timeout value for player turn
          challenge_timeout :: integer(),   %% timeout value for challenge
          ready_timeout     :: integer(),   %% timeout value for ready msg
          rematch_timeout   :: integer()    %% timeout value for general api #rematch{} msg
         }).

-type tournament() :: standalone | elimination | pointing | lucky.

%% outgoing messages; wrapped into #'KakaMessage'
-record(okey_game_info, {
          players :: list(#'PlayerInfo'{}),
          timeouts :: #'OkeyTimeouts'{},   %% timeout value for player turn
          game_type :: atom(),
          finish_with_gosterge :: boolean(),
          pairs = null :: null | list(list('PlayerId'())),
          table_name :: binary(),
          sets   :: integer(), %% number of sets defined for this table
          set_no :: integer(), %% number of current set
          rounds :: integer(), %% number of rounds in this set
          mul_factor    :: pos_integer(),
          slang_flag    :: boolean(),
          observer_flag :: boolean(),
          pause_enabled = true :: boolean(),
          social_actions_enabled = true :: boolean(),
          tournament_type = standalone :: tournament(),
          series_confirmation_mode = yes_exit :: yes_exit | no_exit | no
         }).
-record(okey_player_ready, {
          player :: 'PlayerId'()
         }).
-record(okey_player_has_gosterge, {
          player :: 'PlayerId'()
         }).
-record(okey_game_started, {
          tiles         :: list(#'OkeyPiece'{}),
          gosterge      :: #'OkeyPiece'{},
          pile_height   :: integer(),
          current_round :: integer,
          current_set   :: integer,
          game_type     = null :: atom(), %% FIXME Deprecated
          game_speed    = null :: atom(), %% FIXME Deprecated
          game_submode  = null :: atom(), %% FIXME Deprecated
          chanak_points :: integer(),
          round_timeout = null :: null | integer(),
          set_timeout = null :: null | integer()
         }).

-record(okey_game_player_state, {
          whos_move     :: 'PlayerId'(),
          game_state    :: atom(),
          piles         :: list(#'OkeyPiece'{} | null),
          %% piles are in counterclock-wise order,
          %% with addressie pile being first one
          %% (the pile he is taking tiles from)
          tiles         :: list(#'OkeyPiece'{}),
          gosterge      :: #'OkeyPiece'{},
          pile_height   :: integer(),
          current_round :: integer(),
          game_sub_type = null :: atom(), %% FIXME Deprecated
          next_turn_in  :: integer() | atom(),
          %% number of milliseconds until next turn or 'infinity'
          paused = false :: boolean(),
          chanak_points  = 0 :: integer(),
          round_timeout = null :: null | integer(),
          set_timeout = null :: null | integer()
         }).

-record(okey_next_turn, {
          player                :: 'PlayerId'(),
          can_challenge = false :: boolean(),
          okey_button
          }).

-record(okey_enable, {
          player :: 'PlayerId'(),
          enabled,
          who :: list('PlayerId'())
         }).

-record(okey_tile_taken, {
          player :: 'PlayerId'(),
          pile   ::  integer(),
          revealed    :: #'OkeyPiece'{},
          pile_height :: integer()
          }).

-record(okey_turn_timeout, {
          player :: 'PlayerId'(),
          tile_taken = null :: #'OkeyPiece'{} | null,
          tile_discarded    :: #'OkeyPiece'{}
          }).

-record(okey_tile_discarded, {
          player            :: 'PlayerId'(),
          tile              :: #'OkeyPiece'{},
          timeouted = false :: boolean
         }).

-record(okey_revealed, {
          player    :: 'PlayerId'(),
          discarded :: #'OkeyPiece'{},
          hand      :: list(#'OkeyPiece'{} | null)
         }).

-record(okey_round_ended, {
%          good_shot         :: boolean(),
          round,
          reason            :: atom(),
          results           :: #'OkeyGameResults'{},
          next_action       :: atom() | binary()
         }).

-record(okey_series_ended, {
          standings :: list(#'OkeySeriesResult'{}),
          dialog_type = yes_no :: yes_no | ok
         }).

-record(okey_turn_record, {
          player_id       :: 'PlayerId'(),
          place           :: integer(),
          score           :: integer(),
          status          :: atom() | binary() %% active | eliminated 
         }).

-record(okey_turn_result, {
          turn_num         :: integer(),
          records          :: list(#okey_turn_record{})
         }).

-record(okey_player_has_8_tashes, {
           player          :: 'PlayerId'(),
           value           :: integer()   %% 1 - 13
         }).

-record(okey_playing_tables, {
          num              :: integer()
         }).

%%%%%
%%%%%  Debug
%%%%%
-record(okey_debug, {}).

-record(wrong_reveal, {}).

-record(okey_player, {
          pid                      :: pid(),
          player_id                :: 'PlayerId'(),
          player_info              :: #'PlayerInfo'{},
          hand                     :: list(#'OkeyPiece'{}),
          pile = []                :: list(#'OkeyPiece'{}),  %% a pile player draws from; aka pile number 1
          skill                    :: integer(),
          can_show_gosterge = true :: boolean()
         }).

-record('OkeySetState', {
          round_cur,
          round_max,
          set_cur,
          set_max
         }).
