
-type 'Color'()   :: integer(). %% 'black' or 'red'
-type 'Position'()   :: integer(). %% 'black' or 'red'

%%%%%%%%%%%%%%%%%%%%%%%
%% Events data types
%%%%%%%%%%%%%%%%%%%%%%%

%%-record('TavlaPlace', { count :: integer } ).

-record('TavlaAtomicMoveServer',
         {from :: 'Position'(),
          to   :: 'Position'(),
          hits :: boolean(),
          pips :: integer()
         }).

-record(tavla_checkers,
        {color :: integer(),
         number :: integer()
        }).

-record(tavla_tour_record, {
          player_id       :: 'PlayerId'(),
          place           :: integer(),
          score           :: integer(),
          status          :: atom() | binary() %% active | eliminated 
         }).


-record('TavlaPlayerScore', {
           player_id      :: 'PlayerId'(),
%%           reason         :: atom(),
           winner = <<"none">> :: binary(),
           score_delta    :: integer(),
           score          :: integer()
                         }).

-record('TavlaSeriesResult', {
          player_id :: 'PlayerId'(),
          place :: integer(),
          winner = <<"none">> :: binary(),
          score :: integer()
                             }).

-record('PlayerTavlaStats', {
          playerId, %% : int
          playerName, %% : String;
          level, %% : int; Number
          levelName, %% : String;
          badges, %% : Array; Array of int between [1; 5],
          skill,     %% : int;
          score,     %% : int;
          totalWins, %% : int;
          totalLose, %% : int;
          totalDisconnects, %% : int;
          overalSuccessRatio, %% : Number;
          averagePlayDuration %% : Number;
          }).

-record(tavla_color_info, {
          name :: any(),
          color :: integer()
                     }).

%% -record(tavla_board, {
%%           id :: integer(),
%%           name :: any(),
%%           players :: list(#'PlayerInfo'{}),
%%           main = false :: boolean()
%%                      }).

-record('TavlaGameResults', {
%%           game_id :: integer(),
           players :: list(#'TavlaPlayerScore'{})
                     }).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%     EVENTS      %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(tavla_game_info, {
          table_id          :: integer(),
%%          game_type         :: atom(), %% TODO: Remove
          table_name        :: binary(),
          game_mode = undefined :: atom(),
          sets              :: null | integer(), %% total number of sets
          set_no            :: null | integer(), %% number of current set
          tables_num        :: integer(),
%%          current_round     :: integer(), %% TODO: Remove
          rounds            :: integer(),
          players           :: list(#'PlayerInfo'{}),
          speed             :: atom(),      %% [slow, normal, fast, blitz]
          turn_timeout      :: integer(),   %% timeout value for player turn
%%          challenge_timeout :: integer(),   %% TODO: Remove, timeout value for challenge 
          ready_timeout     :: integer(),   %% timeout value for ready msg
%%          timeout           :: integer(),   %% TODO: Remove
          mul_factor        :: pos_integer(),
          slang_flag        :: boolean(),
          observer_flag     :: boolean(),
          pause_enabled = true :: boolean(),
          social_actions_enabled = true :: boolean(),
          tournament_type = standalone :: atom(), %% standalone | elimination | pointing | lucky
          series_confirmation_mode = yes_exit :: yes_exit | no_exit | no
                         }).

-record(tavla_player_ready, {
          table_id  :: integer(),
          player :: 'PlayerId'()
                     }).

-record(tavla_game_started, {
          table_id       :: integer(),
          board          :: list(#tavla_checkers{} | null),
%%          another_boards :: list(#tavla_board{}),
          players        :: list(#tavla_color_info{}), %% TODO: Rename to players_colors
          current_round  :: integer(),
          round_timeout  :: null | integer(),
          set_timeout    :: null | integer(),
          do_first_move_competition_roll :: boolean()
          }).

-record(tavla_game_player_state, {
          table_id             :: integer(),
          board                :: null | list(#tavla_checkers{} | null),
          dice                 :: list(null | integer()),
          players_colors       :: list(#tavla_color_info{}),
          whos_move            :: list(integer()), %% Color
          game_state           :: initializing | first_move_competition | waiting_for_roll | waiting_for_move | finished,
          current_round        :: integer(),
          next_turn_in         :: integer() | infinity, %% milliseconds
          paused = false       :: boolean(),
          round_timeout = null :: null | integer(),
          set_timeout = null   :: null | integer()
         }).

-record(tavla_won_first_move, {
          table_id  :: integer(),
          color     :: integer(),
          player    :: 'PlayerId'(),
          dice      :: list(integer()),
          reroll    :: boolean()
                     }).

-record(tavla_next_turn, {
          table_id  :: integer(),
          color     :: integer(),
          player    :: 'PlayerId'()
                     }).

-record(tavla_rolls, {
          table_id  :: integer(),
          color     :: integer(),
          player    :: 'PlayerId'(),
          dices     :: list(integer())
                     }).

-record(tavla_moves, {
          table_id     :: integer(),
          color        :: integer(),
          player       :: 'PlayerId'(),
          from         :: 'Position'(),
          to           :: 'Position'(),
          hits = false :: boolean(),
          pips         :: integer()
                          }).

-record(tavla_turn_timeout, {
          table_id     :: integer(),
          color        :: integer(),
          player       :: 'PlayerId'(),
          dice         :: null | list(integer()),
          moves        :: null | list(#'TavlaAtomicMoveServer'{})
                            }).


%-record(tavla_vidoes, {
%          table_id  :: integer(),
%          player   :: 'PlayerId'()
%                      }).

%-record(tavla_accepts, {
%          table_id  :: integer(),
%          player   :: 'PlayerId'(),
%          accept   :: boolean()
%                      }).

%-record(tavla_timeouts, {
%          table_id  :: integer(),
%          player  :: 'PlayerId'()
%                      }).

-record(tavla_series_ended, {
          table_id  :: integer(),
          standings :: list(#'TavlaSeriesResult'{})
                            }).

-record(tavla_game_ended, {
          table_id  :: integer(),
          reason  :: binary(), %% "win", "round_timeout", "set_timeout"
          winner  :: null | 'PlayerId'(),
          results :: #'TavlaGameResults'{}
                     }).

-record(tavla_tour_result, {
          table_id         :: integer(),
          tour_num         :: integer(),
          records          :: list(#tavla_tour_record{})
         }).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%     ACTION      %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(tavla_ready, {table_id  :: integer()}).

-record(tavla_roll, {table_id :: any()}).

-record('TavlaAtomicMove', {from :: 'Position'(), to :: 'Position'() } ).
-record(tavla_move, { table_id  :: integer(), moves :: list(#'TavlaAtomicMove'{}), player :: 'PlayerId'() }).

-record(tavla_skip, {table_id  :: integer()}).


-record(tavla_request, {table_id  :: integer()}).

-record(tavla_vido, {table_id :: integer()}).

-record(tavla_vido_request, { table_id :: integer(),
          from :: 'PlayerId'(),
          to   :: 'PlayerId'()
                    }).

-record(tavla_vido_answer, { table_id  :: integer(),
          from :: 'PlayerId'(),
          to   :: 'PlayerId'(),
          answer :: boolean()
                    }).

-record(tavla_ack, { table_id  :: integer(),
       type :: atom(),
       from :: 'PlayerId'(),
       to   :: 'PlayerId'(),
       answer :: boolean()}).

-record(tavla_accepts_vido, { table_id  :: integer(),
          accept :: boolean()
                    }).

-record(tavla_surrender, { table_id  :: integer()}).

-record(tavla_surrender_request, { table_id  :: integer(),
          from :: 'PlayerId'(),
          to   :: 'PlayerId'()
                    }).

-record(tavla_surrender_answer, { table_id  :: integer(),
          from :: 'PlayerId'(),
          to   :: 'PlayerId'(),
          answer :: boolean()
                    }).

-record(tavla_accept_timeout, { table_id  :: integer(),
          accept = true:: boolean()
         }).

%% -record('TavlaSetState', {
%%           round_cur,
%%           round_max,
%%           set_cur,
%%           set_max
%%          }).

%% -record('TavlaTimeouts', {
%%           speed             :: atom(),      %% [slow, normal, fast, blitz]
%%           turn_timeout      :: integer(),   %% timeout value for player turn
%%           challenge_timeout :: integer(),   %% timeout value for challenge
%%           ready_timeout     :: integer(),   %% timeout value for ready msg
%%           rematch_timeout   :: integer()    %% timeout value for general api #rematch{} msg
%%          }).
