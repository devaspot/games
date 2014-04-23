-include("basic_types.hrl").
-include("types.hrl").

-record(session_attach, {
          token :: string() %% shared secret, stored in auth_server
         }).

-record(session_attach_debug, {
          token :: string(), %% shared secret, stored in auth_server
          id :: string()
         }).

-record(login, {
          username :: string(),
          password :: string()
         }).

-record(logout, {}).

-record(join_game, {
          game :: 'GameId'()
         }).

-record(rematch, {
          game :: 'GameId'()
         }).

-record(get_game_info, {
          game :: 'GameId'()
         }).

-record(get_player_info, {
          player_id :: 'PlayerId'() | 0 %% 0 stands for currently logged in user
         }).

-record(get_player_stats, {
          player_id :: 'PlayerId'() | 0, %% 0 stands for currently logged in user
          game_type :: binary()
         }).

-record(subscribe_player_rels, {
          players :: list()
         }).

-record(unsubscribe_player_rels, {
          players :: list()
         }).

-record(chat, {
          chat_id :: 'GameId'(),
          message :: string()
         }).

-record(game_action, {
          game      :: 'GameId'(),
          action    :: any(),
          args = [] :: proplist()
         }).
%%%
%%% Events, passed via #KakaMessage
%%%
-record(game_event, {
          game      :: 'GameId'(),
          event     :: any(),
          args = [] :: proplist()
         }).

-record(dummy_player_change, {
          player :: 'PlayerId'()
         }).

-record(chat_msg, {
          chat        :: 'GameId'(),
          content     :: string(),
          author_id   :: 'PlayerId'(),
          author_nick :: string()
         }).

-record(social_action, {
          game :: 'GameId'(),
          type :: 'SocialActionEnum'(),
          recipient :: 'PlayerId'() | null
         }).

-record(social_action_msg, {
          type :: 'SocialActionEnum'(),
          game :: 'GameId'(),
          initiator :: 'PlayerId'(),
          recipient :: 'PlayerId'() | null
         }).

-record(pause_game, {
          table_id :: integer(),
          game    :: 'GameId'(),
          action  :: string()
         }).

-record(game_paused, {
          table_id :: integer(),
          game    :: 'GameId'(),
          action  :: string(),
          who     :: 'PlayerId'(),
          retries :: integer()
         }).

-record(disconnect, {
          reason_id = null :: null | string(),
          reason = null :: null | string()
         }).

%% packet as game_event:
-record(player_left, {
          player :: 'PlayerId'(),
          bot_replaced = false :: boolean(),    %% will be replaced by bot?
          human_replaced = false :: boolean(),  %% will be replaced by human?
          replacement = null :: 'PlayerId'() | null %% id of replacement player/bot
         }).

-record('TableInfo', {
          chat    = []      :: list(any()),
          viewers = []      :: list('PlayerId'()),
          players = []      :: list('PlayerId'()),
          game              :: atom(),
          game_status       :: atom()
         }).

