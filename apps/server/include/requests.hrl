-include("basic_types.hrl").

% actions are being instantiated from client to server

-record(session_attach, {token}).
-record(login, {username, password}).
-record(logout, {}).
-record(join_game, {game}).
-record(stats_action, {player_id :: 'PlayerId'(), game_type}).
-record(chat, {game, player_id, who, message }).
-record(game_action, {game :: 'GameId'(), action, args = []}).
-record(pause_game, {table_id :: integer(),game :: 'GameId'(),action}).

% event notifications from server to client

-record(game_event, {game :: 'GameId'(), event, args = [] }).
-record(chat_event, {game :: 'GameId'(), player_id :: 'PlayerId'(), who, message }).
-record(stats_event, {player_id :: 'PlayerId'(), games, reveals, protocol, score}).
-record(game_paused, {table_id :: integer(), game :: 'GameId'(),action,who :: 'PlayerId'(),retries}).
-record(disconnect, {reason_id,reason}).
-record(player_left, {player :: 'PlayerId'(),human_replaced=false,replacement::'PlayerId'()}).
