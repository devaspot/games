-include("basic_types.hrl").

% actions are being instantiated from client to server

-record(session_attach, {token}).
-record(login, {username, password}).
-record(logout, {}).
-record(join_game, {game}).
-record(stats_action, {player_id :: 'PlayerId'(), game_type}).
-record(chat, {chat_id :: 'GameId'(), message :: string()}).
-record(game_action, {game :: 'GameId'(), action, args = []}).
-record(social_action, {game :: 'GameId'(), type, recipient::'PlayerId'()}).

% event notifications from server to client

-record(game_event, {game :: 'GameId'(), event, args = [] }).
-record(chat_event, {chat :: 'GameId'(), content, author_id::'PlayerId'(),author_nick::string() }).
-record(social_event, {type,game::'GameId'(),initiator::'PlayerId'(),recipient::'PlayerId'()}).
-record(stats_event, {player_id :: 'PlayerId'(), games, reveals, protocol}).
-record(pause_game, {table_id :: integer(),game :: 'GameId'(),action}).
-record(game_paused, {table_id :: integer(), game :: 'GameId'(),action,who :: 'PlayerId'(),retries}).
-record(disconnect, {reason_id,reason}).
-record(player_left, {player :: 'PlayerId'(),human_replaced=false,replacement::'PlayerId'()}).
