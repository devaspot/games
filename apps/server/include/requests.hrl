-include("basic_types.hrl").

% actions are being instantiated from client to server

-record(session_attach, {token}).
-record(login, {username, password}).
-record(logout, {}).
-record(join_game, {game}).
-record(player_stats, {player_id :: 'PlayerId'() | 0, game_type}).
-record(chat, {chat_id :: 'GameId'(), message :: string()}).
-record(game_action, {game :: 'GameId'(), action, args = []}).
-record(social_action, {game :: 'GameId'(), type, recipient::'PlayerId'()}).

% event notifications from server to client

-record(game_event, {game :: 'GameId'(), event, args = [] }).
-record(chat_event, {chat :: 'GameId'(), content, author_id::'PlayerId'(),author_nick::string() }).
-record(social_event, {type,game::'GameId'(),initiator::'PlayerId'(),recipient::'PlayerId'()}).
-record(pause_game, {table_id :: integer(),game :: 'GameId'(),action}).
-record(game_paused, {table_id :: integer(), game :: 'GameId'(),action,who :: 'PlayerId'(),retries}).
-record(disconnect, {reason_id,reason}).
-record(player_left, {player :: 'PlayerId'(),human_replaced=false,replacement::'PlayerId'()}).
