-module(known_records).

-include("types.hrl").
-include("requests.hrl").
-include("game_okey.hrl").
-include("game_tavla.hrl").

-export([fields/1]).

-record(unknown_data,{}).

fields(T) when is_tuple(T) ->  fields(element(1, T));

fields(game_action) ->             record_info(fields, game_action);
fields(join_game) ->               record_info(fields, join_game);
fields(get_game_info) ->           record_info(fields, get_game_info);
fields(dummy_player_change) ->     record_info(fields, dummy_player_change);
fields(login) ->                   record_info(fields, login);
fields(session_attach) ->          record_info(fields, session_attach);
fields(logout) ->                  record_info(fields, logout);
fields(chat) ->                    record_info(fields, chat);
fields(get_player_stats) ->        record_info(fields, get_player_stats);
fields(subscribe_player_rels) ->   record_info(fields, subscribe_player_rels);
fields(unsubscribe_player_rels) -> record_info(fields, unsubscribe_player_rels);
fields('PlayerInfo') ->            record_info(fields, 'PlayerInfo');
fields('TableInfo') ->             record_info(fields, 'TableInfo');
fields(game_event) ->              record_info(fields, game_event);
fields(chat_msg) ->                record_info(fields, chat_msg);
fields(social_action) ->           record_info(fields, social_action);
fields(social_action_msg) ->       record_info(fields, social_action_msg);
fields(pause_game) ->              record_info(fields, pause_game);
fields(game_paused) ->             record_info(fields, game_paused);
fields(disconnect) ->              record_info(fields, disconnect);
fields(player_left) ->             record_info(fields, player_left);
fields(okey_debug) ->              record_info(fields, okey_debug);
fields(session_attach_debug) ->    record_info(fields, session_attach_debug);
fields('OkeyPiece') ->             record_info(fields, 'OkeyPiece');
fields('OkeySeriesResult') ->      record_info(fields, 'OkeySeriesResult');
fields('OkeyGameResults') ->       record_info(fields, 'OkeyGameResults');
fields('OkeyGameR') ->             record_info(fields, 'OkeyGameR');
fields('OkeyScoringDetail') ->     record_info(fields, 'OkeyScoringDetail');
fields('PlayerOkeyStats') ->       record_info(fields, 'PlayerOkeyStats');
fields('PlayerTavlaStats') ->      record_info(fields, 'PlayerTavlaStats');
fields(okey_ready) ->              record_info(fields, okey_ready);
fields(okey_has_gosterge) ->       record_info(fields, okey_has_gosterge);
fields(okey_discard) ->            record_info(fields, okey_discard);
fields(okey_reveal) ->             record_info(fields, okey_reveal);
fields(okey_surrender) ->          record_info(fields, okey_surrender);
fields(okey_take) ->               record_info(fields, okey_take);
fields(okey_i_saw_okey) ->         record_info(fields, okey_i_saw_okey);
fields(okey_i_have_8_tashes) ->    record_info(fields, okey_i_have_8_tashes);
fields(okey_challenge) ->          record_info(fields, okey_challenge);
fields(okey_game_info) ->          record_info(fields, okey_game_info);
fields('OkeyTimeouts') ->          record_info(fields, 'OkeyTimeouts');
%%fields('TavlaTimeouts') ->         record_info(fields, 'TavlaTimeouts');
fields(okey_game_started) ->       record_info(fields, okey_game_started);
fields(okey_game_player_state) ->  record_info(fields, okey_game_player_state);
fields(okey_player_ready) ->       record_info(fields, okey_player_ready);
fields(okey_player_has_gosterge) ->record_info(fields, okey_player_has_gosterge);
fields(okey_player_has_8_tashes) ->record_info(fields, okey_player_has_8_tashes);
fields(okey_next_turn) ->          record_info(fields, okey_next_turn);
fields(okey_tile_taken) ->         record_info(fields, okey_tile_taken);
fields(okey_disable_okey) ->       record_info(fields, okey_disable_okey);
fields(okey_turn_timeout) ->       record_info(fields, okey_turn_timeout);
fields(okey_tile_discarded) ->     record_info(fields, okey_tile_discarded);
fields(okey_revealed) ->           record_info(fields, okey_revealed);
fields(okey_round_ended) ->        record_info(fields, okey_round_ended);
fields(okey_series_ended) ->       record_info(fields, okey_series_ended);
fields(okey_turn_result) ->        record_info(fields, okey_turn_result);
fields(okey_turn_record) ->        record_info(fields, okey_turn_record);
fields(okey_playing_tables) ->     record_info(fields, okey_playing_tables);

fields('TavlaAtomicMoveServer') -> record_info(fields, 'TavlaAtomicMoveServer');
fields(tavla_checkers) ->          record_info(fields, tavla_checkers);
fields(tavla_game_info) ->         record_info(fields, tavla_game_info);
%%fields('TavlaPlayer') ->           record_info(fields, 'TavlaPlayer');
fields('TavlaPlayerScore') ->      record_info(fields, 'TavlaPlayerScore');
fields(tavla_color_info) ->        record_info(fields, tavla_color_info);
fields('TavlaGameResults') ->      record_info(fields, 'TavlaGameResults');
fields('TavlaSeriesResult') ->     record_info(fields, 'TavlaSeriesResult');
fields(tavla_player_ready) ->      record_info(fields, tavla_player_ready);
fields(tavla_game_started) ->      record_info(fields, tavla_game_started);
fields(tavla_game_player_state) -> record_info(fields, tavla_game_player_state);
fields(tavla_won_first_move) ->    record_info(fields, tavla_won_first_move);
fields(tavla_next_turn) ->         record_info(fields, tavla_next_turn);
fields(tavla_turn_timeout) ->      record_info(fields, tavla_turn_timeout);
fields(tavla_rolls) ->             record_info(fields, tavla_rolls);
fields(tavla_moves) ->             record_info(fields, tavla_moves);
%%fields(tavla_vidoes) ->            record_info(fields, tavla_vidoes);
%%fields(tavla_accepts) ->           record_info(fields, tavla_accepts);
%%fields(tavla_timeouts) ->          record_info(fields, tavla_timeouts);
fields(tavla_game_ended) ->        record_info(fields, tavla_game_ended);
fields(tavla_series_ended) ->      record_info(fields, tavla_series_ended);
fields(tavla_ready) ->             record_info(fields, tavla_ready);
fields(tavla_roll) ->              record_info(fields, tavla_roll);
fields(tavla_move) ->              record_info(fields, tavla_move);
fields('TavlaAtomicMove') ->       record_info(fields, 'TavlaAtomicMove');
fields(tavla_skip) ->              record_info(fields, tavla_skip);
fields(tavla_vido) ->              record_info(fields, tavla_vido);
fields(tavla_vido_request) ->      record_info(fields, tavla_vido_request);
fields(tavla_vido_answer) ->       record_info(fields, tavla_vido_answer);
fields(tavla_accepts_vido) ->      record_info(fields, tavla_accepts_vido);
fields(tavla_surrender) ->         record_info(fields, tavla_surrender);
fields(tavla_surrender_answer) ->  record_info(fields, tavla_surrender_answer);
fields(tavla_surrender_request) -> record_info(fields, tavla_surrender_request);
fields(tavla_ack) ->               record_info(fields, tavla_ack);
%%fields(tavla_board) ->             record_info(fields, tavla_board);
fields(tavla_accept_timeout) ->    record_info(fields, tavla_accept_timeout);
fields(tavla_tour_result) ->       record_info(fields, tavla_tour_result);
fields(tavla_tour_record) ->       record_info(fields, tavla_tour_record);

fields(_) -> record_info(fields,unknown_data).
