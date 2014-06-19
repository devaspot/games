-module(known_records).

-include("requests.hrl").
-include_lib("db/include/journal.hrl"). 
-include("game_okey.hrl").
-include("game_tavla.hrl").

-export([fields/1]).

-record(unknown_data,{}).

fields(T) when is_tuple(T) ->      fields(element(1, T));
fields(game_action) ->             record_info(fields, game_action);
fields(join_game) ->               record_info(fields, join_game);
fields(login) ->                   record_info(fields, login);
fields(session_attach) ->          record_info(fields, session_attach);
fields(logout) ->                  record_info(fields, logout);
fields(chat) ->                    record_info(fields, chat);
fields(stats_action) ->             record_info(fields, stats_action);
fields(stats_event) ->            record_info(fields, stats_event);
fields(reveal_event) ->            record_info(fields, reveal_event);
fields('PlayerInfo') ->            record_info(fields, 'PlayerInfo');
fields(game_event) ->              record_info(fields, game_event);
fields(chat_event) ->                record_info(fields, chat_event);
fields(pause_game) ->              record_info(fields, pause_game);
fields(game_paused) ->             record_info(fields, game_paused);
fields(disconnect) ->              record_info(fields, disconnect);
fields(player_left) ->             record_info(fields, player_left);

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
fields(wrong_reveal) ->            record_info(fields, wrong_reveal);
fields(okey_take) ->               record_info(fields, okey_take);
fields(okey_i_saw_okey) ->         record_info(fields, okey_i_saw_okey);
fields(okey_i_have_8_tashes) ->    record_info(fields, okey_i_have_8_tashes);
fields(okey_challenge) ->          record_info(fields, okey_challenge);
fields(okey_game_info) ->          record_info(fields, okey_game_info);
fields('OkeyTimeouts') ->          record_info(fields, 'OkeyTimeouts');
fields(okey_game_started) ->       record_info(fields, okey_game_started);
fields(okey_game_player_state) ->  record_info(fields, okey_game_player_state);
fields(okey_player_ready) ->       record_info(fields, okey_player_ready);
fields(okey_player_has_gosterge) ->record_info(fields, okey_player_has_gosterge);
fields(okey_player_has_8_tashes) ->record_info(fields, okey_player_has_8_tashes);
fields(okey_next_turn) ->          record_info(fields, okey_next_turn);
fields(okey_tile_taken) ->         record_info(fields, okey_tile_taken);
fields(okey_enable) ->             record_info(fields, okey_enable);
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
fields(tavla_accept_timeout) ->    record_info(fields, tavla_accept_timeout);
fields(tavla_tour_result) ->       record_info(fields, tavla_tour_result);
fields(tavla_tour_record) ->       record_info(fields, tavla_tour_record);
fields(_) ->                       record_info(fields, unknown_data).
