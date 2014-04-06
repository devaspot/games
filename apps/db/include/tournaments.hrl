-record(team,
        {name, % { team name for now will bu just first player username }
         id,
         play_record, % { linked list history of played users under that ticket }
         type }).

-record(tournament,
        {name, % { tournament name }
         id,
         game_type,
         description,
         creator,
         created,
         start_date,
         start_time,
         end_date,
         status, % { activated, ongoing, finished, canceled }
         quota,
         tours,
         awards, 
         winners :: list(), % [{UserId, Position, GiftId}]
         waiting_queue, % { play_record, added here when user wants to join tournament }
         avatar,
         owner,
         players_count,
         speed,
         type,
         game_mode }). % { eliminatin, pointing, etc }

-record(play_record, % { tournament_player, game_record, tournament_info, choose your name :) }
        {who, % { user }
         tournament, % { tournament in which user played }
         team, % { team under which user player tournament }
         game_id, % { game id that user played under that team }

         realname,
         game_points,
         kakush,
         kakush_currency,
         quota,
         other }).

