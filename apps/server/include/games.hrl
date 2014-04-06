%%%
%%% Message is sent when matchmaker finds match.
%%%
-record('PlayerResults', {
          player_id,           %%
          disconnected = false :: boolean(),
          winner = <<"none">>  :: binary(), %% similar to skill_delta
          skill,               %% skill level the start of the game
          skill_delta,         %% 0 for defeat, 0.25 for draw and 1 for win
          score = 0,           %% total accumulated score for this set
          score_delta = 0     %% delta of okey game points
         }).

-record('GameResults', {
          game_id :: integer(),
          results = []       :: list(#'PlayerResults'{})
         }).
