-record(player_scoring, {
    id,
    temp, 
    permanent, % top of Permanent Scoring record linked list
    agregated_score %-- aggregated score for all game types is is a list
                    %  [{game_okey, 15},{game_tavla, 12},...]

    }).

-record(scoring_record, {
    id,          % id of that record
    next,        % next record for traversal
    prev,
    game_id,     % game id for rematching and lost connections
    who,         % player
    all_players, % with other players
    game_type,   % okey, tavla, batak 
    game_kind,   % chanak, standard, even-odd
    condition,   % reveal with even tashes, color okey reveal, show gosterge, batak 3 aces, tavla mars.
    score_points,       % result score points for player
    score_kakaush,      % result score kakuş for player
    custom,      % erlang record for a specific game
    timestamp    % now() of the record
    }).

-record(personal_score, { % total count of everything
        uid,
        games = 0,  
        wins = 0,
        loses = 0,
        disconnects = 0,
        points = 0,
        average_time = 0
    }).
