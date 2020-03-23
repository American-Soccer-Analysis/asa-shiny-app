# Import player demographic data ----------------
all_players <- dbGetQuery(pool, "SELECT * FROM mls.players
                                 LEFT JOIN mls.players_positions USING(player_id)
                                 ORDER BY player_name")

# Reshape for dropdown menu ---------------------
players_dropdown <- all_players %>%
     select(value = player_id,
            label = player_name,
            url = headshot_url)

all_players_seasons <- dbGetQuery(pool, "SELECT DISTINCT player_id, season_name
                                         FROM mls.formations
                                              LEFT JOIN mls.games USING(game_id)
                                         ORDER BY player_id, season_name")


# Import game data ------------------------------
all_games <- dbGetQuery(pool, "SELECT * FROM mls.games
                               ORDER BY date_time_et, game_id") %>%
     mutate(playoff_game = grepl("Playoff", stage_name)) %>%
     select(game_id:expanded_minutes, season_name, attendance:playoff_game)


# Import player stats by season -----------------
future({
    all_players_stats <- dbGetQuery(pool, "SELECT player_stats_per_game.*, xg
                                           FROM mls.player_stats_per_game
                                                LEFT JOIN mls.player_xgoals_per_game USING (player_id, game_id)") %>%
         left_join(all_games %>% select(game_id, season_name, playoff_game), "game_id")

    # xGoals
    all_players_xgoals <- all_players_stats %>%
         filter(playoff_game == FALSE) %>%
         group_by(player_id, season_name) %>%
         summarize(xg = sum(xg, na.rm = TRUE),
                   expanded_minutes_played = sum(expanded_minutes_played, na.rm = TRUE)) %>%
         ungroup() %>%
         mutate(xg_p96 = xg / expanded_minutes_played * 96) %>%
         left_join(all_players %>% select(player_id, player_name, broad_position), "player_id")

    # Involvement
    touches_team_game <- all_players_stats %>%
         filter(playoff_game == FALSE) %>%
         group_by(game_id, team_id) %>%
         summarize(team_touches = sum(touches, na.rm = TRUE)) %>%
         ungroup()

    all_players_involvement <- all_players_stats %>%
         filter(playoff_game == FALSE) %>%
         left_join(touches_team_game, c("game_id", "team_id")) %>%
         mutate(share_team_touches = touches / team_touches) %>%
         group_by(player_id, season_name) %>%
         summarize(involvement = weighted.mean(share_team_touches, expanded_minutes_played) * 100,
                   expanded_minutes_played = sum(expanded_minutes_played, na.rm = TRUE)) %>%
         ungroup() %>%
         left_join(all_players %>% select(player_id, player_name, broad_position), "player_id")

    # Progressive Passes
    all_players_progressive_passes <- all_players_stats %>%
         filter(playoff_game == FALSE) %>%
         group_by(player_id, season_name) %>%
         summarize(progressive_passes = sum(progressive_passes, na.rm = TRUE),
                   expanded_minutes_played = sum(expanded_minutes_played, na.rm = TRUE)) %>%
         ungroup() %>%
         mutate(progressive_passes_p96 = progressive_passes / expanded_minutes_played * 96) %>%
         left_join(all_players %>% select(player_id, player_name, broad_position), "player_id")

    # Defensive Actions
    all_players_defensive_actions <- all_players_stats %>%
         filter(playoff_game == FALSE) %>%
         mutate(defensive_actions = tackles + interceptions + blocked_passes + clearances) %>%
         group_by(player_id, season_name) %>%
         summarize(defensive_actions = sum(defensive_actions, na.rm = TRUE),
                   expanded_minutes_played = sum(expanded_minutes_played, na.rm = TRUE)) %>%
         ungroup() %>%
         mutate(defensive_actions_p96 = defensive_actions / expanded_minutes_played * 96) %>%
         left_join(all_players %>% select(player_id, player_name, broad_position), "player_id")

    # Tackle Success
    all_players_tackle_success <- all_players_stats %>%
         filter(playoff_game == FALSE) %>%
         group_by(player_id, season_name) %>%
         summarize(successful_tackles = sum(successful_tackles, na.rm = TRUE),
                   tackles = sum(tackles, na.rm = TRUE),
                   expanded_minutes_played = sum(expanded_minutes_played, na.rm = TRUE)) %>%
         ungroup() %>%
         mutate(tackle_success = successful_tackles / tackles * 100) %>%
         left_join(all_players %>% select(player_id, player_name, broad_position), "player_id")

    # Recoveries
    all_players_recoveries <- all_players_stats %>%
         filter(playoff_game == FALSE) %>%
         group_by(player_id, season_name) %>%
         summarize(recoveries = sum(recoveries, na.rm = TRUE),
                   expanded_minutes_played = sum(expanded_minutes_played, na.rm = TRUE)) %>%
         ungroup() %>%
         mutate(recoveries_p96 = recoveries / expanded_minutes_played * 96) %>%
         left_join(all_players %>% select(player_id, player_name, broad_position), "player_id")
})
