# Get distinct seasons --------------------------
all_seasons <- api_request(endpoint = "games", parameters = list(distinct_seasons = TRUE))
all_seasons <- sort(all_seasons$season_name)

# Get distinct seasons for salary releases ------
if (LEAGUE_SCHEMA == "mls") {
    salaries_distinct <- api_request(endpoint = "players/salaries", parameters = list(distinct_releases = TRUE))
    salaries_seasons <- sort(unique(salaries_distinct$season_name))
    salaries_most_recent <- max(salaries_distinct$mlspa_release)
}

# Import player ID lookup -----------------------
player_lookup <- api_request(endpoint = "players", parameters = list(lookup_only = TRUE))

# Import teams ----------------------------------
all_teams <- api_request(endpoint = "teams") %>%
    arrange(team_abbreviation)

# Import game data ------------------------------
recent_games <- api_request(endpoint = "games", parameters = list(limit = 20))




# # Import player demographic data ----------------
# all_players_tmp <- api_request(endpoint = "players")
#
# all_players <- all_players_tmp %>%
#     select(-season_name) %>%
#     mutate(birth_date = as.Date(birth_date)) %>%
#     arrange(player_name)
#
# # Reshape for dropdown menu ---------------------
# players_dropdown <- all_players %>%
#      mutate(url = paste0("player_headshots/", player_id, ".png")) %>%
#      select(value = player_id,
#             label = player_name,
#             url)
#
# all_players_seasons <- all_players_tmp %>%
#     select(player_id, season_name) %>%
#     unnest(season_name)

# # Import player stats by season -----------------
# all_players_stats <- api_request(endpoint = "players/stats") %>%
#     left_join(all_players %>% select(player_id, player_name, broad_position), "player_id")
