bo <- 1

while (bo != 4) {

    # Get distinct seasons --------------------------
    all_seasons <- try(api_request(endpoint = "games", parameters = list(distinct_seasons = TRUE)))
    all_seasons <- try(sort(all_seasons$season_name))

    # Get distinct seasons for salary releases ------
    if (LEAGUE_SCHEMA == "mls") {
        salaries_distinct <- try(api_request(endpoint = "players/salaries", parameters = list(distinct_releases = TRUE)))
        salaries_seasons <- try(sort(unique(salaries_distinct$season_name)))
        salaries_most_recent <- try(max(salaries_distinct$mlspa_release))
    } else {
        salaries_distinct <- NA
    }

    # Import player ID lookup -----------------------
    player_lookup <- try(api_request(endpoint = "players", parameters = list(lookup_only = TRUE)))

    # Import teams ----------------------------------
    all_teams <- try(api_request(endpoint = "teams") %>% arrange(team_abbreviation))

    # Import game data ------------------------------
    recent_games <- try(api_request(endpoint = "games", parameters = list(limit = 20)))


    if (all(class(all_seasons) == "try-error") | all(class(salaries_distinct) == "try-error") |
        all(class(player_lookup) == "try-error") | all(class(all_teams) == "try-error") |
        all(class(recent_games) == "try-error")) {

        bo <- bo + 1

    } else break

}

if (all(class(all_seasons) == "try-error") | all(class(salaries_distinct) == "try-error") |
    all(class(player_lookup) == "try-error") | all(class(all_teams) == "try-error") |
    all(class(recent_games) == "try-error")) {

    stopApp()

}




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
