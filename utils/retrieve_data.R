# Initialize dictionaries -----------------------
vars_to_initialize <- c("all_seasons", "salaries_distinct", "salaries_seasons", "salaries_most_recent",
                        "player_lookup", "general_positions", "all_teams", "recent_games")

for (v in vars_to_initialize) {
    assign(v, list())
}

# Retrieve data necessary to start app ----------
for (key in league_schemas) {

    # Get distinct seasons --------------------------
    all_seasons[[key]] <- try(api_request(endpoint = assemble_endpoint(key, subheader = "games"), parameters = list(distinct_seasons = TRUE))$season_name)

    # Get distinct stages ---------------------------
    league_config[[key]][["stages"]] <- try(unique(api_request(endpoint = assemble_endpoint(key, subheader = "games"), parameters = list(distinct_stages = TRUE))$stage_name))

    # Get distinct seasons for salary releases ------
    if (key == "mls") {
        salaries_distinct[[key]] <- try(api_request(endpoint = assemble_endpoint(key, "salaries", "players"), parameters = list(distinct_releases = TRUE)))
        salaries_seasons[[key]] <- try(sort(unique(salaries_distinct[[key]]$season_name)))
        salaries_most_recent[[key]] <- try(max(salaries_distinct[[key]]$mlspa_release))
    } else {
        salaries_distinct[[key]] <- NA
    }

    # Import player ID lookup -----------------------
    player_lookup[[key]] <- try(api_request(endpoint = assemble_endpoint(key, subheader = "players"), parameters = list(lookup_only = TRUE)))

    # Import distinct positions ---------------------
    general_positions[[key]] <- try(api_request(endpoint = assemble_endpoint(key, subheader = "players"), parameters = list(general_position_only = TRUE)))
    general_positions[[key]] <- c("GK",
                                  sort(general_positions[[key]]$general_position[grepl("B", general_positions[[key]]$general_position)]),
                                  sort(general_positions[[key]]$general_position[grepl("M", general_positions[[key]]$general_position)], decreasing = TRUE),
                                  sort(general_positions[[key]]$general_position[!grepl("(B|M|GK)", general_positions[[key]]$general_position)], decreasing = TRUE))

    # Import teams ----------------------------------
    all_teams[[key]] <- try(api_request(endpoint =assemble_endpoint(key, subheader = "teams")) %>% arrange(team_abbreviation))

    # Import game data ------------------------------
    recent_games[[key]] <- try(api_request(endpoint = assemble_endpoint(key, subheader = "games"), parameters = list(limit = 20)))


    if (all(class(all_seasons[[key]]) == "try-error") | all(class(salaries_distinct[[key]]) == "try-error") |
        all(class(player_lookup[[key]]) == "try-error") | all(class(all_teams[[key]]) == "try-error") |
        all(class(recent_games[[key]]) == "try-error")) {

        stopApp()

    }

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
