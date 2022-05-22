# Initialize dictionaries -----------------------
vars_to_initialize <- c("all_seasons", "all_stages", "salaries_distinct", "salaries_seasons",
                        "salaries_most_recent", "all_players_tmp", "general_positions",
                        "all_teams", "all_games", "player_lookup", "all_players", "players_dropdown")

for (v in vars_to_initialize) {
    assign(v, list())
}

# Retrieve data necessary to start app ----------
for (key in league_schemas) {

    # Get distinct seasons --------------------------
    all_seasons[[key]] <- try(api_request(endpoint = assemble_endpoint(key, subheader = "games"), parameters = list(distinct_seasons = TRUE))$season_name)

    # Get distinct stages ---------------------------
    all_stages[[key]] <- try(api_request(endpoint = assemble_endpoint(key, subheader = "games"), parameters = list(distinct_stages = TRUE))$stage_name)

    # Get distinct seasons for salary releases ------
    if (key == "mls") {
        salaries_distinct[[key]] <- try(api_request(endpoint = assemble_endpoint(key, "salaries", "players"), parameters = list(distinct_releases = TRUE)))
        salaries_seasons[[key]] <- try(sort(unique(salaries_distinct[[key]]$season_name)))
        salaries_most_recent[[key]] <- try(max(salaries_distinct[[key]]$mlspa_release))
    } else {
        salaries_distinct[[key]] <- NA
    }

    # Import player demographic data ----------------
    all_players_tmp[[key]] <- try(api_request(endpoint = assemble_endpoint(key, subheader = "players")))

    # Import distinct positions ---------------------
    general_positions[[key]] <- try(api_request(endpoint = assemble_endpoint(key, subheader = "players"), parameters = list(general_position_only = TRUE)))
    general_positions[[key]] <- c("GK",
                                  sort(general_positions[[key]]$general_position[grepl("B", general_positions[[key]]$general_position)]),
                                  sort(general_positions[[key]]$general_position[grepl("M", general_positions[[key]]$general_position)], decreasing = TRUE),
                                  sort(general_positions[[key]]$general_position[!grepl("(B|M|GK)", general_positions[[key]]$general_position)], decreasing = TRUE))

    # Import teams ----------------------------------
    all_teams[[key]] <- try(api_request(endpoint = assemble_endpoint(key, subheader = "teams")))

    # Import game data ------------------------------
    all_games[[key]] <- try(api_request(endpoint = assemble_endpoint(key, subheader = "games")))


    if (all(class(all_seasons[[key]]) == "try-error") | all(class(salaries_distinct[[key]]) == "try-error") |
        all(class(player_lookup[[key]]) == "try-error") | all(class(all_teams[[key]]) == "try-error") |
        all(class(all_games[[key]]) == "try-error")) {

        shiny::stopApp()

    }

    if (length(all_teams[[key]]) > 0) {
        all_teams[[key]] <- all_teams[[key]] %>%
            dplyr::arrange(team_abbreviation)
    }

    if (length(all_players_tmp[[key]]) > 0) {

        # Reshape player ID lookup ----------------------
        player_lookup[[key]] <- all_players_tmp[[key]] %>% select(player_id, player_name)

        # Reshape for dropdown menu ---------------------
        all_players[[key]] <- all_players_tmp[[key]] %>%
            select(-season_name) %>%
            mutate(birth_date = as.Date(birth_date)) %>%
            arrange(player_name)

        players_dropdown[[key]] <- all_players[[key]] %>%
            select(value = player_id,
                   label = player_name)

    }

}
