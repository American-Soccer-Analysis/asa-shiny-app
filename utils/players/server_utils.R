# -----------------------------------------------
# VIOLINS ---------------------------------------
# -----------------------------------------------

# Set behavior on refresh -----------------------
profiles_players_violin_refresh <- function(refresh_button_id, players_rv, page) {
    shinyjs::disable(refresh_button_id)

    league <- get_values_from_page(page)$league
    player_id <- players_rv[[league]][["profiles_players_name"]]

    matching_inputs <- paste("profiles_players_violin", names(players_rv[[league]][[player_id]]), sep = "_")
    matching_inputs <- matching_inputs[matching_inputs %in% names(input)]
    matching_inputs <- c(matching_inputs, gsub("date_type$", "date_range", matching_inputs[grepl("date_type", matching_inputs)]))

    lapply(matching_inputs, function(y) {
        if (grepl("date_range", y)) {
            players_rv[[league]][[player_id]][["start_date"]] <- input[[y]][1]
            players_rv[[league]][[player_id]][["end_date"]] <- input[[y]][2]
        } else {
            rv_secondary_key <- gsub("profiles_players_violin_", "", y)
            players_rv[[league]][[player_id]] <- input[[y]]
        }
    })

    players_rv[[league]][[player_id]][["data_frame"]] <- players_rv_to_violins_df(page, players_rv)

    shinyjs::enable(refresh_button_id)
}

# Make API request ------------------------------
players_rv_to_violins_df <- function(page, players_rv) {
    league <- get_values_from_page(page)$league
    endpoint <- assemble_endpoint(league, "stats", "players")
    player_id <- players_rv[[league]][["profiles_players_name"]]

    parameters <- players_rv[[league]][[player_id]]
    parameters <- parameters[!(grepl("data_frame", names(parameters)))]

    if (parameters[["date_type"]] == "Season") {
        parameters <- parameters[!(grepl("start_date", names(parameters)))]
        parameters <- parameters[!(grepl("end_date", names(parameters)))]
    } else if (parameters[["date_type"]] == "Date Range") {
        parameters <- parameters[!(grepl("season_name", names(parameters)))]
    }
    parameters <- parameters[!(grepl("date_type", names(parameters)))]

    df <- try(api_request(endpoint = endpoint, parameters = parameters))

    if (all(class(df) == "try-error")) {
        stopApp()
    } else if (class(df) == "list") {
        return(df)
    }

    if (class(df$team_id) == "list") {

        player_teams <- df %>%
            select(player_id, team_id) %>%
            unnest(team_id) %>%
            mutate(team_id = all_teams[[league]]$team_abbreviation[match(team_id, all_teams[[league]]$team_id)]) %>%
            group_by(player_id) %>%
            summarize(team_id = paste0(team_id, collapse = ", "), .groups = "drop")

        df <- df %>%
            mutate(team_id = player_teams$team_id[match(player_id, player_teams$player_id)])

    } else {

        df <- df %>%
            mutate(team_id = all_teams[[league]]$team_abbreviation[match(team_id, all_teams[[league]]$team_id)])

    }

    df <- df %>%
        mutate(player_name = player_lookup[[league]]$player_name[match(player_id, player_lookup[[league]]$player_id)],
               current_player = player_id == players_rv[[league]][["profiles_players_name"]]) %>%
        select(-player_id)

    return(df)

}
