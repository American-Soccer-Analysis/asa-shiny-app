# Set behavior on refresh -----------------------
tables_refresh <- function(refresh_button_id, input, tables_rv, page, league_config) {
    shinyjs::disable(refresh_button_id)

    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    subheader <- get_values_from_page(page)$subheader

    rv_key <- assemble_key(league, route_prefix, subheader)

    matching_inputs <- paste("tables", route_prefix, subheader, names(tables_rv[[rv_key]]), sep = "_")
    matching_inputs <- matching_inputs[matching_inputs %in% names(input)]


    execute_api_call <- TRUE

    if (any(grepl("date_type", matching_inputs))) {

        if (input[[matching_inputs[grepl("date_type", matching_inputs)]]] == "Date Range") {

            if (sum(is.na(c(input[[matching_inputs[grepl("date_range", matching_inputs)]]][1], input[[matching_inputs[grepl("date_range", matching_inputs)]]][2]))) == 1) {

                sendSweetAlert(
                    session,
                    title = "Error: Date Filter",
                    text = "If filtering by date range, both a start and end date must be included.",
                    type = "error"
                )

                shinyjs::enable(refresh_button_id)

                execute_api_call <- FALSE

            } else if (sum(is.na(c(input[[matching_inputs[grepl("date_range", matching_inputs)]]][1], input[[matching_inputs[grepl("date_range", matching_inputs)]]][2]))) == 0 &
                       input[[matching_inputs[grepl("date_range", matching_inputs)]]][2] < input[[matching_inputs[grepl("date_range", matching_inputs)]]][1]) {

                sendSweetAlert(
                    session,
                    title = "Error: Date Filter",
                    text = "If filtering by date range, the end date must be greater than or equal to the start date.",
                    type = "error"
                )

                shinyjs::enable(refresh_button_id)

                execute_api_call <- FALSE

            }

        }

    }

    if (grepl("salaries/teams", rv_key)) {

        if (sum(c(input[[matching_inputs[grepl("split_by_teams", matching_inputs)]]],
                  input[[matching_inputs[grepl("split_by_seasons", matching_inputs)]]],
                  input[[matching_inputs[grepl("split_by_positions", matching_inputs)]]])) == 0) {

            sendSweetAlert(
                session,
                title = "Error: Grouping Results",
                text = "Results must be grouped by at least one of teams, positions, or seasons.",
                type = "error"
            )

            shinyjs::enable(refresh_button_id)

            execute_api_call <- FALSE

        }

    }

    if (execute_api_call) {

        shinyjs::toggleClass(selector = "body", class = "control-sidebar-slide-open")

        lapply(matching_inputs, function(y) {
            if (grepl("date_range", y)) {
                tables_rv[[rv_key]][["start_date"]] <- input[[y]][1]
                tables_rv[[rv_key]][["end_date"]] <- input[[y]][2]
            } else {
                rv_secondary_key <- gsub(paste0("tables_", route_prefix, "_", subheader, "_"), "", y)
                tables_rv[[rv_key]][[rv_secondary_key]] <- input[[y]]
            }
        })

        tables_rv[[rv_key]][["data_frame"]] <- tables_rv_to_df(page, league_config, tables_rv, input$client_timezone)

        shinyjs::enable(refresh_button_id)

    }
}


# Make API request ------------------------------
tables_rv_to_df <- function(page, league_config, tables_rv, client_timezone, database_timezone = DATABASE_TIMEZONE) {
    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    subheader <- get_values_from_page(page)$subheader

    endpoint <- assemble_endpoint(league, route_prefix, subheader)
    rv_key <- assemble_key(league, route_prefix, subheader)

    parameters <- tables_rv[[rv_key]]
    parameters <- parameters[!(grepl("data_frame", names(parameters)))]

    logical_parameters <- parameters[which(sapply(parameters, class) == "logical")]

    if (length(logical_parameters) > 0) {
        false_parameters <- which(sapply(logical_parameters, isFALSE))

        if (length(false_parameters) > 0) {
            false_parameters <- which(names(parameters) %in% names(false_parameters))
            parameters <- parameters[-false_parameters]
        }
    }

    if ("date_type" %in% names(parameters)) {
        if (parameters[["date_type"]] == "Season") {
            parameters <- parameters[!(grepl("start_date", names(parameters)))]
            parameters <- parameters[!(grepl("end_date", names(parameters)))]
        } else if (parameters[["date_type"]] == "Date Range") {
            parameters <- parameters[!(grepl("season_name", names(parameters)))]
        }
        parameters <- parameters[!(grepl("date_type", names(parameters)))]
    }

    if ("sort_vector" %in% names(parameters)) {
        parameters <- parameters[!(grepl("sort_vector", names(parameters)))]
    }

    if ("goals_added_variation" %in% names(parameters)) {
        if (parameters$goals_added_variation == "Above Replacement") {
            parameters$above_replacement <- TRUE
        }
        parameters <- parameters[!(grepl("goals_added_variation", names(parameters)))]
    }

    if ("normalize_by" %in% names(parameters)) {
        parameters <- parameters[!(grepl("normalize_by", names(parameters)))]
        normalize_variables <- tables_rv[[rv_key]][["normalize_by"]]
    } else {
        normalize_variables <- "None"
    }

    df <- try(api_request(endpoint = endpoint, parameters = parameters))

    if (all(class(df) == "try-error")) {
        stopApp()
    } else if (class(df) == "list") {
        return(df)
    }

    if (grepl("goals_added/players", rv_key)) {

        if (tables_rv[[rv_key]][["goals_added_variation"]] %in% c("Raw", "Above Average")) {

            df <- df %>% unnest(data)

            if (tables_rv[[rv_key]][["goals_added_variation"]] == "Raw") {
                df <- df %>% select(-goals_added_above_avg)
            } else if (tables_rv[[rv_key]][["goals_added_variation"]] == "Above Average") {
                df <- df %>% select(-goals_added_raw)
            }

            df <- df %>%
                gather(variable, value, -(player_id:action_type)) %>%
                pivot_wider(names(df)[which(names(df) %in% c("player_id", "team_id", "season_name", "general_position", "minutes_played"))], names_from = c(action_type, variable), values_from = value)

            df <- df %>%
                mutate(total_goals_added_above_avg = rowSums(df %>% select(contains("goals_added"))),
                       total_count_actions = rowSums(df %>% select(contains("count_actions"))))

        } else if (tables_rv[[rv_key]][["goals_added_variation"]] == "Above Replacement") {

            df <- df %>% select(-goals_added_raw)

        }

    } else if(grepl("goals_added/teams", rv_key)){

        df <- df %>% unnest(data)

        df <- df %>%
            filter(action_type != "Claiming") %>%
            pivot_wider(names_from = action_type,
                        values_from = num_actions_for:goals_added_against,
                        values_fill = list(num_actions_for = 0,
                                           goals_added_for = 0,
                                           num_actions_against = 0,
                                           goals_added_against = 0))

        df <- df %>%
            mutate(total_goals_added_for = rowSums(df %>% select(setdiff(grep("goals_added_for", names(df)), grep("Interrupting", names(df))))),
                   total_count_actions_for = rowSums(df %>% select(setdiff(grep("num_actions_for", names(df)), grep("Interrupting", names(df))))),
                   total_goals_added_against = rowSums(df %>% select(setdiff(grep("goals_added_against", names(df)), grep("Interrupting", names(df))))),
                   total_count_actions_against = rowSums(df %>% select(setdiff(grep("num_actions_against", names(df)), grep("Interrupting", names(df))))))

        if("season_name" %in% names(df)){
            df <- df %>%
                mutate(total_goals_added_differential = total_goals_added_for - total_goals_added_against) %>%
                select(-c(grep("num_actions", names(df), value = T))) %>%
                select(team_id, season_name, minutes,
                       total_goals_added_for, total_goals_added_against, total_goals_added_differential,
                       everything())
        } else
            df <- df %>%
            mutate(total_goals_added_differential = total_goals_added_for - total_goals_added_against) %>%
            select(-c(grep("num_actions", names(df), value = T))) %>%
            select(team_id, minutes,
                   total_goals_added_for, total_goals_added_against, total_goals_added_differential,
                   everything())

    }


    if (names(df)[1] == "player_id") {

        df <- df %>%
            mutate(player_headshot = paste0("<div class = 'tables_player_headshot tables_player_headshot_", league, "'><img src = 'player_headshots/", player_id, ".png'></div>")) %>%
            select(player_headshot, everything())

    } else if (names(df)[1] == "team_id") {

        df <- df %>%
            mutate(club_logo = paste0("<div class = 'tables_club_logo'><img src = 'club_logos/", team_id, ".png'></div>")) %>%
            select(club_logo, everything())
    }


    if ("player_id" %in% names(df) & "team_id" %in% names(df) & !grepl("salaries", rv_key)) {

        if (class(df$team_id) == "list") {

            if ("season_name" %in% names(df)) {

                player_teams <- df %>%
                    select(player_id, season_name, team_id) %>%
                    unnest(team_id) %>%
                    mutate(team_id = all_teams[[league]]$team_abbreviation[match(team_id, all_teams[[league]]$team_id)]) %>%
                    group_by(player_id, season_name) %>%
                    summarize(team_id = paste0(team_id, collapse = ", ")) %>%
                    ungroup()

                df <- df %>%
                    rowwise() %>%
                    mutate(team_id = player_teams$team_id[player_teams$player_id == player_id & player_teams$season_name == season_name]) %>%
                    ungroup()

            } else {

                player_teams <- df %>%
                    select(player_id, team_id) %>%
                    unnest(team_id) %>%
                    mutate(team_id = all_teams[[league]]$team_abbreviation[match(team_id, all_teams[[league]]$team_id)]) %>%
                    group_by(player_id) %>%
                    summarize(team_id = paste0(team_id, collapse = ", ")) %>%
                    ungroup()

                df <- df %>%
                    mutate(team_id = player_teams$team_id[match(player_id, player_teams$player_id)])

            }

        } else {

            df <- df %>%
                mutate(team_id = all_teams[[league]]$team_abbreviation[match(team_id, all_teams[[league]]$team_id)])

        }

    } else if ("team_id" %in% names(df)) {

        df <- df %>%
            mutate(team_id = all_teams[[league]]$team_abbreviation[match(team_id, all_teams[[league]]$team_id)])

    } else if ("home_team_id" %in% names(df)) {

        df <- df %>%
            mutate(home_team_id = all_teams[[league]]$team_abbreviation[match(home_team_id, all_teams[[league]]$team_id)],
                   away_team_id = all_teams[[league]]$team_abbreviation[match(away_team_id, all_teams[[league]]$team_id)])

    }

    if ("player_id" %in% names(df)) {

        df <- df %>%
            mutate(player_id = player_lookup[[league]]$player_name[match(player_id, player_lookup[[league]]$player_id)])

    }

    if ("game_id" %in% names(df)) {

        df <- df %>% select(-game_id)

    }

    if ("date_time_et" %in% names(df)) {

        df <- df %>%
            rowwise() %>%
            mutate(date_time_et = as.POSIXct(date_time_et, tz = database_timezone),
                   date = format(date_time_et, "%Y-%m-%d", tz = client_timezone),
                   time = format(date_time_et, "%H:%M", tz = client_timezone, usetz = TRUE)) %>%
            ungroup() %>%
            select(date, time, everything(), -date_time_et)

    }

    if (normalize_variables == "96 Minutes") {

        tmp_columns <- tables_normalize_columns[tables_normalize_columns %in% names(df)]
        df <- df %>% mutate_at(tmp_columns, function(x) x * 96 / .$Minutes)

    } else if (normalize_variables == "Game") {

        tmp_columns <- tables_normalize_columns[tables_normalize_columns %in% names(df)]
        df <- df %>% mutate_at(tmp_columns, function(x) x / .$Games)

    }

    return(df)
}
