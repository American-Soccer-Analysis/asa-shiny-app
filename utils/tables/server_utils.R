# Set behavior on refresh -----------------------
tables_refresh <- function(refresh_button_id, input, tables_rv, page, league_config) {
    shinyjs::disable(refresh_button_id)

    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    subheader <- get_values_from_page(page)$subheader

    rv_key <- assemble_key(league, route_prefix, subheader)

    matching_inputs <- paste("tables", route_prefix, subheader, names(tables_rv[[rv_key]]), sep = "_")
    matching_inputs <- matching_inputs[!grepl("sort_vector", matching_inputs)]


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

    names(df) <- tables_column_name_map$app_name[match(names(df), tables_column_name_map$api_name)]

    if (normalize_variables == "96 Minutes") {

        tmp_columns <- tables_normalize_columns[tables_normalize_columns %in% names(df)]
        df <- df %>% mutate_at(tmp_columns, function(x) x * 96 / .$Minutes)

    } else if (normalize_variables == "Game") {

        tmp_columns <- tables_normalize_columns[tables_normalize_columns %in% names(df)]
        df <- df %>% mutate_at(tmp_columns, function(x) x / .$Games)

    }

    return(df)
}


# Column name mapping ---------------------------
tables_column_name_map <- list(
    player_id = "Player",
    team_id = "Team",
    general_position = "Position",
    season_name = "Season",
    minutes_played = "Minutes",
    shots = "Shots",
    shots_on_target = "SoT",
    goals = "G",
    xgoals = "xG",
    xplace = "xPlace",
    goals_minus_xgoals = "G-xG",
    key_passes = "KeyP",
    primary_assists = "A",
    xassists = "xA",
    primary_assists_minus_xassists = "A-xA",
    xgoals_plus_xassists = "xG+xA",
    points_added = "PA",
    xpoints_added = "xPA",
    attempted_passes = "Passes",
    pass_completion_percentage = "Pass %",
    xpass_completion_percentage = "xPass %",
    passes_completed_over_expected = "Score",
    passes_completed_over_expected_p100 = "Per100",
    avg_distance_yds = "Distance",
    avg_vertical_distance_yds = "Vertical",
    share_team_touches = "Touch %",
    position = "Position",
    base_salary = "Base Salary",
    guaranteed_compensation = "Guaranteed Compensation",
    mlspa_release = "Date",
    shots_faced = "Shots Faced",
    goals_conceded = "Goals Conceded",
    saves = "Saves",
    share_headed_shots = "Header %",
    xgoals_gk_faced = "xG",
    goals_minus_xgoals_gk = "G-xG",
    goals_divided_by_xgoals_gk = "G/xG",
    count_games = "Games",
    shots_for = "ShtF",
    shots_against = "ShtA",
    goals_for = "GF",
    goals_against = "GA",
    goal_difference = "GD",
    xgoals_for = "xGF",
    xgoals_against = "xGA",
    xgoal_difference = "xGD",
    goal_difference_minus_xgoal_difference = "GD-xGD",
    points = "Pts",
    xpoints = "xPts",
    attempted_passes_for = "PassF",
    pass_completion_percentage_for = "PctF",
    xpass_completion_percentage_for = "xPctF",
    passes_completed_over_expected_for = "ScoreF",
    passes_completed_over_expected_p100_for = "Per100F",
    avg_vertical_distance_for = "VertF",
    attempted_passes_against = "PassA",
    pass_completion_percentage_against = "PctA",
    xpass_completion_percentage_against = "xPctA",
    passes_completed_over_expected_against = "ScoreA",
    passes_completed_over_expected_p100_against = "Per100A",
    avg_vertical_distance_against = "VertA",
    passes_completed_over_expected_difference = "ScoreDiff",
    avg_vertical_distance_difference = "VertDiff",
    count_players = "N",
    total_guaranteed_compensation = "TotalGuar",
    avg_guaranteed_compensation = "AvgGuar",
    median_guaranteed_compensation = "MedGuar",
    std_dev_guaranteed_compensation = "StdDevGuar",
    date = "Date",
    time = "Time",
    home_team_id = "Home",
    home_goals = "HG",
    home_team_xgoals = "HxGt",
    home_player_xgoals = "HxGp",
    away_team_id = "Away",
    away_goals = "AG",
    away_team_xgoals = "AxGt",
    away_player_xgoals = "AxGp",
    goal_difference = "GD",
    team_xgoal_difference = "xGDt",
    player_xgoal_difference = "xGDp",
    final_score_difference = "Final",
    home_xpoints = "HxPts",
    away_xpoints = "AxPts",
    Dribbling_goals_added_raw = "Dribbling",
    Fouling_goals_added_raw = "Fouling",
    Interrupting_goals_added_raw = "Interrupting",
    Passing_goals_added_raw = "Passing",
    Receiving_goals_added_raw = "Receiving",
    Shooting_goals_added_raw = "Shooting",
    total_goals_added_raw = "Goals Added",
    Dribbling_goals_added_above_avg = "Dribbling",
    Dribbling_count_actions = "Dribbling Actions",
    Fouling_goals_added_above_avg = "Fouling",
    Fouling_count_actions = "Fouling Actions",
    Interrupting_goals_added_above_avg = "Interrupting",
    Interrupting_count_actions = "Interrupting Actions",
    Passing_goals_added_above_avg = "Passing",
    Passing_count_actions = "Passing Actions",
    Receiving_goals_added_above_avg = "Receiving",
    Receiving_count_actions = "Receiving Actions",
    Shooting_goals_added_above_avg = "Shooting",
    Shooting_count_actions = "Shooting Actions",
    total_goals_added_above_avg = "Goals Added",
    total_count_actions = "All Actions",
    goals_added_above_replacement = "Goals Added",
    count_actions = "All Actions",

    minutes = "Minutes",
    goals_added_for_Dribbling = "DribblingF",
    num_actions_for_Dribbling = "Dribbling ActionsF",
    goals_added_for_Fouling = "FoulingF",
    num_actions_for_Fouling = "Fouling ActionsF",
    goals_added_for_Interrupting = "InterruptingF",
    num_actions_for_Interrupting = "Interrupting ActionsF",
    goals_added_for_Passing = "PassingF",
    num_actions_for_Passing = "Passing ActionsF",
    goals_added_for_Receiving = "ReceivingF",
    num_actions_for_Receiving = "Receiving ActionsF",
    goals_added_for_Shooting = "ShootingF",
    num_actions_for_Shooting = "Shooting ActionsF",
    total_goals_added_for = "Goals AddedF",
    total_count_actions_for = "All ActionsF",

    goals_added_against_Dribbling = "DribblingA",
    num_actions_against_Dribbling = "Dribbling ActionsA",
    goals_added_against_Fouling = "FoulingA",
    num_actions_against_Fouling = "Fouling ActionsA",
    goals_added_against_Interrupting = "InterruptingA",
    num_actions_against_Interrupting = "Interrupting ActionsA",
    goals_added_against_Passing = "PassingA",
    num_actions_against_Passing = "Passing ActionsA",
    goals_added_against_Receiving = "ReceivingA",
    num_actions_against_Receiving = "Receiving ActionsA",
    goals_added_against_Shooting = "ShootingA",
    num_actions_against_Shooting = "Shooting ActionsA",
    total_goals_added_against = "Goals AddedA",
    total_count_actions_against = "All ActionsA",
    total_goals_added_differential = "Goals Added diff"
)

tables_column_name_map <- data.frame(api_name = names(tables_column_name_map),
                                     app_name = unlist(tables_column_name_map, use.names = FALSE),
                                     stringsAsFactors = FALSE)

# Column tooltip text ---------------------------
tables_column_tooltip_text <- list(
    Minutes = "Includes stoppage time.",
    SoT = "Shots on Target",
    G = "Goals",
    xG = "xGoals",
    xPlace = "Difference between post- and pre-shot xG models.",
    KeyP = "Key Passes",
    A = "Primary Assists",
    xA = "xAssists",
    PA = "Expected points added through scoring goals.",
    xPA = "Expected points added through taking shots.",
    `Pass %` = "Pass Completion",
    `xPass %` = "Expected pass completion percentage.",
    Score = "Number of passes completed over/under expected.",
    Per100 = "Passes completed over/under expected, measured per 100 passes.",
    Distance = "Average distance of completed passes, measured in yards. Assumes 115x80 field dimensions.",
    Vertical = "Average vertical distance of completed passes, measured in yards. Assumes 115x80 field dimensions.",
    `Touch %` = "Players' share of their team's number of touches.",
    `Header %` = "Share of shots faced that were headed.",
    ShtF = "Shots For",
    ShtA = "Shots Against",
    GF = "Goals For (Own goals excluded.)",
    GA = "Goals Against (Own goals excluded.)",
    GD = "Goal Difference (Own goals excluded.)",
    xGF = "xGoals For (Own goals excluded.)",
    xGA = "xGoals Against (Own goals excluded.)",
    xGD = "xGoal Difference (Own goals excluded.)",
    xPts = "Expected points earned, given the same sample of shots over 1,000 simulations.",
    PctF = "Pass Completion",
    xPctF = "Expected pass completion percentage.",
    ScoreF = "Number of passes completed over/under expected.",
    Per100F = "Passes completed over/under expected, measured per 100 passes.",
    VertF = "Average vertical distance of completed passes, measured in yards. Assumes 115x80 field dimensions.",
    PctA = "Pass Completion",
    xPctA = "Expected pass completion percentage.",
    ScoreA = "Number of passes completed over/under expected.",
    Per100A = "Passes completed over/under expected, measured per 100 passes.",
    VertA = "Average vertical distance of completed passes, measured in yards. Assumes 115x80 field dimensions.",
    ScoreDiff = "Number of passes completed over/under expected.",
    VertDiff = "Average vertical distance of completed passes, measured in yards. Assumes 115x80 field dimensions.",
    TotalGuar = "Sum of Guaranteed Compensation",
    AvgGuar = "Average Guaranteed Compensation",
    MedGuar = "Median Guaranteed Compensation",
    StdDevGuar = "Standard Deviation of Guaranteed Compensation",
    HxGt = "Team xGoals (Conditional probabilities are applied to ensure the xGoals value of a single possession never exceeds 1.)",
    HxGp = "Player xGoals",
    AxGt = "Team xGoals (Conditional probabilities are applied to ensure the xGoals value of a single possession never exceeds 1.)",
    AxGp = "Player xGoals",
    xGDt = "Team xGoals (Conditional probabilities are applied to ensure the xGoals value of a single possession never exceeds 1.)",
    xGDp = "Player xGoals",
    Final = "Final Score (Own goals included.)",
    HxPts = "Expected points earned, given the same sample of shots over 1,000 simulations.",
    AxPts = "Expected points earned, given the same sample of shots over 1,000 simulations.",
    `Goals Added` = "Calculated each game against the position at which the player lined up.",
    `Goals AddedF` = "Ignores Interrupting g+.",
    `Goals AddedA` = "Ignores Interrupting g+.",
    `Goals Added diff` = "Ignores Interrupting g+."
)

tables_column_tooltip_text <- data.frame(app_name = names(tables_column_tooltip_text),
                                         tooltip_text = unlist(tables_column_tooltip_text, use.names = FALSE),
                                         stringsAsFactors = FALSE)


# Column type mapping ---------------------------
tables_percentage_columns <- c("Solo", "Pass %", "xPass %", "Touch %", "Header %",
                               "PctF", "xPctF", "PctA", "xPctA")

tables_currency_columns <- c("Base Salary", "Guaranteed Compensation",
                             "TotalGuar", "AvgGuar", "MedGuar", "StdDevGuar")

tables_normalize_columns <- c("Shots", "SoT", "G", "xG", "xPlace", "G-xG", "KeyP",
                              "A", "xA", "A-xA", "xG+xA", "PA", "xPA", "Passes",
                              "Score", "Shots Faced", "Goals Conceded", "Saves",
                              "xG", "G-xG", "ShtF", "ShtA", "GF", "GA", "GD",
                              "xGF", "xGA", "xGD", "GD-xGD", "Pts", "xPts",
                              "PassF", "ScoreF", "PassA", "ScoreA", "ScoreDiff",
                              "Dribbling", "Fouling", "Interrupting", "Passing",
                              "Receiving", "Shooting", "Goals Added",

                              "DribblingF", "FoulingF", "InterruptingF", "PassingF",
                              "ReceivingF", "ShootingF", "Goals AddedF",

                              "DribblingA", "FoulingA", "InterruptingA", "PassingA",
                              "ReceivingA", "ShootingA", "Goals AddedA", "Goals Added diff")
