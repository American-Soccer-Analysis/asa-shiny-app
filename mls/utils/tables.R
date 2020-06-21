# Menu items and options ------------------------
tables_menu_items <-
    list(tables_xgoals = list(name = "xGoals",
                              tabName = "tables_xgoals",
                              icon = "futbol-o",
                              subheaders = c("Players", "Teams", "Goalkeepers", "Games")),
         tables_xpass = list(name = "xPass",
                             tabName = "tables_xpass",
                             icon = "bullseye",
                             subheaders = c("Players", "Teams")),
         tables_goals_added = list(name = "Goals Added (g+)",
                                   tabName = "tables_goals_added",
                                   icon = "google-plus",
                                   subheaders = c("Players")),
         tables_salaries = list(name = "Salaries",
                                tabName = "tables_salaries",
                                icon = "usd",
                                subheaders = c("Players", "Teams")))


# Wrapper div -----------------------------------
tables_div <- div(
    uiOutput("tables_header"),
    uiOutput("tables_subheader"),
    uiOutput("tables_body") %>% withSpinner(color = "#27aae1")
)


# Header ----------------------------------------
tables_header <- function(tab) {
    bs4Box(
        div(class = "header_background",
            h2(tables_menu_items[[tab]][["name"]])),
        width = 12
    )
}


# Subheader -------------------------------------
tables_subheader <- function(tab) {
    bs4Box(
        radioGroupButtons(inputId = "tables_subheader",
                          choices = tables_menu_items[[tab]][["subheaders"]]),
        width = 12
    )
}


# Interactive tables ----------------------------
tables_rv_to_df <- function(header, subheader) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    endpoint <- paste("/mls", subheader, header, sep = "/")
    rv_key <- paste(header, subheader, sep = "_")

    parameters <- tables_rv[[rv_key]]
    parameters <- parameters[!(grepl("data_frame", names(parameters)))]

    false_parameters <- which(sapply(parameters, isFALSE))

    if (length(false_parameters) > 0) {
        parameters <- parameters[-false_parameters]
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

    if ("sort_column" %in% names(parameters)) {
        parameters <- parameters[!(grepl("sort_column", names(parameters)))]
    }

    df <- api_request(endpoint = endpoint, parameters = parameters)

    if (class(df) == "list") {
        return(df)
    }

    if (grepl("goals_added", rv_key)) {

        df <- df %>%
            unnest(data) %>%
            gather(variable, value, -(player_id:action_type)) %>%
            pivot_wider(player_id:team_id, names_from = c(action_type, variable), values_from = value)

        df <- df %>%
            mutate(total_goals_added_above_avg = rowSums(df %>% select(contains("goals_added"))),
                   total_count_actions = rowSums(df %>% select(contains("count_actions"))))

    }

    if ("player_id" %in% names(df) & "team_id" %in% names(df) & !grepl("salaries", rv_key)) {

        if (class(df$team_id) == "list") {

            if ("season_name" %in% names(df)) {

                player_teams <- df %>%
                    select(player_id, season_name, team_id) %>%
                    unnest(team_id) %>%
                    mutate(team_id = all_teams$team_abbreviation[match(team_id, all_teams$team_id)]) %>%
                    group_by(player_id, season_name) %>%
                    summarize(team_id = paste0(team_id, collapse = ", ")) %>%
                    ungroup()

                df <- df %>%
                    rowwise() %>%
                    mutate(team_id = player_teams$team_id[player_teams$player_id == player_id & player_teams$season_name == season_name])

            } else {

                player_teams <- df %>%
                    select(player_id, team_id) %>%
                    unnest(team_id) %>%
                    mutate(team_id = all_teams$team_abbreviation[match(team_id, all_teams$team_id)]) %>%
                    group_by(player_id) %>%
                    summarize(team_id = paste0(team_id, collapse = ", ")) %>%
                    ungroup()

                df <- df %>%
                    mutate(team_id = player_teams$team_id[match(player_id, player_teams$player_id)])

            }

        } else {

            df <- df %>%
                mutate(team_id = all_teams$team_abbreviation[match(team_id, all_teams$team_id)])

        }

    } else if ("team_id" %in% names(df)) {

        df <- df %>%
            mutate(team_id = all_teams$team_abbreviation[match(team_id, all_teams$team_id)])

    } else if ("home_team_id" %in% names(df)) {

        df <- df %>%
            mutate(home_team_id = all_teams$team_abbreviation[match(home_team_id, all_teams$team_id)],
                   away_team_id = all_teams$team_abbreviation[match(away_team_id, all_teams$team_id)])

    }

    if ("player_id" %in% names(df)) {

        df <- df %>%
            mutate(player_id = player_lookup$player_name[match(player_id, player_lookup$player_id)])

    }

    if ("game_id" %in% names(df)) {

        df <- df %>% select(-game_id)

    }

    names(df) <- tables_column_name_map$app_name[match(names(df), tables_column_name_map$api_name)]

    return(df)
}

tables_body <- function(header, subheader, client_timezone) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    rv_key <- paste(header, subheader, sep = "_")

    if (is.null(tables_rv[[rv_key]][["data_frame"]])) {
        tables_rv[[rv_key]][["data_frame"]] <- try(tables_rv_to_df(header, subheader), silent = TRUE)
    }

    df <- tables_rv[[rv_key]][["data_frame"]]

    if (!is.data.frame(df)) {
        bs4Box(
            p("Search yielded zero results."),
            width = 12
        )
    } else {
        df <- df %>% select(-contains("Actions"))

        sort_column <- tables_rv[[rv_key]][["sort_column"]][1]
        sort_column_int <- which(names(df) == sort_column) - 1

        sort_order <- tables_rv[[rv_key]][["sort_column"]][2]

        dt <- DT::datatable(
            df,
            extensions = "Buttons",
            options = list(pageLength = 50,
                           autoWidth = TRUE,
                           dom = "Bfrtip",
                           order = list(list(sort_column_int, sort_order)),
                           buttons = list("copy",
                                          list(extend = "csv",
                                               filename = paste("american_soccer_analysis_mls", header, subheader, format(Sys.time(), "%Y-%m-%d", tz = client_timezone), sep = "_")),
                                          list(extend = 'excel',
                                               filename = paste("american_soccer_analysis_mls", header, subheader, format(Sys.time(), "%Y-%m-%d", tz = client_timezone), sep = "_"),
                                               title = paste0("American Soccer Analysis  |  MLS  |  ", gsub("^Xp", "xP", gsub("^Xg", "xG", toTitleCase(gsub("_", " ", header)))), "  |  ", toTitleCase(subheader)),
                                               messageTop = paste0("Exported on ", format(Sys.time(), "%B %d, %Y", tz = client_timezone), ".")))),
            rownames = FALSE,
            style = "bootstrap4",
            autoHideNavigation = TRUE,
            escape = FALSE,
            selection = "none",
            width = "100%",
            height = "auto"
        )

        if (any(tables_currency_columns %in% names(df))) {
            tmp_columns <- tables_currency_columns[tables_currency_columns %in% names(df)]
            dt <- dt %>% formatCurrency(columns = tmp_columns)
        }

        if (any(tables_percentage_columns %in% names(df))) {
            tmp_columns <- tables_percentage_columns[tables_percentage_columns %in% names(df)]
            dt <- dt %>% formatPercentage(columns = tmp_columns, digits = 1)
        }

        bs4Box(
            div(class = "datatable_wrapper", dt),
            width = 12
        )
    }
}


# Control bar inputs ----------------------------
tables_cb_slider <- function(header, subheader, tables_rv, max_value, label_name, variable_name) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    sliderInput(inputId = paste("tables", header, subheader, variable_name, sep = "_"),
                label = label_name,
                min = 0,
                max = max_value,
                value = tables_rv[[paste(header, subheader, sep = "_")]][[variable_name]],
                step = 1,
                ticks = FALSE,
                width = "100%")
}

tables_cb_date_filter <- function(header, subheader, tables_rv, all_seasons) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    div(
        radioGroupButtons(inputId = paste("tables", header, subheader, "date_type", sep = "_"),
                          label = "Filter Dates By",
                          choices = c("Season", "Date Range"),
                          justified = TRUE,
                          selected = tables_rv[[paste(header, subheader, sep = "_")]][["date_type"]],
                          width = "100%"),
        conditionalPanel(condition = paste0("input.", paste("tables", header, subheader, "date_type", sep = "_"), " == 'Season'"),
                         pickerInput(inputId = paste("tables", header, subheader, "season_name", sep = "_"),
                                     label = "Seasons",
                                     choices = all_seasons,
                                     selected = tables_rv[[paste(header, subheader, sep = "_")]][["season_name"]],
                                     multiple = TRUE,
                                     options = list(`selected-text-format` = "count > 3",
                                                    `actions-box` = TRUE))),
        conditionalPanel(condition = paste0("input.", paste("tables", header, subheader, "date_type", sep = "_"), " == 'Date Range'"),
                         dateRangeInput(inputId = paste("tables", header, subheader, "date_range", sep = "_"),
                                        label = "Date Range",
                                        start = tables_rv[[paste(header, subheader, sep = "_")]][["start_date"]],
                                        end = tables_rv[[paste(header, subheader, sep = "_")]][["end_date"]],
                                        min = paste0(min(all_seasons), "-01-01"),
                                        max = paste0(max(all_seasons), "-12-31"),
                                        separator = "  to  "))
    )
}

tables_cb_picker <- function(header, subheader, tables_rv, label_name, variable_name, available_values, available_names = NULL) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    if (!is.null(available_names)) {
        names(available_values) <- available_names
    }

    pickerInput(inputId = paste("tables", header, subheader, variable_name, sep = "_"),
                label = label_name,
                choices = available_values,
                selected = tables_rv[[paste(header, subheader, sep = "_")]][[variable_name]],
                multiple = TRUE,
                options = list(`selected-text-format` = "count > 3",
                               `actions-box` = TRUE))
}

tables_cb_switch <- function(header, subheader, tables_rv, label_name, variable_name) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    prettyCheckbox(inputId = paste("tables", header, subheader, variable_name, sep = "_"),
                   label = label_name,
                   value = tables_rv[[paste(header, subheader, sep = "_")]][[variable_name]],
                   icon = icon("check"))
}

tables_cb_home_away <- function(header, subheader, tables_rv) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    selected_options <- c()
    for (s in c("home_only", "away_only")) {
        if (tables_rv[[paste(header, subheader, sep = "_")]][[s]]) {
            selected_options <- c(selected_options, s)
        }
    }

    checkboxGroupButtons(inputId = paste("tables", header, subheader, "home_away", sep = "_"),
                         labels = "Filter by Venue",
                         choices = list(home_only = "Home Only",
                                        away_only = "Away Only"),
                         selected = selected_options)
}

tables_cb_refresh <- function(header, subheader) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    actionButton(paste("tables", header, subheader, "refresh", sep = "_"), "Refresh Results", width = "100%")
}


# Control bar -----------------------------------
controlbar_tables <- function(header, subheader, tables_rv) {

    if (any(grepl("xgoals", header))) {
        if (any(grepl("Players", subheader))) {
            div(
                column(12,
                       h4("Player Settings"),
                       tables_cb_slider(header, subheader, tables_rv, MAX_MINUTES,
                                        "Minimum Minutes Played", "minimum_minutes"),
                       tables_cb_slider(header, subheader, tables_rv, MAX_SHOTS_TAKEN_FACED,
                                        "Minimum Shots Taken", "minimum_shots"),
                       tables_cb_slider(header, subheader, tables_rv, MAX_KEY_PASSES,
                                        "Minimum Key Passes", "minimum_key_passes"),
                       tables_cb_picker(header, subheader, tables_rv, "Teams", "team_id",
                                        all_teams$team_id, all_teams$team_abbreviation),
                       tables_cb_picker(header, subheader, tables_rv, "Patterns of Play", "shot_pattern", PATTERNS_OF_PLAY),
                       tables_cb_date_filter(header, subheader, tables_rv, all_seasons),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Teams", "split_by_teams"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Seasons", "split_by_seasons"),
                       tables_cb_refresh(header, subheader)
                )
            )
        } else if (any(grepl("Teams", subheader))) {
            div(
                column(12,
                       h4("Team Settings"),
                       tables_cb_picker(header, subheader, tables_rv, "Patterns of Play", "shot_pattern", PATTERNS_OF_PLAY),
                       tables_cb_date_filter(header, subheader, tables_rv, all_seasons),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Seasons", "split_by_seasons"),
                       p(class = "control-label", "Filter Results by Venue"),
                       tables_cb_switch(header, subheader, tables_rv, "Home Only", "home_only"),
                       tables_cb_switch(header, subheader, tables_rv, "Away Only", "away_only"),
                       p(class = "control-label", "Home-Adjust Results"),
                       tables_cb_switch(header, subheader, tables_rv, "Home Adjustment", "home_adjusted"),
                       p(class = "control-label", "Filter Results by Game State"),
                       tables_cb_switch(header, subheader, tables_rv, "Even Game State Only", "even_game_state"),
                       tables_cb_refresh(header, subheader)
                )
            )
        } else if (any(grepl("Goalkeepers", subheader))) {
            div(
                column(12,
                       h4("Goalkeeper Settings"),
                       tables_cb_slider(header, subheader, tables_rv, MAX_MINUTES,
                                        "Minimum Minutes Played", "minimum_minutes"),
                       tables_cb_slider(header, subheader, tables_rv, MAX_SHOTS_TAKEN_FACED,
                                        "Minimum Shots Faced", "minimum_shots_faced"),
                       tables_cb_picker(header, subheader, tables_rv, "Teams", "team_id",
                                        all_teams$team_id, all_teams$team_abbreviation),
                       tables_cb_picker(header, subheader, tables_rv, "Patterns of Play", "shot_pattern", PATTERNS_OF_PLAY),
                       tables_cb_date_filter(header, subheader, tables_rv, all_seasons),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Teams", "split_by_teams"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Seasons", "split_by_seasons"),
                       tables_cb_refresh(header, subheader)
                )
            )
        } else if (any(grepl("Games", subheader))) {
            div(
                column(12,
                       h4("Game Settings"),
                       tables_cb_date_filter(header, subheader, tables_rv, all_seasons),
                       tables_cb_refresh(header, subheader)
                )
            )
        }
    } else if (any(grepl("xpass", header))) {
        if (any(grepl("Players", subheader))) {
            div(
                column(12,
                       h4("Player Settings"),
                       tables_cb_slider(header, subheader, tables_rv, MAX_MINUTES,
                                        "Minimum Minutes Played", "minimum_minutes"),
                       tables_cb_slider(header, subheader, tables_rv, MAX_PASSES,
                                        "Minimum Key Passes", "minimum_passes"),
                       tables_cb_picker(header, subheader, tables_rv, "Teams", "team_id",
                                        all_teams$team_id, all_teams$team_abbreviation),
                       tables_cb_picker(header, subheader, tables_rv, "Passing Third", "pass_origin_third", THIRDS_OF_FIELD),
                       tables_cb_date_filter(header, subheader, tables_rv, all_seasons),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Teams", "split_by_teams"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Seasons", "split_by_seasons"),
                       tables_cb_refresh(header, subheader)
                )
            )
        } else if (any(grepl("Teams", subheader))) {
            div(
                column(12,
                       h4("Team Settings"),
                       tables_cb_picker(header, subheader, tables_rv, "Passing Third", "pass_origin_third", THIRDS_OF_FIELD),
                       tables_cb_date_filter(header, subheader, tables_rv, all_seasons),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Seasons", "split_by_seasons"),
                       p(class = "control-label", "Filter Results by Venue"),
                       tables_cb_switch(header, subheader, tables_rv, "Home Only", "home_only"),
                       tables_cb_switch(header, subheader, tables_rv, "Away Only", "away_only"),
                       tables_cb_refresh(header, subheader)
                )
            )
        }
    } else if (any(grepl("goals_added", header))) {
        if (any(grepl("Players", subheader))) {
            div(
                column(12,
                       h4("Player Settings"),
                       tables_cb_slider(header, subheader, tables_rv, MAX_MINUTES,
                                        "Minimum Minutes Played", "minimum_minutes"),
                       tables_cb_picker(header, subheader, tables_rv, "Teams", "team_id",
                                        all_teams$team_id, all_teams$team_abbreviation),
                       tables_cb_date_filter(header, subheader, tables_rv, all_seasons),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Teams", "split_by_teams"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Seasons", "split_by_seasons"),
                       tables_cb_refresh(header, subheader)
                )
            )
        }
    } else if (any(grepl("salaries", header))) {
        if (any(grepl("Players", subheader))) {
            div(
                column(12,
                       h4("Player Settings"),
                       tables_cb_picker(header, subheader, tables_rv, "Teams", "team_id",
                                        all_teams$team_id, all_teams$team_abbreviation),
                       tables_cb_picker(header, subheader, tables_rv, "Position", "position", MLSPA_POSITIONS),
                       tables_cb_date_filter(header, subheader, tables_rv, all_seasons),
                       tables_cb_refresh(header, subheader)
                )
            )
        } else if (any(grepl("Teams", subheader))) {
            div(
                column(12,
                       h4("Team Settings"),
                       tables_cb_picker(header, subheader, tables_rv, "Seasons", "season_name", salaries_seasons),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Teams", "split_by_teams"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Seasons", "split_by_seasons"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Positions", "split_by_positions"),
                       tables_cb_refresh(header, subheader)
                )
            )
        }
    }
}


# Column name mapping ---------------------------
tables_column_name_map <- list(
    player_id = "Player",
    team_id = "Team",
    season_name = "Season",
    minutes_played = "Minutes",
    shots = "Shots",
    shots_on_target = "SoT",
    avg_distance_from_goal_yds = "Dist",
    share_unassisted_shots = "Solo",
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
    base_salary = "Base",
    guaranteed_compensation = "Guaranteed",
    mlspa_release = "Date",
    shots_faced = "Shots",
    goals_conceded = "Goals",
    saves = "Saves",
    share_headed_shots = "Header %",
    avg_distance_from_goal_yds = "Dist",
    xgoals_gk_faced = "xG",
    goals_minus_xgoals_gk = "G-xG",
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
    game_date = "Date",
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
    Dribbling_goals_added_above_avg = "Dribbling g+",
    Dribbling_count_actions = "Dribbling Actions",
    Fouling_goals_added_above_avg = "Fouling g+",
    Fouling_count_actions = "Fouling Actions",
    Interrupting_goals_added_above_avg = "Interrupting g+",
    Interrupting_count_actions = "Interrupting Actions",
    Passing_goals_added_above_avg = "Passing g+",
    Passing_count_actions = "Passing Actions",
    Receiving_goals_added_above_avg = "Receiving g+",
    Receiving_count_actions = "Receiving Actions",
    Shooting_goals_added_above_avg = "Shooting g+",
    Shooting_count_actions = "Shooting Actions",
    total_goals_added_above_avg = "Total g+",
    total_count_actions = "All Actions"
)

tables_column_name_map <- data.frame(api_name = names(tables_column_name_map),
                                     app_name = unlist(tables_column_name_map, use.names = FALSE),
                                     stringsAsFactors = FALSE)

# Column type mapping ---------------------------
tables_percentage_columns <- c("Solo", "Pass %", "xPass %", "Touch %", "Header %",
                               "PctF", "xPctF", "PctA", "xPctA")

tables_currency_columns <- c("Base", "Guaranteed", "TotalGuar", "AvgGuar", "MedGuar", "StdDevGuar")

