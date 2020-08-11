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
                          choices = tables_menu_items[[tab]][["subheaders"]],
                          selected = tables_menu_items[[tab]][["subheaders"]][1]),
        width = 12
    )
}


# Interactive tables ----------------------------
tables_rv_to_df <- function(header, subheader) {
    header <- gsub("^tables_", "", header)

    if (is_empty(subheader)) {
        return(list())
    }

    subheader <- tolower(subheader)

    endpoint <- paste(subheader, header, sep = "/")
    rv_key <- paste(header, subheader, sep = "_")

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

    if ("sort_column" %in% names(parameters)) {
        parameters <- parameters[!(grepl("sort_column", names(parameters)))]
    }

    if ("normalize_by" %in% names(parameters)) {
        parameters <- parameters[!(grepl("normalize_by", names(parameters)))]
        normalize_variables <- tables_rv[[rv_key]][["normalize_by"]]
    } else {
        normalize_variables <- "None"
    }

    bo <- 1

    while (bo != 4) {
        df <- try(api_request(endpoint = endpoint, parameters = parameters))

        if (all(class(df) == "try-error")) {
            bo <- bo + 1
        } else break
    }

    if (all(class(df) == "try-error")) {
        stopApp()
    } else if (class(df) == "list") {
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

        if (normalize_variables == "Action") {

            gplus_variables <- names(df)[grepl("goals_added_above_avg", names(df))]

            for (g in gplus_variables) {
                df[[g]] <- df[[g]] / df[[gsub("goals_added_above_avg", "count_actions", g)]]
            }

        }

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
                    mutate(team_id = player_teams$team_id[player_teams$player_id == player_id & player_teams$season_name == season_name]) %>%
                    ungroup()

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

    if (normalize_variables == "96 Minutes") {

        tmp_columns <- tables_normalize_columns[tables_normalize_columns %in% names(df)]
        df <- df %>% mutate_at(tmp_columns, function(x) x * 96 / .$Minutes)

    } else if (normalize_variables == "Game") {

        tmp_columns <- tables_normalize_columns[tables_normalize_columns %in% names(df)]
        df <- df %>% mutate_at(tmp_columns, function(x) x / .$Games)

    }

    return(df)
}

tables_body <- function(header, subheader, client_timezone) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    rv_key <- paste(header, subheader, sep = "_")

    if (is.null(tables_rv[[rv_key]][["data_frame"]])) {
        tables_rv[[rv_key]][["data_frame"]] <- try(tables_rv_to_df(header, subheader))
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
        
        old_names <- names(df)

        for (i in 1:length(names(df))) {
            tmp_match <- match(names(df)[i], tables_column_tooltip_text$app_name)

            if (!is.na(tmp_match)) {
                names(df)[i] <- paste0(names(df)[i], "-", "<span class=\"tables_helper_tooltip\">", tables_column_tooltip_text$tooltip_text[tmp_match], "</span>")
            }
        }

        dt <- DT::datatable(
            df,
            extensions = "Buttons",
            options = list(pageLength = 50,
                           autoWidth = TRUE,
                           dom = "Bfrtip",
                           order = list(list(sort_column_int, sort_order)),
                           
                           buttons = list("copy",
                                          list(extend = "csv",
                                               filename = paste("american_soccer_analysis", LEAGUE_SCHEMA, header, subheader, format(Sys.time(), "%Y-%m-%d", tz = client_timezone), sep = "_")),
                                          list(extend = 'excel',
                                               filename = paste("american_soccer_analysis", LEAGUE_SCHEMA, header, subheader, format(Sys.time(), "%Y-%m-%d", tz = client_timezone), sep = "_"),
                                               title = paste0("American Soccer Analysis  |  ", toupper(LEAGUE_SCHEMA), "  |  ", gsub("^Xp", "xP", gsub("^Xg", "xG", toTitleCase(gsub("_", " ", header)))), "  |  ", toTitleCase(subheader)),
                                               exportOptions = JS("
                                                                  {
      format: {
        header: function (data) {
          return $('<span></span>')
            .append(data)
            .find('.tables_helper_tooltip')
            .remove()
            .end()
            .text()
          }
        }   
      }"),
                                               messageTop = paste0("Exported on ", format(Sys.time(), "%B %d, %Y", tz = client_timezone), ".")))),
            rownames = FALSE,
            style = "bootstrap4",
            autoHideNavigation = TRUE,
            escape = FALSE,
            selection = "none",
            width = "100%",
            height = "auto"
        )

        if (any(unlist(lapply(df, class), use.names = FALSE) == "numeric")) {
            tmp_columns <- which(unlist(lapply(df, class), use.names = FALSE) == "numeric")
            dt <- dt %>% formatRound(columns = tmp_columns, digits = 2)
        }

        if (any(gsub("<.*>$", "", names(df)) %in% tables_currency_columns)) {
            tmp_columns <- which(gsub("<.*>$", "", names(df)) %in% tables_currency_columns)
            dt <- dt %>% formatCurrency(columns = tmp_columns, digits = 0)
        }

        if (any(gsub("<.*>$", "", names(df)) %in% tables_percentage_columns)) {
            tmp_columns <- which(gsub("<.*>$", "", names(df)) %in% tables_percentage_columns)
            dt <- dt %>% formatPercentage(columns = tmp_columns, digits = 1)
        }

        bs4Box(
            panel("Click the settings option (the gear icon) in the top-right corner to tailor your results.",
                  status = "info",
                  heading = HTML("<span class=\"glyphicon\">&#xe086;</span>Filtering")),
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

tables_cb_radio <- function(header, subheader, tables_rv, label_name, variable_name, available_values, available_names = NULL) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    if (!is.null(available_names)) {
        names(available_values) <- available_names
    }

    radioButtons(inputId = paste("tables", header, subheader, variable_name, sep = "_"),
                 label = label_name,
                 choices = available_values,
                 selected = tables_rv[[paste(header, subheader, sep = "_")]][[variable_name]])
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
                                        "Minimum Passes", "minimum_key_passes"),
                       tables_cb_picker(header, subheader, tables_rv, "Teams", "team_id",
                                        all_teams$team_id, all_teams$team_abbreviation),
                       tables_cb_picker(header, subheader, tables_rv, "Patterns of Play", "shot_pattern", PATTERNS_OF_PLAY),
                       tables_cb_date_filter(header, subheader, tables_rv, all_seasons),
                       tables_cb_picker(header, subheader, tables_rv, "Competition Stages", "stage_name", COMPETITION_STAGES),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Teams", "split_by_teams"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Seasons", "split_by_seasons"),
                       tables_cb_radio(header, subheader, tables_rv, "Normalize Results By", "normalize_by",
                                       c("None", "96 Minutes")),
                       tables_cb_refresh(header, subheader)
                )
            )
        } else if (any(grepl("Teams", subheader))) {
            div(
                column(12,
                       h4("Team Settings"),
                       tables_cb_picker(header, subheader, tables_rv, "Patterns of Play", "shot_pattern", PATTERNS_OF_PLAY),
                       tables_cb_date_filter(header, subheader, tables_rv, all_seasons),
                       tables_cb_picker(header, subheader, tables_rv, "Competition Stages", "stage_name", COMPETITION_STAGES),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Seasons", "split_by_seasons"),
                       p(class = "control-label", "Filter Results by Venue"),
                       tables_cb_switch(header, subheader, tables_rv, "Home Only", "home_only"),
                       tables_cb_switch(header, subheader, tables_rv, "Away Only", "away_only"),
                       conditionalPanel(condition = "input.tables_xgoals_teams_stage_name == 'Regular Season'",
                                        p(class = "control-label", "Home-Adjust Results"),
                                        tables_cb_switch(header, subheader, tables_rv, "Home Adjustment", "home_adjusted")),
                       p(class = "control-label", "Filter Results by Game State"),
                       tables_cb_switch(header, subheader, tables_rv, "Even Game State Only", "even_game_state"),
                       tables_cb_radio(header, subheader, tables_rv, "Normalize Results By", "normalize_by",
                                       c("None", "Game")),
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
                       tables_cb_picker(header, subheader, tables_rv, "Competition Stages", "stage_name", COMPETITION_STAGES),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Teams", "split_by_teams"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Seasons", "split_by_seasons"),
                       tables_cb_radio(header, subheader, tables_rv, "Normalize Results By", "normalize_by",
                                       c("None", "96 Minutes")),
                       tables_cb_refresh(header, subheader)
                )
            )
        } else if (any(grepl("Games", subheader))) {
            div(
                column(12,
                       h4("Game Settings"),
                       tables_cb_date_filter(header, subheader, tables_rv, all_seasons),
                       tables_cb_picker(header, subheader, tables_rv, "Competition Stages", "stage_name", COMPETITION_STAGES),
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
                                        "Minimum Passes", "minimum_passes"),
                       tables_cb_picker(header, subheader, tables_rv, "Teams", "team_id",
                                        all_teams$team_id, all_teams$team_abbreviation),
                       tables_cb_picker(header, subheader, tables_rv, "Passing Third", "pass_origin_third", THIRDS_OF_FIELD),
                       tables_cb_date_filter(header, subheader, tables_rv, all_seasons),
                       tables_cb_picker(header, subheader, tables_rv, "Competition Stages", "stage_name", COMPETITION_STAGES),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Teams", "split_by_teams"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Seasons", "split_by_seasons"),
                       tables_cb_radio(header, subheader, tables_rv, "Normalize Results By", "normalize_by",
                                       c("None", "96 Minutes")),
                       tables_cb_refresh(header, subheader)
                )
            )
        } else if (any(grepl("Teams", subheader))) {
            div(
                column(12,
                       h4("Team Settings"),
                       tables_cb_picker(header, subheader, tables_rv, "Passing Third", "pass_origin_third", THIRDS_OF_FIELD),
                       tables_cb_date_filter(header, subheader, tables_rv, all_seasons),
                       tables_cb_picker(header, subheader, tables_rv, "Competition Stages", "stage_name", COMPETITION_STAGES),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Seasons", "split_by_seasons"),
                       p(class = "control-label", "Filter Results by Venue"),
                       tables_cb_switch(header, subheader, tables_rv, "Home Only", "home_only"),
                       tables_cb_switch(header, subheader, tables_rv, "Away Only", "away_only"),
                       tables_cb_radio(header, subheader, tables_rv, "Normalize Results By", "normalize_by",
                                       c("None", "Game")),
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
                       tables_cb_picker(header, subheader, tables_rv, "Competition Stages", "stage_name", COMPETITION_STAGES),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Teams", "split_by_teams"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Seasons", "split_by_seasons"),
                       tables_cb_radio(header, subheader, tables_rv, "Normalize Results By", "normalize_by",
                                       c("None", "96 Minutes")),
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
    total_count_actions = "All Actions"
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
    Total = "Goals added above the average player, normalized by position."
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
                              "Receiving", "Shooting", "Goals Added")
