# Wrapper div -----------------------------------
tables_ui <- div(
    uiOutput("tables_header"),
    uiOutput("tables_subheader"),
    uiOutput("tables_body") %>% withSpinner(color = "#27aae1")
)


# Header ----------------------------------------
tables_header <- function(page, league_config) {
    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    display_name <- get_config_element(league, "Tables", route_prefix, league_config, "display_name")

    bs4Box(
        div(
            class = "header_background",
            h2(display_name)
        ),
        width = 12
    )
}


# Subheader -------------------------------------
tables_subheader <- function(page, league_config) {
    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    subheaders <- get_config_element(league, "Tables", route_prefix, league_config, "subheaders")

    # bs4Box(
    #     radioGroupButtons(inputId = "tables_subheader",
    #                       choiceNames = subheaders,
    #                       choiceValues = assemble_key(league, route_prefix, subheaders),
    #                       selected = assemble_key(league, route_prefix, subheaders_rv[[assemble_key(league, route_prefix)]])),
    #     width = 12
    # )

    bs4Box(
        div(
            id = "tables_subheader",
            class = "radioGroupButtons shiny-bound-input",
            div(
                class = "btn-group btn-group-container-sw",
                lapply(subheaders, function(s) {
                    div(
                        class = "btn-group",
                        a(
                            class = ifelse(page == assemble_key(league, route_prefix, s), "btn btn-default active", "btn btn-default"),
                            href = route_link(assemble_key(league, route_prefix, s)),
                            s
                        )
                    )
                })
            )
        ),
        width = 12
    )
}


# Body ------------------------------------------
tables_body <- function(page, league_config, client_timezone, tables_rv, filtering_hint_ind) {
    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    subheader <- get_values_from_page(page)$subheader

    rv_key <- assemble_key(league, route_prefix, subheader)

    if (is.null(tables_rv[[rv_key]][["data_frame"]])) {
        tables_rv[[rv_key]][["data_frame"]] <- try(tables_rv_to_df(page, league_config, tables_rv, client_timezone))
    }

    df <- tables_rv[[rv_key]][["data_frame"]]

    if (!is.data.frame(df)) {
        bs4Box(
            p("Search yielded zero results."),
            width = 12
        )
    } else {
        df <- df %>% select(-contains("Actions"))

        sort_vector <- tables_rv[[rv_key]][["sort_vector"]]
        sort_vector <- lapply(sort_vector, function(x) {
            x[1] <- which(names(df) == x[1]) - 1
            return(x)
        })

        for (i in 1:length(names(df))) {
            if (names(df)[i] == "Goals Added" & any(tables_rv[[rv_key]][["goals_added_variation"]] == "Raw")) {
                break
            }

            tmp_match <- match(names(df)[i], tables_column_tooltip_text$app_name)

            if (!is.na(tmp_match)) {
                names(df)[i] <- paste0(names(df)[i], "<i class=\"fa fa-question-circle tables_helper_icon\"></i><span class=\"tables_helper_tooltip\">", tables_column_tooltip_text$tooltip_text[tmp_match], "</span>")
            }
        }

        dt <- DT::datatable(
            df,
            extensions = "Buttons",
            plugins = "accent-neutralise",
            options = list(pageLength = 30,
                           autoWidth = TRUE,
                           dom = "lBfrtip",
                           order = sort_vector,
                           buttons = list("copy",
                                          list(extend = "csv",
                                               filename = paste("american_soccer_analysis", league, route_prefix, subheader, format(Sys.time(), "%Y-%m-%d", tz = client_timezone), sep = "_"),
                                               exportOptions = JS("{
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
                                                                      }")),
                                          list(extend = 'excel',
                                               filename = paste("american_soccer_analysis", league, route_prefix, subheader, format(Sys.time(), "%Y-%m-%d", tz = client_timezone), sep = "_"),
                                               title = paste0("American Soccer Analysis  |  ", toupper(league), "  |  ", gsub("^Xp", "xP", gsub("^Xg", "xG", toTitleCase(gsub("_", " ", route_prefix)))), "  |  ", toTitleCase(subheader)),
                                               exportOptions = JS("{
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

        if (filtering_hint_ind()) {
            bs4Box(
                div(id = "filtering_hint_wrapper",
                    panel(p("Click the settings option (the gear icon) in the top-right corner to tailor your results."),
                          actionButton("filtering_hint_disable", "Got it!"),
                          status = "info",
                          heading = HTML("<span class=\"glyphicon\">&#xe086;</span>Filtering"))),
                div(class = "datatable_wrapper", dt),
                width = 12
            )
        } else {
            bs4Box(
                div(class = "datatable_wrapper", dt),
                width = 12
            )
        }
    }
}


# Controlbar ------------------------------------
tables_controlbar <- function(page, league_config, tables_rv) {
    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    subheader <- get_values_from_page(page)$subheader

    if (any(grepl("xgoals", route_prefix))) {
        if (any(grepl("players", subheader))) {
            div(
                column(12,
                       h4("Player Settings"),
                       tables_cb_numeric(page, league_config, tables_rv,
                                         "Minimum Minutes Played", "minimum_minutes", 25),
                       tables_cb_numeric(page, league_config, tables_rv,
                                         "Minimum Shots Taken", "minimum_shots", 5),
                       tables_cb_numeric(page, league_config, tables_rv,
                                         "Minimum Key Passes", "minimum_key_passes", 5),
                       tables_cb_picker(page, league_config, tables_rv, "Positions", "general_position", general_positions),
                       tables_cb_picker(page, league_config, tables_rv, "Teams", "team_id",
                                        all_teams[[league]]$team_id, all_teams[[league]]$team_abbreviation),
                       tables_cb_picker(page, league_config, tables_rv, "Patterns of Play", "shot_pattern", PATTERNS_OF_PLAY),
                       tables_cb_date_filter(page, league_config, tables_rv, all_seasons[[league]]),
                       tables_cb_picker(page, league_config, tables_rv, "Competition Stages", "stage_name", league_config[[league]][["stages"]]),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(page, league_config, tables_rv, "Split by Teams", "split_by_teams"),
                       tables_cb_switch(page, league_config, tables_rv, "Split by Seasons", "split_by_seasons"),
                       tables_cb_radio(page, league_config, tables_rv, "Normalize Results By", "normalize_by",
                                       c("None", "96 Minutes")),
                       tables_cb_refresh()
                )
            )
        } else if (any(grepl("teams", subheader))) {
            div(
                column(12,
                       h4("Team Settings"),
                       tables_cb_picker(page, league_config, tables_rv, "Patterns of Play", "shot_pattern", PATTERNS_OF_PLAY),
                       tables_cb_date_filter(page, league_config, tables_rv, all_seasons[[league]]),
                       tables_cb_picker(page, league_config, tables_rv, "Competition Stages", "stage_name", league_config[[league]][["stages"]]),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(page, league_config, tables_rv, "Split by Seasons", "split_by_seasons"),
                       p(class = "control-label", "Filter Results by Venue"),
                       tables_cb_switch(page, league_config, tables_rv, "Home Only", "home_only"),
                       tables_cb_switch(page, league_config, tables_rv, "Away Only", "away_only"),
                       conditionalPanel(condition = "input.tables_xgoals_teams_stage_name == 'Regular Season'",
                                        p(class = "control-label", "Home-Adjust Results"),
                                        tables_cb_switch(page, league_config, tables_rv, "Home Adjustment", "home_adjusted")),
                       p(class = "control-label", "Filter Results by Game State"),
                       tables_cb_switch(page, league_config, tables_rv, "Even Game State Only", "even_game_state"),
                       tables_cb_radio(page, league_config, tables_rv, "Normalize Results By", "normalize_by",
                                       c("None", "Game")),
                       tables_cb_refresh()
                )
            )
        } else if (any(grepl("goalkeepers", subheader))) {
            div(
                column(12,
                       h4("Goalkeeper Settings"),
                       tables_cb_numeric(page, league_config, tables_rv,
                                         "Minimum Minutes Played", "minimum_minutes", 25),
                       tables_cb_numeric(page, league_config, tables_rv,
                                         "Minimum Shots Faced", "minimum_shots_faced", 5),
                       tables_cb_picker(page, league_config, tables_rv, "Teams", "team_id",
                                        all_teams[[league]]$team_id, all_teams[[league]]$team_abbreviation),
                       tables_cb_picker(page, league_config, tables_rv, "Patterns of Play", "shot_pattern", PATTERNS_OF_PLAY),
                       tables_cb_date_filter(page, league_config, tables_rv, all_seasons[[league]]),
                       tables_cb_picker(page, league_config, tables_rv, "Competition Stages", "stage_name", league_config[[league]][["stages"]]),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(page, league_config, tables_rv, "Split by Teams", "split_by_teams"),
                       tables_cb_switch(page, league_config, tables_rv, "Split by Seasons", "split_by_seasons"),
                       tables_cb_radio(page, league_config, tables_rv, "Normalize Results By", "normalize_by",
                                       c("None", "96 Minutes")),
                       tables_cb_refresh()
                )
            )
        } else if (any(grepl("games", subheader))) {
            div(
                column(12,
                       h4("Game Settings"),
                       tables_cb_date_filter(page, league_config, tables_rv, all_seasons[[league]]),
                       tables_cb_picker(page, league_config, tables_rv, "Competition Stages", "stage_name", league_config[[league]][["stages"]]),
                       tables_cb_refresh()
                )
            )
        }
    } else if (any(grepl("xpass", route_prefix))) {
        if (any(grepl("players", subheader))) {
            div(
                column(12,
                       h4("Player Settings"),
                       tables_cb_numeric(page, league_config, tables_rv,
                                         "Minimum Minutes Played", "minimum_minutes", 25),
                       tables_cb_numeric(page, league_config, tables_rv,
                                         "Minimum Passes", "minimum_passes", 25),
                       tables_cb_picker(page, league_config, tables_rv, "Positions", "general_position", general_positions),
                       tables_cb_picker(page, league_config, tables_rv, "Teams", "team_id",
                                        all_teams[[league]]$team_id, all_teams[[league]]$team_abbreviation),
                       tables_cb_picker(page, league_config, tables_rv, "Passing Third", "pass_origin_third", THIRDS_OF_FIELD),
                       tables_cb_date_filter(page, league_config, tables_rv, all_seasons[[league]]),
                       tables_cb_picker(page, league_config, tables_rv, "Competition Stages", "stage_name", league_config[[league]][["stages"]]),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(page, league_config, tables_rv, "Split by Teams", "split_by_teams"),
                       tables_cb_switch(page, league_config, tables_rv, "Split by Seasons", "split_by_seasons"),
                       tables_cb_radio(page, league_config, tables_rv, "Normalize Results By", "normalize_by",
                                       c("None", "96 Minutes")),
                       tables_cb_refresh()
                )
            )
        } else if (any(grepl("teams", subheader))) {
            div(
                column(12,
                       h4("Team Settings"),
                       tables_cb_picker(page, league_config, tables_rv, "Passing Third", "pass_origin_third", THIRDS_OF_FIELD),
                       tables_cb_date_filter(page, league_config, tables_rv, all_seasons[[league]]),
                       tables_cb_picker(page, league_config, tables_rv, "Competition Stages", "stage_name", league_config[[league]][["stages"]]),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(page, league_config, tables_rv, "Split by Seasons", "split_by_seasons"),
                       p(class = "control-label", "Filter Results by Venue"),
                       tables_cb_switch(page, league_config, tables_rv, "Home Only", "home_only"),
                       tables_cb_switch(page, league_config, tables_rv, "Away Only", "away_only"),
                       tables_cb_radio(page, league_config, tables_rv, "Normalize Results By", "normalize_by",
                                       c("None", "Game")),
                       tables_cb_refresh()
                )
            )
        }
    } else if (any(grepl("goals_added", route_prefix))) {
        if (any(grepl("players", subheader))) {
            div(
                column(12,
                       h4("Player Settings"),
                       tables_cb_numeric(page, league_config, tables_rv,
                                         "Minimum Minutes Played", "minimum_minutes", 25),
                       tables_cb_picker(page, league_config, tables_rv, "Positions", "general_position",
                                        general_positions[general_positions != "GK"]),
                       tables_cb_picker(page, league_config, tables_rv, "Teams", "team_id",
                                        all_teams[[league]]$team_id, all_teams[[league]]$team_abbreviation),
                       tables_cb_date_filter(page, league_config, tables_rv, all_seasons[[league]]),
                       tables_cb_picker(page, league_config, tables_rv, "Competition Stages", "stage_name", league_config[[league]][["stages"]]),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(page, league_config, tables_rv, "Split by Teams", "split_by_teams"),
                       tables_cb_switch(page, league_config, tables_rv, "Split by Seasons", "split_by_seasons"),
                       tables_cb_radio(page, league_config, tables_rv, "g+ Variation", "goals_added_variation",
                                       c("Raw", "Above Average", "Above Replacement")),
                       tables_cb_radio(page, league_config, tables_rv, "Normalize Results By", "normalize_by",
                                       c("None", "96 Minutes")),
                       tables_cb_refresh()
                )
            )
        } else if(any(grepl("teams", subheader))){
            div(
                column(12,
                       h4("Team Settings"),
                       tables_cb_pitch_zones(page, league_config, tables_rv, "Zones", "zone"),
                       tables_cb_picker(page, league_config, tables_rv, "Gamestates", "gamestate_trunc",
                                        c(-2:2), as.character(c("< -2", -1:1, "2+"))),
                       tables_cb_date_filter(page, league_config, tables_rv, all_seasons[[league]], season_only = TRUE),
                       tables_cb_picker(page, league_config, tables_rv, "Competition Stages", "stage_name", league_config[[league]][["stages"]]),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(page, league_config, tables_rv, "Split by Seasons", "split_by_seasons"),
                       tables_cb_radio(page, league_config, tables_rv, "Normalize Results By", "normalize_by",
                                       c("None", "96 Minutes")),
                       tables_cb_refresh()
                )
            )
        }
    } else if (any(grepl("salaries", route_prefix))) {
        if (any(grepl("players", subheader))) {
            div(
                column(12,
                       h4("Player Settings"),
                       tables_cb_picker(page, league_config, tables_rv, "Teams", "team_id",
                                        all_teams[[league]]$team_id, all_teams[[league]]$team_abbreviation),
                       tables_cb_picker(page, league_config, tables_rv, "Position", "position", MLSPA_POSITIONS),
                       tables_cb_date_filter(page, league_config, tables_rv, all_seasons[[league]]),
                       tables_cb_refresh()
                )
            )
        } else if (any(grepl("teams", subheader))) {
            div(
                column(12,
                       h4("Team Settings"),
                       tables_cb_picker(page, league_config, tables_rv, "Seasons", "season_name", salaries_seasons[[league]]),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(page, league_config, tables_rv, "Split by Teams", "split_by_teams"),
                       tables_cb_switch(page, league_config, tables_rv, "Split by Seasons", "split_by_seasons"),
                       tables_cb_switch(page, league_config, tables_rv, "Split by Positions", "split_by_positions"),
                       tables_cb_refresh()
                )
            )
        }
    }
}


# Controlbar inputs -----------------------------
tables_cb_slider <- function(page, league_config, tables_rv, max_value, label_name, variable_name, step) {
    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    subheader <- get_values_from_page(page)$subheader

    sliderInput(inputId = paste("tables", route_prefix, subheader, variable_name, sep = "_"),
                label = label_name,
                min = 0,
                max = max_value,
                value = tables_rv[[assemble_key(league, route_prefix, subheader)]][[variable_name]],
                step = step,
                ticks = FALSE,
                width = "100%")
}

tables_cb_date_filter <- function(page, league_config, tables_rv, all_seasons, season_only = FALSE) {
    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    subheader <- get_values_from_page(page)$subheader

    if (season_only) {
        pickerInput(inputId = paste("tables", route_prefix, subheader, "season_name", sep = "_"),
                    label = "Seasons",
                    choices = all_seasons,
                    selected = tables_rv[[assemble_key(league, route_prefix, subheader)]][["season_name"]],
                    multiple = TRUE,
                    options = list(`selected-text-format` = "count > 3",
                                   `actions-box` = TRUE))
    } else {
        div(
            radioGroupButtons(inputId = paste("tables", route_prefix, subheader, "date_type", sep = "_"),
                              label = "Filter Dates By",
                              choices = c("Season", "Date Range"),
                              justified = TRUE,
                              selected = tables_rv[[assemble_key(league, route_prefix, subheader)]][["date_type"]],
                              width = "100%"),
            conditionalPanel(condition = paste0("input.", paste("tables", route_prefix, subheader, "date_type", sep = "_"), " == 'Season'"),
                             pickerInput(inputId = paste("tables", route_prefix, subheader, "season_name", sep = "_"),
                                         label = NULL,
                                         choices = all_seasons,
                                         selected = tables_rv[[assemble_key(league, route_prefix, subheader)]][["season_name"]],
                                         multiple = TRUE,
                                         options = list(`selected-text-format` = "count > 3",
                                                        `actions-box` = TRUE))),
            conditionalPanel(condition = paste0("input.", paste("tables", route_prefix, subheader, "date_type", sep = "_"), " == 'Date Range'"),
                             dateRangeInput(inputId = paste("tables", route_prefix, subheader, "date_range", sep = "_"),
                                            label = NULL,
                                            start = tables_rv[[assemble_key(league, route_prefix, subheader)]][["start_date"]],
                                            end = tables_rv[[assemble_key(league, route_prefix, subheader)]][["end_date"]],
                                            min = paste0(min(all_seasons), "-01-01"),
                                            max = paste0(max(all_seasons), "-12-31"),
                                            separator = "  to  "))
        )
    }
}

tables_cb_picker <- function(page, league_config, tables_rv, label_name, variable_name, available_values, available_names = NULL) {
    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    subheader <- get_values_from_page(page)$subheader

    if (!is.null(available_names)) {
        names(available_values) <- available_names
    }

    pickerInput(inputId = paste("tables", route_prefix, subheader, variable_name, sep = "_"),
                label = label_name,
                choices = available_values,
                selected = tables_rv[[assemble_key(league, route_prefix, subheader)]][[variable_name]],
                multiple = TRUE,
                options = list(`selected-text-format` = "count > 3",
                               `actions-box` = TRUE))
}

tables_cb_pitch_zones <- function(page, league_config, tables_rv, label_name, variable_name) {
    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    subheader <- get_values_from_page(page)$subheader

    div(
        class = "pitch-zone-wrapper",
        p(class = "control-label", label_name),
        actionGroupButtons(inputIds = c(paste("tables", route_prefix, subheader, variable_name, "select", sep = "_"),
                                        paste("tables", route_prefix, subheader, variable_name, "deselect", sep = "_")),
                           labels = c("Select All", "Deselect All"),
                           status = "default",
                           fullwidth = TRUE),
        checkboxGroupButtons(inputId = paste("tables", route_prefix, subheader, variable_name, sep = "_"),
                             label = NULL,
                             choices = FIELD_ZONES,
                             justified = TRUE,
                             selected = tables_rv[[assemble_key(league, route_prefix, subheader)]][[variable_name]]),
        p(em("The attacking half (top) is represented by zones 16 through 30. The defensive half (bottom) is represented by zones 1 through 15."))
    )
}

tables_cb_switch <- function(page, league_config, tables_rv, label_name, variable_name) {
    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    subheader <- get_values_from_page(page)$subheader

    prettyCheckbox(inputId = paste("tables", route_prefix, subheader, variable_name, sep = "_"),
                   label = label_name,
                   value = tables_rv[[assemble_key(league, route_prefix, subheader)]][[variable_name]],
                   icon = icon("check"))
}

tables_cb_home_away <- function(page, league_config, tables_rv) {
    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    subheader <- get_values_from_page(page)$subheader

    selected_options <- c()
    for (s in c("home_only", "away_only")) {
        if (tables_rv[[assemble_key(league, route_prefix, subheader)]][[s]]) {
            selected_options <- c(selected_options, s)
        }
    }

    checkboxGroupButtons(inputId = paste("tables", route_prefix, subheader, "home_away", sep = "_"),
                         labels = "Filter by Venue",
                         choices = list(home_only = "Home Only",
                                        away_only = "Away Only"),
                         selected = selected_options)
}

tables_cb_radio <- function(page, league_config, tables_rv, label_name, variable_name, available_values, available_names = NULL) {
    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    subheader <- get_values_from_page(page)$subheader

    if (!is.null(available_names)) {
        names(available_values) <- available_names
    }

    radioButtons(inputId = paste("tables", route_prefix, subheader, variable_name, sep = "_"),
                 label = label_name,
                 choices = available_values,
                 selected = tables_rv[[assemble_key(league, route_prefix, subheader)]][[variable_name]])
}

tables_cb_numeric <- function(page, league_config, tables_rv, label_name, variable_name, step = NA, max_value = NA) {
    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    subheader <- get_values_from_page(page)$subheader

    numericInput(inputId = paste("tables", route_prefix, subheader, variable_name, sep = "_"),
                 label = label_name,
                 min = 0,
                 max = max_value,
                 value = tables_rv[[assemble_key(league, route_prefix, subheader)]][[variable_name]],
                 step = step,
                 width = "100%")
}

tables_cb_refresh <- function() {
    actionButton("tables_refresh", "Refresh Results", width = "100%")
}
