# UI --------------------------------------------
goals_added_players_ui <- tables_div

# Server ----------------------------------------
goals_added_players_server <- function(input, output, session) {
    DISPLAY_HEADER <- "Goals Added (g+)"

    page <- get_page(session)
    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    subheader <- get_values_from_page(page)$subheader


    output$tables_header <- renderUI({
        tables_header(DISPLAY_HEADER)
    })

    output$tables_subheader <- renderUI({
        page <- get_page(session)
        tables_subheader(DISPLAY_HEADER, page, league_config, subheaders_rv)
    })

    observeEvent(input$tables_subheader, {
        if (get_page(session) != input$tables_subheader) {
            rv_key <- gsub("/[A-Za-z]+$", "", input$tables_subheader)
            rv_value <- gsub(rv_key, "", input$tables_subheader)
            subheaders_rv[[rv_key]] <- rv_value

            change_page(input$tables_subheader)
        }
    })

    output$tables_body <- renderUI({
        page <- get_page(session)
        tables_body(page, league_config, input$client_timezone, tables_rv, filtering_hint_ind)
    })

    output$asa_controlbar <- renderUI({
        page <- get_page(session)
        div(
            column(12,
                   h4("Player Settings"),
                   tables_cb_numeric(page, league_config, tables_rv,
                                     "Minimum Minutes Played", "minimum_minutes", 25),
                   tables_cb_picker(page, league_config, tables_rv, "Positions", "general_position",
                                    general_positions[[league]][general_positions[[league]] != "GK"]),
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
                   tables_cb_refresh(page, league_config)
            )
        )
    })


    refresh_button_id <- paste("tables", route_prefix, subheader, "refresh", sep = "_")

    observeEvent(input[[refresh_button_id]], {
        tables_refresh(refresh_button_id, input, tables_rv, page, league_config)
    })


    observeEvent(input$filtering_hint_disable, {
        filtering_hint_ind(FALSE)
    })
}
