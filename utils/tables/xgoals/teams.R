# UI --------------------------------------------
xgoals_teams_ui <- tables_div

# Server ----------------------------------------
xgoals_teams_server <- function(input, output, session) {
    DISPLAY_HEADER <- "xGoals"
    REFRESH_BUTTON_ID <- "tables_xgoals_teams_refresh"

    output$tables_header <- renderUI({
        tables_header(DISPLAY_HEADER)
    })

    output$tables_subheader <- renderUI({
        page <- get_page(session)
        tables_subheader(DISPLAY_HEADER, page, league_config, subheaders_rv)
    })

    output$tables_body <- renderUI({
        page <- get_page(session)
        tables_body(page, league_config, input$client_timezone, tables_rv, filtering_hint_ind)
    })

    output$asa_controlbar <- renderUI({
        page <- get_page(session)
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
                   tables_cb_refresh(page, league_config)
            )
        )
    })


    observeEvent(input[[REFRESH_BUTTON_ID]], {
        page <- get_page(session)
        tables_refresh(REFRESH_BUTTON_ID, input, tables_rv, page, league_config)
    })


    observeEvent(input$filtering_hint_disable, {
        filtering_hint_ind(FALSE)
    })
}
