# UI --------------------------------------------
xgoals_goalkeepers_ui <- tables_div

# Server ----------------------------------------
xgoals_goalkeepers_server <- function(input, output, session) {
    DISPLAY_HEADER <- "xGoals"
    REFRESH_BUTTON_ID <- "tables_xgoals_goalkeepers_refresh"

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
