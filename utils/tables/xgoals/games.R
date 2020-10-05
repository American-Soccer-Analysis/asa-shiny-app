# UI --------------------------------------------
xgoals_games_ui <- tables_div

# Server ----------------------------------------
xgoals_games_server <- function(input, output, session) {
    DISPLAY_HEADER <- "xGoals"
    REFRESH_BUTTON_ID <- "tables_xgoals_games_refresh"

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
                   h4("Game Settings"),
                   tables_cb_date_filter(page, league_config, tables_rv, all_seasons[[league]]),
                   tables_cb_picker(page, league_config, tables_rv, "Competition Stages", "stage_name", league_config[[league]][["stages"]]),
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
