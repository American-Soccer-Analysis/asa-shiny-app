# UI --------------------------------------------
salaries_teams_ui <- tables_div

# Server ----------------------------------------
salaries_teams_server <- function(input, output, session) {
    DISPLAY_HEADER <- "Salaries"

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
                   h4("Team Settings"),
                   tables_cb_picker(page, league_config, tables_rv, "Seasons", "season_name", salaries_seasons[[league]]),
                   p(class = "control-label", "Group Results"),
                   tables_cb_switch(page, league_config, tables_rv, "Split by Teams", "split_by_teams"),
                   tables_cb_switch(page, league_config, tables_rv, "Split by Seasons", "split_by_seasons"),
                   tables_cb_switch(page, league_config, tables_rv, "Split by Positions", "split_by_positions"),
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
