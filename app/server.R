server <- function(input, output, session) {

    # -----------------------------------------------
    # GLOBAL SETTINGS -------------------------------
    # -----------------------------------------------

    # Source data -----------------------------------
    source("../app/utils/retrieve_data.R")
    source("../app/utils/reactive_values.R", local = TRUE)

    # Hide loading page -----------------------------
    hide(id = "loader_page", anim = TRUE, animType = "fade", time = 2)

    # Set default reactive values -------------------
    # players_reactive_values <- reactiveValues(profile_player_name = START_PLAYER,
    #                                           profile_player_season = max(all_players_seasons$season_name[all_players_seasons$player_id == START_PLAYER]))

    # Body settings ---------------------------------
    body_reactive <- eventReactive(input$asa_sidebar, {
        # if (input$asa_sidebar == "profile_player") {
        #     profile_player
        # }
        if (grepl("^tables", input$asa_sidebar)) {
            tables_div
        } else if (input$asa_sidebar == "home") {
            home_div
        }
    })

    output$asa_body <- renderUI({
        body_reactive()
    })

    # Controlbar settings ---------------------------
    controlbar_listener <- reactive({
        list(input$asa_sidebar, input$tables_subheader)
    })

    controlbar_reactive <- eventReactive(controlbar_listener(), {
        if (input$asa_sidebar == "profile_player") {
            controlbar_profile_player(players_dropdown, players_reactive_values, all_players_seasons)
        } else if (grepl("^tables", input$asa_sidebar)) {
            controlbar_tables(input$asa_sidebar, input$tables_subheader, tables_rv)
        }
    })

    output$asa_controlbar <- renderUI({
        controlbar_reactive()
    })

    # Footer settings -------------------------------
    observeEvent(input$client_timezone, {
        output$asa_footer <- renderUI({
            footer_reactive(recent_games, input$client_timezone)
        })
    })


    # -----------------------------------------------
    # TABLES ----------------------------------------
    # -----------------------------------------------

    # Change reactive values upon refresh -----------
    table_refreshes <- c()
    for (x in names(tables_menu_items)) {
        for (y in tables_menu_items[[x]][["subheaders"]]) {
            table_refreshes <- c(table_refreshes, paste(x, tolower(y), "refresh", sep = "_"))
        }
    }

    lapply(table_refreshes, function(x) {
        observeEvent(input[[x]], {
            shinyjs::disable(x)

            matching_inputs <- names(input)
            matching_inputs <- matching_inputs[grepl(gsub("_refresh$", "", x), matching_inputs) & !grepl("refresh", matching_inputs)]

            rv_key <- gsub("(^tables_|_refresh$)", "", x)


            execute_api_call <- TRUE

            if (any(grepl("date_type", matching_inputs))) {

                if (input[[gsub("_refresh$", "_date_type", x)]] == "Date Range") {

                    if (sum(is.na(c(input[[gsub("_refresh$", "_date_range", x)]][1], input[[gsub("_refresh$", "_date_range", x)]][2]))) == 1) {

                        sendSweetAlert(
                            session,
                            title = "Error: Date Filter",
                            text = "If filtering by date range, both a start and end date must be included.",
                            type = "error"
                        )

                        shinyjs::enable(x)

                        execute_api_call <- FALSE

                    } else if (sum(is.na(c(input[[gsub("_refresh$", "_date_range", x)]][1], input[[gsub("_refresh$", "_date_range", x)]][2]))) == 0 &
                               input[[gsub("_refresh$", "_date_range", x)]][2] < input[[gsub("_refresh$", "_date_range", x)]][1]) {

                        sendSweetAlert(
                            session,
                            title = "Error: Date Filter",
                            text = "If filtering by date range, the end date must be greater than or equal to the start date.",
                            type = "error"
                        )

                        shinyjs::enable(x)

                        execute_api_call <- FALSE

                    }

                }

            }

            if (grepl("salaries_teams", x)) {

                if (sum(c(input[[gsub("_refresh$", "_split_by_teams", x)]], input[[gsub("_refresh$", "_split_by_seasons", x)]], input[[gsub("_refresh$", "_split_by_positions", x)]])) == 0) {

                    sendSweetAlert(
                        session,
                        title = "Error: Grouping Results",
                        text = "Results must be grouped by at least one of teams, positions, or seasons.",
                        type = "error"
                    )

                    shinyjs::enable(x)

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
                        rv_secondary_key <- gsub(paste0("tables_", rv_key, "_"), "", y)
                        tables_rv[[rv_key]][[rv_secondary_key]] <- input[[y]]
                    }
                })

                subheader <- gsub("^_", "", stri_extract_last_regex(rv_key, "_[a-z]+$"))
                header <- stri_replace_last_regex(rv_key, "_[a-z]+$", "")

                tables_rv[[rv_key]][["data_frame"]] <- tables_rv_to_df(header, subheader, tables_rv, input$client_timezone)

                shinyjs::enable(x)

            }

        })
    })

    # Tables header ---------------------------------
    tables_header_reactive <- reactive({
        tables_header(input$asa_sidebar)
    })

    output$tables_header <- renderUI({
        tables_header_reactive()
    })

    # Tables subheader ------------------------------
    tables_subheader_reactive <- reactive({
        if (grepl("^tables", input$asa_sidebar)) {
            tables_subheader(input$asa_sidebar)
        }
    })

    output$tables_subheader <- renderUI({
        tables_subheader_reactive()
    })

    # Tables body -----------------------------------
    tables_body_reactive <- reactive({
        tables_body(input$asa_sidebar, input$tables_subheader, input$client_timezone, tables_rv, filtering_hint_ind)
    })

    output$tables_body <- renderUI({
        tables_body_reactive()
    })

    # Collapse filtering hint panel -----------------
    observeEvent(input$filtering_hint_disable, {
        # hide(id = "filtering_hint_wrapper", anim = TRUE)
        filtering_hint_ind(FALSE)
    })


    # -----------------------------------------------
    # PROFILES: PLAYERS -----------------------------
    # -----------------------------------------------

    # # Add headshots to controlbar -------------------
    # observeEvent(input$asa_sidebar, {
    #     updateSelectizeInput(session,
    #                          "profile_player_name",
    #                          server = TRUE,
    #                          choices = players_dropdown,
    #                          selected = players_reactive_values$profile_player_name,
    #                          options = list(render = I(
    #                              "{
    #                            option: function(item, escape) {
    #                            return '<div><div class = \"players_dropdown_wrapper\"><img class=\"players_dropdown_img\"' +
    #                            'src=\"' + item.url + '\" /></div><div class = \"players_dropdown_txt\">' +
    #                            item.label + '</div></div>';
    #                            }
    #                            }"
    #                          )))
    # })
    #
    # # Update season on controlbar -------------------
    # observeEvent(input$profile_player_name, {
    #     if (input$profile_player_name == players_reactive_values$profile_player_name) {
    #         updateSelectizeInput(session,
    #                              "profile_player_season",
    #                              server = TRUE,
    #                              choices = all_players_seasons$season_name[all_players_seasons$player_id == input$profile_player_name],
    #                              selected = players_reactive_values$profile_player_season)
    #     } else if (input$profile_player_name != "") {
    #         updateSelectizeInput(session,
    #                              "profile_player_season",
    #                              server = TRUE,
    #                              choices = all_players_seasons$season_name[all_players_seasons$player_id == input$profile_player_name],
    #                              selected = max(all_players_seasons$season_name[all_players_seasons$player_id == input$profile_player_name]))
    #     } else {
    #         updateSelectizeInput(session,
    #                              "profile_player_season",
    #                              server = TRUE,
    #                              choices = NULL,
    #                              selected = NULL)
    #     }
    # })
    #
    # # Change reactive values upon refresh -----------
    # observeEvent(input$profile_player_refresh, {
    #     players_reactive_values$profile_player_name <- input$profile_player_name
    #     players_reactive_values$profile_player_season <- input$profile_player_season
    # })
    #
    # # Profile header --------------------------------
    # player_profile_basic_info_reactive <- reactive({
    #     header_profile_player(players_reactive_values, all_players)
    # })
    #
    # output$player_profile_basic_info <- renderUI({
    #     player_profile_basic_info_reactive()
    # })
    #
    # # Violin plots ----------------------------------
    # player_profile_violin_plots_reactive <- reactive({
    #     violin_plots_profile_player(all_players_stats, players_reactive_values)
    # })
    #
    # output$player_profile_violin_plots <- renderUI({
    #     player_profile_violin_plots_reactive()
    # })
    #
    # # Touch heatmap ---------------------------------
    # player_profile_touch_heatmap_ggplot_reactive <- reactive({
    #     touch_heatmap_ggplot_profile_player(players_reactive_values)
    # })
    #
    # output$player_profile_touch_heatmap_ggplot <- renderPlot({
    #     player_profile_touch_heatmap_ggplot_reactive()
    # })
    #
    # player_profile_touch_heatmap_reactive <- reactive({
    #     touch_heatmap_profile_player()
    # })
    #
    # output$player_profile_touch_heatmap <- renderUI({
    #     player_profile_touch_heatmap_reactive()
    # })
}
