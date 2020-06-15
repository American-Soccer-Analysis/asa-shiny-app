shinyServer(function(input, output, session) {

    # -----------------------------------------------
    # GLOBAL SETTINGS -------------------------------
    # -----------------------------------------------

    # Source data -----------------------------------
    source("utils/retrieve_data.R")
    source("utils/reactive_values.R")

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
            matching_inputs <- names(input)
            matching_inputs <- matching_inputs[grepl(gsub("_refresh$", "", x), matching_inputs) & !grepl("refresh", matching_inputs)]

            rv_key <- gsub("(^tables_|_refresh$)", "", x)

            lapply(matching_inputs, function(y) {
                rv_secondary_key <- gsub(paste0("tables_", rv_key, "_"), "", y)
                tables_rv[[rv_key]][[rv_secondary_key]] <- input[[y]]
            })
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
        tables_subheader(input$asa_sidebar)
    })

    output$tables_subheader <- renderUI({
        tables_subheader_reactive()
    })

    # xGoals body -----------------------------------
    tables_xgoals_ui_reactive <- reactive({
        tables_xgoals_ui()
    })

    output$tables_xgoals_ui <- renderUI({
        tables_xgoals_ui_reactive()
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
})
