server <- function(input, output, session) {

    # -----------------------------------------------
    # GENERAL ---------------------------------------
    # -----------------------------------------------

    # Source data -----------------------------------
    source("utils/retrieve_data.R")

    # Initialize reactive value objects -------------
    subheaders_rv <- reactiveValues()
    for (l in league_schemas) {
        headers <- league_config[[l]][["tabs"]]
        for (h in headers) {
            for (m in h) {
                if (!is.null(m$subheaders)) {
                    rv_key <- paste0(l, "/", m$route_link)
                    subheaders_rv[[rv_key]] <- tolower(m$subheaders[1])
                }
            }
        }
    }

    leagues_rv <- reactiveValues()
    for (l in league_schemas) {
        leagues_rv[[l]] <- l
    }

    # Initialize router -----------------------------
    router(input, output, session)

    # Hide loading page -----------------------------
    hide(id = "loader_page", anim = TRUE, animType = "fade", time = 2)

    # Sidebar settings ------------------------------
    output$asa_sidebar_reactive <- renderUI({
        page <- get_page(session)

        league <- get_values_from_page(page)$league
        route_prefix <- get_values_from_page(page)$route_prefix
        subheader <- get_values_from_page(page)$subheader
        subheaders_rv[[assemble_key(league, route_prefix)]] <- subheader

        sidebar_ui(page, league_config, subheaders_rv)
    })

    # Navbar settings -------------------------------
    output$asa_navbar <- renderUI({
        page <- get_page(session)

        league <- get_values_from_page(page)$league
        route_prefix <- get_values_from_page(page)$route_prefix
        subheader <- get_values_from_page(page)$subheader
        leagues_rv[[league]] <- assemble_key(league, route_prefix, subheader)

        navbar_ui(page, league_config, leagues_rv)
    })

    # Footer settings -------------------------------
    output$asa_footer <- renderUI({
        footer_ui(recent_games, input$client_timezone)
    })


    # -----------------------------------------------
    # TABLES ----------------------------------------
    # -----------------------------------------------

    # Initialize reactive value objects -------------
    source("utils/tables/reactive_values.R", local = TRUE)

    # Header element --------------------------------
    output$tables_header <- renderUI({
        page <- get_page(session)
        tables_header(page, league_config)
    })

    # Subheader element -----------------------------
    output$tables_subheader <- renderUI({
        page <- get_page(session)
        tables_subheader(page, league_config)
    })

    # Body element ----------------------------------
    output$tables_body <- renderUI({
        page <- get_page(session)
        tables_body(page, league_config, input$client_timezone, tables_rv, filtering_hint_ind)
    })

    # Controlbar element ----------------------------
    output$asa_controlbar <- renderUI({
        page <- get_page(session)
        tables_controlbar(page, league_config, tables_rv)
    })

    # API request -----------------------------------
    observeEvent(input$tables_refresh, {
        page <- get_page(session)
        tables_refresh("tables_refresh", input, tables_rv, page, league_config)
    })

    # Hide filtering hint box -----------------------
    observeEvent(input$filtering_hint_disable, {
        filtering_hint_ind(FALSE)
    })

    # Select/deselect pitch zones -------------------
    # TODO: Make this generalizable to any inputs with pitch zones
    observeEvent(input$tables_goals_added_teams_zone_select, {
        updateCheckboxGroupButtons(session, "tables_goals_added_teams_zone", selected = as.character(30:1))
    })

    observeEvent(input$tables_goals_added_teams_zone_deselect, {
        updateCheckboxGroupButtons(session, "tables_goals_added_teams_zone", selected = character(0))
    })


    # -----------------------------------------------
    # PLAYER PROFILES -------------------------------
    # -----------------------------------------------

    # Set default reactive values -------------------
    # players_reactive_values <- reactiveValues(profile_player_name = START_PLAYER,
    #                                           profile_player_season = max(all_players_seasons$season_name[all_players_seasons$player_id == START_PLAYER]))

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
