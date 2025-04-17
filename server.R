server <- function(input, output, session) {

    # -----------------------------------------------
    # GENERAL ---------------------------------------
    # -----------------------------------------------

    # Source data -----------------------------------
    source("utils/retrieve_data.R")

    # Initialize reactive value objects -------------
    subheaders_rv <- reactiveValues()
    for (l in league_schemas) {
        tab_groups <- sapply(tab_config, names)
        for (tab_group in tab_groups) {
            i <- which(tab_groups == tab_group)
            tabs <- tab_config[[i]][[tab_group]]
            for (tab in tabs) {
                if (!is.null(tab$subheaders)) {
                    rv_key <- paste0(l, "/", tab$route_link)
                    subheaders_rv[[rv_key]] <- tolower(tab$subheaders[1])
                }
            }
        }
    }

    leagues_rv <- reactiveValues()
    for (l in league_schemas) {
        leagues_rv[[l]] <- l
    }

    # Initialize router -----------------------------
    router_server("mls")

    # Hide loading page -----------------------------
    hide(id = "loader_page", anim = TRUE, animType = "fade", time = 2)

    # Sidebar settings ------------------------------
    output$asa_sidebar_reactive <- renderUI({
        page <- get_page(session)

        league <- get_values_from_page(page)$league
        route_prefix <- get_values_from_page(page)$route_prefix
        subheader <- get_values_from_page(page)$subheader
        subheaders_rv[[assemble_key(league, route_prefix)]] <- subheader

        sidebar_ui(page, tab_config, subheaders_rv)
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
        page <- get_page(session)
        footer_ui(page, all_games, input$client_timezone)
    })

    # Controlbar element ----------------------------
    output$asa_controlbar <- renderUI({
        page <- get_page(session)
        page_key <- gsub("\\?.*$", "", page)

        if (any(controlbar_lookup[[page_key]] == "Tables")) {
            tables_controlbar(page, tables_rv)
        } else if (any(controlbar_lookup[[page_key]] == "Profiles")) {
            if (get_values_from_page(page)$route_prefix == "players") {
                profiles_players_controlbar(page, players_rv, players_dropdown)
            }
        } else {
            div()
        }
    })


    # -----------------------------------------------
    # TABLES ----------------------------------------
    # -----------------------------------------------

    # Initialize reactive value objects -------------
    source("utils/tables/reactive_values.R", local = TRUE)

    # Header element --------------------------------
    output$tables_header <- renderUI({
        page <- get_page(session)
        tables_header(page, tab_config)
    })

    # Subheader element -----------------------------
    output$tables_subheader <- renderUI({
        page <- get_page(session)
        tables_subheader(page, tab_config)
    })

    # Body element ----------------------------------
    output$tables_body <- renderUI({
        page <- get_page(session)
        tables_body(page, input$client_timezone, tables_rv, filtering_hint_ind)
    })

    lapply(1:30, function(i) {
        onevent("mouseenter", paste0("tables_header_", i), shinyjs::addCssClass(selector = paste0("#tables_header_", i, " .tables_helper_tooltip"), class = "visible"))
        onevent("mouseleave", paste0("tables_header_", i), shinyjs::removeCssClass(selector = paste0("#tables_header_", i, " .tables_helper_tooltip"), class = "visible"))
    })

    # API request -----------------------------------
    observeEvent(input$tables_refresh, {
        page <- get_page(session)
        tables_refresh("tables_refresh", input, tables_rv, page)
    })

    # Hide filtering hint box -----------------------
    observeEvent(input$filtering_hint_disable, {
        filtering_hint_ind(FALSE)
    })

    # Select/deselect pitch zones -------------------
    # TODO: Make this generalizable to any inputs with pitch zones
    observeEvent(input$`tables_goals-added_teams_zone_select`, {
        updateCheckboxGroupButtons(session, "tables_goals-added_teams_zone", selected = as.character(FIELD_ZONES))
    })

    observeEvent(input$`tables_goals-added_teams_zone_deselect`, {
        updateCheckboxGroupButtons(session, "tables_goals-added_teams_zone", selected = character(0))
    })


    # -----------------------------------------------
    # PLAYER PROFILES -------------------------------
    # -----------------------------------------------

    # Initialize reactive value objects -------------
    source("utils/players/reactive_values.R", local = TRUE)

    # Change reactive values upon refresh -----------
    observeEvent(input$profiles_players_refresh, {
        page <- get_page(session)
        league <- get_values_from_page(page)$league
        players_rv[[league]][["profiles_players_name"]] <- input$profiles_players_name
    })

    # Profile header --------------------------------
    output$profiles_players_header <- renderUI({
        page <- get_page(session)
        profiles_players_header(page, players_rv, all_players)
    })

    # Violins API request ---------------------------
    observeEvent(input$profiles_players_violin_refresh, {
        page <- get_page(session)
        profiles_players_violin_refresh("profiles_players_violin_refresh", players_rv, page)
    })

    # Violin plots ----------------------------------
    output$profiles_players_violins <- renderUI({
        page <- get_page(session)
        violin_plots_profiles_players(page, players_rv)
    })

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
