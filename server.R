shinyServer(function(input, output, session) {

    # Source data -----------------------------------
    source("retrieve_data.R")

    # Hide loading page -----------------------------
    hide(id = "loader_page", anim = TRUE, animType = "fade", time = 1)
    # addClass(selector = "body", class = "sidebar-open")
    # removeClass(selector = "body", class = "sidebar-collapse")

    # Set default player ----------------------------
    START_PLAYER <- 31740   # Dax
    players_reactive_values <- reactiveValues(profile_player_name = START_PLAYER,
                                              profile_player_season = max(all_players_seasons$season_name[all_players_seasons$player_id == START_PLAYER]))


    controlbar_reactive <- eventReactive(input$asa_sidebar, {
        if(input$asa_sidebar == "profile_player") {
            div(
                column(12,
                       h4("Player Settings"),
                       selectizeInput(inputId = "profile_player_name",
                                      label = "Player",
                                      choices = setNames(players_dropdown$value, players_dropdown$label),
                                      selected = players_reactive_values$profile_player_name,
                                      width = "100%",
                                      options = list(placeholder = 'Start typing a player\'s name...',
                                                     maxOptions = 25)),
                       selectizeInput(inputId = "profile_player_season",
                                      label = "Season",
                                      choices = sort(unique(all_players_seasons$season_name)),
                                      selected = players_reactive_values$profile_player_season,
                                      width = "100%",
                                      options = list(placeholder = "Select a season")),
                       actionButton("profile_player_refresh", "Refresh"))
            )
        }
    })

    output$asa_controlbar <- renderUI({
        controlbar_reactive()
    })

    observeEvent(input$asa_sidebar, {
        updateSelectizeInput(session,
                             "profile_player_name",
                             server = TRUE,
                             choices = players_dropdown,
                             selected = players_reactive_values$profile_player_name,
                             options = list(render = I(
                                 "{
                               option: function(item, escape) {
                               return '<div><div class = \"players_dropdown_wrapper\"><img class=\"players_dropdown_img\"' +
                               'src=\"' + item.url + '\" /></div><div class = \"players_dropdown_txt\">' +
                               item.label + '</div></div>';
                               }
                               }"
                             )))
    })

    observeEvent(input$profile_player_name, {
        if (input$profile_player_name == players_reactive_values$profile_player_name) {
            updateSelectizeInput(session,
                                 "profile_player_season",
                                 server = TRUE,
                                 choices = all_players_seasons$season_name[all_players_seasons$player_id == input$profile_player_name],
                                 selected = players_reactive_values$profile_player_season)
        }

        else if (input$profile_player_name != "") {
            updateSelectizeInput(session,
                                 "profile_player_season",
                                 server = TRUE,
                                 choices = all_players_seasons$season_name[all_players_seasons$player_id == input$profile_player_name],
                                 selected = max(all_players_seasons$season_name[all_players_seasons$player_id == input$profile_player_name]))
        }

        else {
            updateSelectizeInput(session,
                                 "profile_player_season",
                                 server = TRUE,
                                 choices = NULL,
                                 selected = NULL)
        }
    })

    observeEvent(input$profile_player_refresh, {
        players_reactive_values$profile_player_name <- input$profile_player_name
        players_reactive_values$profile_player_season <- input$profile_player_season
    })

    player_profile_basic_info_reactive <- reactive({
        bs4Box(
            div(class = "profile_player_background",
                div(class = "profile_player_headshot",
                    img(src = paste0("player_headshots/", players_reactive_values$profile_player_name, ".png"))),
                div(class = "profile_player_demographics",
                    h3(all_players$player_name[all_players$player_id == players_reactive_values$profile_player_name]),
                    p(HTML(paste0(all_players$broad_position[all_players$player_id == players_reactive_values$profile_player_name],
                                  " &nbsp;|&nbsp; Age: ", floor(age_calc(all_players$birth_date[all_players$player_id == players_reactive_values$profile_player_name], units = "years")),
                                  " &nbsp;|&nbsp; Height: ", all_players$height_ft[all_players$player_id == players_reactive_values$profile_player_name], "'", all_players$height_in[all_players$player_id == players_reactive_values$profile_player_name], "\"",
                                  " &nbsp;|&nbsp; Weight: ", all_players$weight_lb[all_players$player_id == players_reactive_values$profile_player_name], " lbs"))),
                    p(all_players$home_town[all_players$player_id == players_reactive_values$profile_player_name]))
            ),
            width = 12
        )
    })

    player_profile_violin_plots_reactive <- reactive({
        bs4Box(
            h3("Comparative Performance Metrics (per 96 Minutes Played)", class = "card_header"),
            bs4TabSetPanel(
                id = "player_profile_violin_tabs",
                side = "left",
                bs4TabPanel(
                    tabName = "xGoals",
                    active = TRUE,
                    violin_d3(data_frame = all_players_stats,
                              metric = "xg_p96",
                              metric_percentage = FALSE,
                              precision = 0.01,
                              tooltip_precision = 0.01,
                              x_axis_title = "xGoals per 96 Minutes Played",
                              x_axis_suffix = "",
                              x_axis_absolute = TRUE,
                              annotation_suffix = " xG",
                              player = players_reactive_values$profile_player_name,
                              season = players_reactive_values$profile_player_season)
                ),
                bs4TabPanel(
                    tabName = "xAssists",
                    active = FALSE,
                    violin_d3(data_frame = all_players_stats,
                              metric = "xa_p96",
                              metric_percentage = FALSE,
                              precision = 0.005,
                              tooltip_precision = 0.01,
                              x_axis_title = "xAssists per 96 Minutes Played",
                              x_axis_suffix = "",
                              x_axis_absolute = TRUE,
                              annotation_suffix = " xA",
                              player = players_reactive_values$profile_player_name,
                              season = players_reactive_values$profile_player_season)
                ),
                bs4TabPanel(
                    tabName = "Involvement",
                    active = FALSE,
                    violin_d3(data_frame = all_players_stats,
                              metric = "involvement",
                              metric_percentage = TRUE,
                              precision = 0.15,
                              tooltip_precision = 0.1,
                              x_axis_title = "Share of Team's Touches (%)",
                              x_axis_suffix = "%",
                              x_axis_absolute = FALSE,
                              annotation_suffix = "%",
                              player = players_reactive_values$profile_player_name,
                              season = players_reactive_values$profile_player_season)
                ),
                bs4TabPanel(
                    tabName = "Pass Quality",
                    active = FALSE,
                    p("Coming soon.")
                ),
                bs4TabPanel(
                    tabName = "Progressive Passes",
                    active = FALSE,
                    violin_d3(data_frame = all_players_stats,
                              metric = "progressive_passes_p96",
                              metric_percentage = FALSE,
                              precision = 0.1,
                              tooltip_precision = 0.01,
                              x_axis_title = "Progressive Passes per 96 Minutes Played",
                              x_axis_suffix = "",
                              x_axis_absolute = TRUE,
                              annotation_suffix = " passes",
                              player = players_reactive_values$profile_player_name,
                              season = players_reactive_values$profile_player_season)
                ),
                bs4TabPanel(
                    tabName = "Dribbling",
                    active = FALSE,
                    violin_d3(data_frame = all_players_stats,
                              metric = "successful_dribbles_p96",
                              metric_percentage = FALSE,
                              precision = 0.05,
                              tooltip_precision = 0.01,
                              x_axis_title = "Successful Dribbles per 96 Minutes Played",
                              x_axis_suffix = "",
                              x_axis_absolute = TRUE,
                              annotation_suffix = " dribbles",
                              player = players_reactive_values$profile_player_name,
                              season = players_reactive_values$profile_player_season)
                ),
                bs4TabPanel(
                    tabName = "Ball Security",
                    active = FALSE,
                    violin_d3(data_frame = all_players_stats,
                              metric = "ball_security",
                              metric_percentage = FALSE,
                              precision = 2.5,
                              tooltip_precision = 1,
                              x_axis_title = "Pass Attempts per Turnover",
                              x_axis_suffix = "",
                              x_axis_absolute = FALSE,
                              annotation_suffix = " attempts",
                              player = players_reactive_values$profile_player_name,
                              season = players_reactive_values$profile_player_season)
                ),
                bs4TabPanel(
                    tabName = "Defensive Actions",
                    active = FALSE,
                    violin_d3(data_frame = all_players_stats,
                              metric = "defensive_actions_p96",
                              metric_percentage = FALSE,
                              precision = 0.25,
                              tooltip_precision = 0.1,
                              x_axis_title = "Defensive Actions per 96 Minutes Played",
                              x_axis_suffix = "",
                              x_axis_absolute = TRUE,
                              annotation_suffix = " actions",
                              player = players_reactive_values$profile_player_name,
                              season = players_reactive_values$profile_player_season)
                ),
                bs4TabPanel(
                    tabName = "Tackle Success",
                    active = FALSE,
                    violin_d3(data_frame = all_players_stats,
                              metric = "tackle_success",
                              metric_percentage = TRUE,
                              precision = 1,
                              tooltip_precision = 0.1,
                              x_axis_title = "Tackle Win Rate (%)",
                              x_axis_suffix = "%",
                              x_axis_absolute = FALSE,
                              annotation_suffix = "%",
                              player = players_reactive_values$profile_player_name,
                              season = players_reactive_values$profile_player_season)
                ),
                bs4TabPanel(
                    tabName = "Recoveries",
                    active = FALSE,
                    violin_d3(data_frame = all_players_stats,
                              metric = "recoveries_p96",
                              metric_percentage = FALSE,
                              precision = 0.1,
                              tooltip_precision = 0.1,
                              x_axis_title = "Loose Balls Recovered per 96 Minutes Played",
                              x_axis_suffix = "",
                              x_axis_absolute = FALSE,
                              annotation_suffix = " recoveries",
                              player = players_reactive_values$profile_player_name,
                              season = players_reactive_values$profile_player_season)
                )
            ),
            width = 12
        )
    })

    output$player_profile_basic_info <- renderUI({
        player_profile_basic_info_reactive()
    })

    output$player_profile_violin_plots <- renderUI({
        player_profile_violin_plots_reactive()
    })
})
