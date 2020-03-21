shinyServer(function(input, output, session) {
    start_player <- 31740   # Dax

    players_reactive_values <- reactiveValues(profile_player_name = start_player,
                                              profile_player_season = max(all_players_seasons$season_name[all_players_seasons$player_id == start_player]))

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
                                      selected = max(unique(all_players_seasons$season_name)),
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
        if(input$profile_player_name != "") {
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
                    img(src = all_players$headshot_url[all_players$player_id == players_reactive_values$profile_player_name])),
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

    output$player_profile_basic_info <- renderUI({
        player_profile_basic_info_reactive()
    })
})
