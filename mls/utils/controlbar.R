# PROFILES: PLAYERS -----------------------------
controlbar_profile_player <- function(players_dropdown, players_reactive_values, all_players_seasons) {
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
