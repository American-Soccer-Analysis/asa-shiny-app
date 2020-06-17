# TABLES: XGOALS --------------------------------
controlbar_tables <- function(header, subheader, tables_rv) {

    if (grepl("xgoals", header)) {
        if (grepl("Players", subheader)) {
            div(
                column(12,
                       h4("Player Settings"),
                       tables_cb_slider(header, subheader, tables_rv, MAX_MINUTES,
                                        "Minimum Minutes Played", "minimum_minutes"),
                       tables_cb_slider(header, subheader, tables_rv, MAX_SHOTS_TAKEN_FACED,
                                        "Minimum Shots Taken", "minimum_shots"),
                       tables_cb_slider(header, subheader, tables_rv, MAX_KEY_PASSES,
                                        "Minimum Key Passes", "minimum_key_passes"),
                       tables_cb_picker(header, subheader, tables_rv, "Teams", "team_id",
                                        all_teams$team_id, all_teams$team_abbreviation),
                       tables_cb_picker(header, subheader, tables_rv, "Patterns of Play", "shot_pattern", PATTERNS_OF_PLAY),
                       tables_cb_date_filter(header, subheader, tables_rv, all_seasons),
                       p(class = "control-label", "Group Results"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Teams", "split_by_teams"),
                       tables_cb_switch(header, subheader, tables_rv, "Split by Seasons", "split_by_seasons"),
                       tables_cb_refresh(header, subheader)
                )
            )
        }
    }

}


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
