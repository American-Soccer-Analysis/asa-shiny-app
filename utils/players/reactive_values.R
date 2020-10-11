# Default reactive values -----------------------
players_rv <- reactiveValues()

for (l in league_schemas) {
    players_rv[[paste0(l, "/players")]] <- list(
        profiles_players_name = league_config[[l]]$tabs$Profiles$players$default_selection
    )
}
