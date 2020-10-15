# Default reactive values -----------------------
players_rv <- reactiveValues()

for (l in league_schemas) {
    players_rv[[l]] <- list(
        profiles_players_name = case_when(l == "mls" ~ "ljqEoboMx0",  # Dax
                                          l == "nwsl" ~ "AB5AWZBIFyZCLAZCKUKHOF35J")  # Midge
    )
}
