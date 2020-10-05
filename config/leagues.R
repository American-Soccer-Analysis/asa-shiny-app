league_config <- list(
    mls = list(
        name = "Major League Soccer",
        stages = c("Regular Season", "Playoffs", "MLS is Back Group Stage", "MLS is Back Knockout Round"),
        tabs = list(
            Tables = list(
                xgoals = list(
                    display_name = "xGoals",
                    route_link = "xgoals",
                    icon = "futbol-o",
                    subheaders = c("Players", "Teams", "Games", "Goalkeepers"),
                    ui = "tables_ui",
                    server = NA
                ),
                xpass = list(
                    display_name = "xPass",
                    route_link = "xpass",
                    icon = "bullseye",
                    subheaders = c("Players", "Teams"),
                    ui = "tables_ui",
                    server = NA
                ),
                goals_added = list(
                    display_name = "Goals Added (g+)",
                    route_link = "goals_added",
                    icon = "google-plus-g",
                    subheaders = c("Players", "Teams"),
                    ui = "tables_ui",
                    server = NA
                ),
                salaries = list(
                    display_name = "Salaries",
                    route_link = "salaries",
                    icon = "usd",
                    subheaders = c("Players", "Teams"),
                    ui = "tables_ui",
                    server = NA
                )
            )
        )
    ),
    nwsl = list(
        name = "National Women's Soccer League",
        stages = c("Regular Season", "Playoffs", "NWSL Challenge Cup Group Stage", "NWSL Challenge Cup Knockout Round"),
        tabs = list(
            Tables = list(
                xgoals = list(
                    display_name = "xGoals",
                    route_link = "xgoals",
                    icon = "futbol-o",
                    subheaders = c("Players", "Teams", "Games", "Goalkeepers"),
                    ui = "tables_ui",
                    server = NA
                ),
                xpass = list(
                    display_name = "xPass",
                    route_link = "xpass",
                    icon = "bullseye",
                    subheaders = c("Players", "Teams"),
                    ui = "tables_ui",
                    server = NA
                ),
                goals_added = list(
                    display_name = "Goals Added (g+)",
                    route_link = "goals_added",
                    icon = "google-plus",
                    subheaders = c("Players", "Teams"),
                    ui = "tables_ui",
                    server = NA
                )
            )
        )
    )
)
