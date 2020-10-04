league_config <- list(
    mls = list(
        name = "Major League Soccer",
        stages = c("Regular Season", "Playoffs", "MLS is Back Group Stage", "MLS is Back Knockout Round"),
        tabs = list(
            Tables = list(
                xGoals = list(
                    route_link = "xgoals",
                    icon = "futbol-o",
                    subheaders = c("Players", "Teams", "Games", "Goalkeepers")
                ),
                xPass = list(
                    route_link = "xpass",
                    icon = "bullseye",
                    subheaders = c("Players", "Teams")
                ),
                `Goals Added (g+)` = list(
                    route_link = "goals_added",
                    icon = "google-plus",
                    subheaders = c("Players", "Teams")
                ),
                Salaries = list(
                    route_link = "salaries",
                    icon = "usd",
                    subheaders = c("Players", "Teams")
                )
            )
        )
    ),
    nwsl = list(
        name = "National Women's Soccer League",
        stages = c("Regular Season", "Playoffs", "NWSL Challenge Cup Group Stage", "NWSL Challenge Cup Knockout Round"),
        tabs = list(
            Tables = list(
                xGoals = list(
                    route_link = "xgoals",
                    icon = "futbol-o",
                    subheaders = c("Players", "Teams", "Games", "Goalkeepers")
                ),
                xPass = list(
                    route_link = "xpass",
                    icon = "bullseye",
                    subheaders = c("Players", "Teams")
                ),
                `Goals Added (g+)` = list(
                    route_link = "goals_added",
                    icon = "google-plus",
                    subheaders = c("Players", "Teams")
                )
            )
        )
    )
)
