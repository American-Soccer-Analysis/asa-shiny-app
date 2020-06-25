navbar <- bs4DashNavbar(
     skin = "dark",
     status = "white",
     border = TRUE,
     sidebarIcon = "bars",
     controlbarIcon = "th",
     fixed = FALSE,
     rightUi = div(div(id = "nav_mls", class = ifelse(LEAGUE_SCHEMA == "mls", "nav_league nav_league_active", "nav_league"),
                       a(href = ifelse(LEAGUE_SCHEMA == "mls", "#", "../mls"), img(src = "mls_logo_white.png", height = "32px"))),
                   div(id = "nav_nwsl", class = ifelse(LEAGUE_SCHEMA == "nwsl", "nav_league nav_league_active", "nav_league"),
                       a(href = ifelse(LEAGUE_SCHEMA == "nwsl", "#", "../nwsl"), img(src = "nwsl_logo_white.png", height = "32px"))))
)