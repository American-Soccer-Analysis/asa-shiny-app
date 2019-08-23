navbar <- bs4DashNavbar(
     skin = "dark",
     status = "white",
     border = TRUE,
     sidebarIcon = "bars",
     controlbarIcon = "th",
     fixed = FALSE,
     rightUi = div(div(id = "nav_mls", class = "nav_league",
                   a(href = "#", img(src = "mls_logo_white.png", height = "32px"))),
                   div(id = "nav_nwsl", class = "nav_league",
                       a(href = "#", img(src = "nwsl_logo_white.png", height = "32px"))),
                   div(id = "nav_usl_c", class = "nav_league",
                       a(href = "#", img(src = "usl_c_logo_white.png", height = "32px"))))
)