navbar_ui <- function(page, league_config) {
    league <- gsub("/.*$", "", gsub("^/", "", page))
    div(
        lapply(names(league_config), function(l) {
            div(
                id = paste0("nav_", l),
                class = ifelse(l == league, "nav_league nav_league_active", "nav_league"),
                a(
                    href = ifelse(l == league, "#", route_link(gsub(league, l, page))),
                    img(src = paste0("league_logos/", l, "_logo_white.png"), height = "32px")
                )
            )
        })
    )
}
