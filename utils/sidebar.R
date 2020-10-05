sidebar_ui <- function(page, league_config, subheaders_rv) {
    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    headers <- names(league_config[[league]][["tabs"]])

    menu_list <- list()
    i <- 1

    # for (h in headers) {
    #     tab_header <- league_config[[league]][["tabs"]][[h]]
    #     menu_items <- names(tab_header)
    #
    #     menu_list[[i]] <- bs4SidebarHeader(h); i <- i + 1
    #
    #     for (m in menu_items) {
    #         tab_name_prefix <- paste0(league, "/", tab_header[[m]][["route_link"]])
    #         menu_list[[i]] <- bs4SidebarMenuItem(
    #             text = tab_header[[m]][["display_name"]],
    #             tabName = paste0(tab_name_prefix, "/", subheaders_rv[[tab_name_prefix]]),
    #             icon = tab_header[[m]][["icon"]]
    #         ); i <- i + 1
    #     }
    # }
    #
    # bs4SidebarMenu(
    #     id = "asa_sidebar",
    #     bs4SidebarMenuItem(
    #         "Home",
    #         tabName = league,
    #         icon = "home"
    #     ),
    #     menu_list
    # )

    for (h in headers) {
        tab_header <- league_config[[league]][["tabs"]][[h]]
        menu_items <- names(tab_header)

        menu_list[[i]] <- tags$li(class = "nav-header", h); i <- i + 1

        for (m in menu_items) {
            tab_name_prefix <- paste0(league, "/", tab_header[[m]][["route_link"]])
            menu_list[[i]] <- tags$li(
                class = "nav-item",
                a(
                    class = ifelse(assemble_key(league, route_prefix) == tab_name_prefix, "nav-link active", "nav-link"),
                    id = paste0("tab-", paste0(tab_name_prefix, "/", subheaders_rv[[tab_name_prefix]])),
                    href = route_link(paste0(tab_name_prefix, "/", subheaders_rv[[tab_name_prefix]])),
                    icon(
                        tab_header[[m]][["icon"]],
                        class = "nav-icon"
                        ),
                    p(tab_header[[m]][["display_name"]])
                )
            ); i <- i + 1
        }
    }

    tags$ul(
        class = "nav nav-pills nav-sidebar flex-column",
        id = "mymenu",
        tags$li(
            class = "nav-item",
            a(
                class = ifelse(page == league, "nav-link active", "nav-link"),
                id = paste0("tab-", league),
                href = route_link(league),
                icon(
                    "home",
                    class = "nav-icon"
                ),
                p("Home")
            )
        ),
        menu_list
    )
}
