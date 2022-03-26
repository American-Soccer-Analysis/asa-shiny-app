sidebar_ui <- function(page, tab_config, subheaders_rv) {
    league <- get_values_from_page(page)$league
    route_prefix <- get_values_from_page(page)$route_prefix
    tab_groups <- sapply(tab_config, names)

    menu_list <- list()
    i <- 1

    for (tab_group in tab_groups) {
        j <- which(tab_groups == tab_group)
        tabs <- tab_config[[j]][[tab_group]]

        menu_list[[i]] <- tags$li(class = "nav-header", tab_group)
        i <- i + 1

        for (tab in tabs) {
            if (league %in% tab$leagues) {
                tab_name_prefix <- tab$route_link
                subheader <- subheaders_rv[[assemble_key(league, tab_name_prefix)]]

                menu_list[[i]] <- tags$li(
                    class = "nav-item",
                    a(
                        class = ifelse(assemble_key(league, route_prefix) == assemble_key(league, tab_name_prefix), "nav-link active", "nav-link"),
                        id = paste0("tab-", assemble_key(league, tab_name_prefix, subheader)),
                        href = route_link_patched(assemble_key(league, tab_name_prefix, subheader)),
                        icon(
                            tab$icon,
                            class = "nav-icon"
                            ),
                        p(tab$display_name)
                    )
                )

                i <- i + 1
            }
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
                href = route_link_patched(league),
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
