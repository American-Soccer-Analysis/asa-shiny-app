shinyUI(
    tagList(
        useShinyjs(),
        tags$head(
            HTML('<link rel="stylesheet" href="https://use.typekit.net/zmt3hir.css">'),
            includeCSS("www/asa.css"),
            tags$script("$(document).ready(function() {
                             $('.fa-th').removeClass('fa-th').addClass('fa-cog');
                          });"),
            tags$script("$(document).on('shiny:connected', function(event) {
                             var timezone = Intl.DateTimeFormat().resolvedOptions().timeZone
                             Shiny.setInputValue('client_timezone', timezone);
                          });")
        ),
        div(id = "loader_page",
            div(class = "lds-ripple",
                div(),
                div(),
                p("Loading..."))
            ),
        dashboardPage(
            old_school = FALSE,
            sidebar_collapsed = TRUE,
            controlbar_collapsed = TRUE,
            title = "American Soccer Analysis | Major League Soccer",
            navbar = navbar,
            sidebar = bs4DashSidebar(
                skin = "dark",
                status = "primary",
                title = "American Soccer Analysis",
                brandColor = "primary",
                src = "asa_icon_white.png",
                elevation = 0,
                opacity = 1,
                bs4SidebarMenu(
                    id = "asa_sidebar",
                    bs4SidebarMenuItem(
                        "Home",
                        tabName = "home",
                        icon = "home"
                    ),
                    bs4SidebarHeader("Tables"),
                    lapply(tables_menu_items, function(x) {
                        bs4SidebarMenuItem(
                            x$name,
                            tabName = x$tabName,
                            icon = x$icon
                        )
                    })
                    # bs4SidebarHeader("Profiles"),
                    # bs4SidebarMenuItem(
                    #     "Players",
                    #     tabName = "profile_player",
                    #     icon = "user"
                    # ),
                    # bs4SidebarMenuItem(
                    #     "Teams",
                    #     tabName = "profile_teams",
                    #     icon = "shield-alt"
                    # )
                )
            ),
            footer = footer,
            body = bs4DashBody(
                uiOutput("asa_body")
            ),
            controlbar = bs4DashControlbar(
                skin = "light",
                title = NULL,
                width = 350,
                uiOutput("asa_controlbar")
            )
        )
    )
)