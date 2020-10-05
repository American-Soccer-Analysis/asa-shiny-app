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
                      });"),
            tags$link(rel = "shortcut icon", href = "favicon.ico")
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
            title = "American Soccer Analysis | App",
            navbar = bs4DashNavbar(
                skin = "dark",
                status = "white",
                border = TRUE,
                sidebarIcon = "bars",
                controlbarIcon = "th",
                fixed = FALSE,
                rightUi = uiOutput("asa_navbar")
            ),
            sidebar = bs4DashSidebar(
                skin = "dark",
                status = "primary",
                title = "American Soccer Analysis",
                brandColor = "primary",
                src = "asa_assets/asa_icon_white.png",
                elevation = 0,
                opacity = 1,
                uiOutput("asa_sidebar_reactive")
            ),
            footer = bs4DashFooter(
                uiOutput("asa_footer")
            ),
            body = bs4DashBody(
                div(
                    id = "asa_body",
                    router_ui()
                )
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
