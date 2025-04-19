shinyUI(
    tagList(
        useShinyjs(),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "https://use.typekit.net/zmt3hir.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "asa.css"),
            tags$link(rel = "shortcut icon", href = "favicon.ico"),
            tags$script(src = "js/controlbar_cog.js"),
            tags$script(src = "js/client_timezone.js")
        ),
        div(id = "loader_page",
            div(class = "lds-ripple",
                div(),
                div(),
                p("Loading..."))
        ),
        dashboardPage(
            title = "American Soccer Analysis | App",
            header = bs4DashNavbar(
                skin = "dark",
                status = "white",
                border = TRUE,
                sidebarIcon = shiny::icon("bars"),
                controlbarIcon = shiny::icon("gear"),
                fixed = FALSE,
                compact = TRUE,
                rightUi = uiOutput("asa_navbar")
            ),
            sidebar = bs4DashSidebar(
                skin = "dark",
                status = "primary",
                title = "American Soccer Analysis",
                elevation = 0,
                collapsed = TRUE,
                minified = TRUE,
                expandOnHover = TRUE,
                uiOutput("asa_sidebar_reactive")
            ),
            footer = bs4DashFooter(
                uiOutput("asa_footer")
            ),
            body = bs4DashBody(
                div(
                    id = "asa_body",
                    router
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
