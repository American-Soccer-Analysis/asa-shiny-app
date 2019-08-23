shinyUI(
        tagList(
                useShinyjs(),
                tags$head(
                        HTML('<link rel="stylesheet" href="https://use.typekit.net/biz3ufq.css">'),
                        includeCSS("www/asa.css")
                ),
                bs4DashPage(
                        old_school = FALSE,
                        sidebar_collapsed = FALSE,
                        controlbar_collapsed = TRUE,
                        title = "Basic Dashboard",
                        navbar = navbar,
                        sidebar = sidebar,
                        footer = bs4DashFooter(),
                        body = bs4DashBody()
                )
        )
)