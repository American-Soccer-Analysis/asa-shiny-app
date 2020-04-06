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
        bs4DashPage(
            old_school = FALSE,
            sidebar_collapsed = TRUE,
            controlbar_collapsed = TRUE,
            title = "American Soccer Analysis | Major League Soccer",
            navbar = navbar,
            sidebar = sidebar,
            footer = footer,
            body = body,
            controlbar = controlbar
        )
    )
)