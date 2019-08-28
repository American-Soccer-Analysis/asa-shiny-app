shinyUI(
        tagList(
                useShinyjs(),
                tags$head(
                        HTML('<link rel="stylesheet" href="https://use.typekit.net/zmt3hir.css">'),
                        includeCSS("www/asa.css"),
                        tags$script("$(document).ready(function() {
                                     $('.fa-th').removeClass('fa-th').addClass('fa-cog');
                                     });")
                ),
                bs4DashPage(
                        old_school = FALSE,
                        sidebar_collapsed = FALSE,
                        controlbar_collapsed = TRUE,
                        title = "American Soccer Analysis | Major League Soccer",
                        navbar = navbar,
                        sidebar = sidebar,
                        footer = bs4DashFooter(),
                        body = bs4DashBody(profile_player),
                        controlbar = controlbar
                )
        )
)