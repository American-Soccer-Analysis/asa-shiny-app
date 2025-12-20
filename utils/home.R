home_ui <- div(
    id = "home_wrapper",
    div(
        column(
            12,
            bs4Jumbotron(title = "Welcome to the new American Soccer Analysis app!",
                         lead = "We built this interactive web application to give ASA's loyal readers more autonomy to sort, filter, and download our data. It's constantly evolving, and we'd love your input along the way.",
                         btn_name = NULL),
            fluidRow(
                bs4Card(title = "Methodology Resources",
                       bs4ListGroup(
                           type = "heading",
                           bs4ListGroupItem(
                               "American Soccer Analysis",
                               title = "xGoals",
                               href = "https://www.americansocceranalysis.com/explanation/"
                           ),
                           bs4ListGroupItem(
                               "American Soccer Analysis",
                               title = "xPass",
                               href = "https://www.americansocceranalysis.com/home/2018/4/19/an-updated-expected-passing-model"
                           ),
                           bs4ListGroupItem(
                               "American Soccer Analysis",
                               title = "Goals Added (g+)",
                               href = "https://www.americansocceranalysis.com/what-are-goals-added"
                           ),
                           width = 12
                       ),
                       width = 6,
                       collapsible = FALSE,
                       headerBorder = FALSE),
                bs4Card(title = "App Resources",
                       bs4ListGroup(
                           type = "heading",
                           bs4ListGroupItem(
                               "GitHub",
                               title = "Report an Issue",
                               href = "https://github.com/American-Soccer-Analysis/asa-shiny-app/issues"
                           ),
                           bs4ListGroupItem(
                               "GitHub",
                               title = "Request a New Feature",
                               href = "https://github.com/American-Soccer-Analysis/asa-shiny-app/issues"
                           ),
                           bs4ListGroupItem(
                               "GitHub",
                               title = "Read the Latest Release Notes",
                               href = "https://github.com/American-Soccer-Analysis/asa-shiny-app/releases"
                           ),
                           width = 12
                       ),
                       width = 6,
                       collapsible = FALSE,
                       headerBorder = FALSE)
            )
        )
    ),
)
