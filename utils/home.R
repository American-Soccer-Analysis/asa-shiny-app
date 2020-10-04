home_div <- div(
    id = "home_wrapper",
    div(
        column(
            12,
            bs4Jumbotron(title = "Welcome to the new American Soccer Analysis app!",
                         lead = "We built this interactive web application to give ASA's loyal readers more autonomy to sort, filter, and download our data. It's constantly evolving, and we'd love your input along the way.",
                         btn_name = NULL),
            fluidRow(
                bs4Box(title = "Methodology Resources",
                       bs4ListGroup(
                           bs4ListGroupItem(
                               "American Soccer Analysis",
                               type = "heading",
                               title = "xGoals",
                               src = "https://www.americansocceranalysis.com/explanation/"
                           ),
                           bs4ListGroupItem(
                               "American Soccer Analysis",
                               type = "heading",
                               title = "xPass",
                               src = "https://www.americansocceranalysis.com/home/2018/4/19/an-updated-expected-passing-model"
                           ),
                           bs4ListGroupItem(
                               "American Soccer Analysis",
                               type = "heading",
                               title = "Goals Added (g+)",
                               src = "https://www.americansocceranalysis.com/what-are-goals-added"
                           ),
                           width = 12
                       ),
                       width = 6),
                bs4Box(title = "App Resources",
                       bs4ListGroup(
                           bs4ListGroupItem(
                               "GitHub",
                               type = "heading",
                               title = "Report an Issue",
                               src = "https://github.com/American-Soccer-Analysis/asa-shiny-app/issues"
                           ),
                           bs4ListGroupItem(
                               "GitHub",
                               type = "heading",
                               title = "Request a New Feature",
                               src = "https://github.com/American-Soccer-Analysis/asa-shiny-app/issues"
                           ),
                           bs4ListGroupItem(
                               "GitHub",
                               type = "heading",
                               title = "Read the Latest Release Notes",
                               src = "https://github.com/American-Soccer-Analysis/asa-shiny-app/releases"
                           ),
                           width = 12
                       ),
                       width = 6)
            )
        )
    ),

)
