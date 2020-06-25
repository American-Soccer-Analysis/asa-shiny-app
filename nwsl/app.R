# Source league-specific setup file -------------
source("nwsl.R")

# Source universal files ------------------------
source("../app/global.R")
source("../app/ui.R")
source("../app/server.R")

# Run app ---------------------------------------
shinyApp(ui = ui(LEAGUE_SCHEMA, LEAGUE_NAME), server = server)
