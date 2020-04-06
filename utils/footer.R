# Wrapper div -----------------------------------
footer <- bs4DashFooter(
    uiOutput("asa_footer")
)

# Get data last updated date --------------------
footer_reactive <- function(all_games, client_timezone) {
    last_updated <- as.POSIXct(max(all_games$last_updated_et, na.rm = TRUE), tz = "America/New_York")
    attributes(last_updated)$tzone <- client_timezone

    last_updated_date <- format(last_updated, "%B %M, %Y")
    last_update_hour <- as.numeric(format(last_updated, "%H"))
    last_updated_time <- gsub("^0", "", paste0(format(last_updated, "%I:%M"), ifelse(last_update_hour < 12, " a.m.", " p.m.")))

    div(paste0("Data last refreshed: ", last_updated_date, " at ", last_updated_time))
}
