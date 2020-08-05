# Set universal variables -----------------------
STAGE <- ifelse(grepl("stage", getwd()), "stage/", "")
API_PATH <- paste0("https://app.americansocceranalysis.com/", STAGE, "api/v1/", LEAGUE_SCHEMA, "/")

# VIOLIN_MINUTES_CUTOFF <- 500
# VIOLIN_HEIGHT <- "450px"
# VIOLIN_WIDTH <- "96%"
# START_PLAYER <- NA   # Dax

FIELD_WIDTH <- 80
FIELD_LENGTH <- 115

DATABASE_TIMEZONE <- "America/New_York"

PATTERNS_OF_PLAY <- c("Corner", "Fastbreak", "Free kick", "Penalty", "Regular", "Set piece")
THIRDS_OF_FIELD <- c("Attacking", "Middle", "Defensive")

MAX_MINUTES <- 3000
MAX_SHOTS_TAKEN_FACED <- 125
MAX_KEY_PASSES <- 125
MAX_PASSES <- 2000

MLSPA_POSITIONS <- c("GK", "D", "M", "F")


# Utility functions -----------------------------
api_request <- function(path = API_PATH, endpoint, parameters = NULL) {
    parameters_array <- c()

    if (length(parameters) > 0) {
        for (i in 1:length(parameters)) {
            tmp_name <- names(parameters[i])
            tmp_value <- parameters[[tmp_name]]

            if (all(!is.na(tmp_value)) & all(!is.null(tmp_value))) {
                if (length(tmp_value) > 1) {
                    tmp_value <- gsub("\\s+", "%20", paste0(tmp_value, collapse = ","))
                } else {
                    tmp_value <- gsub("\\s+", "%20", tmp_value)
                }

                parameters_array <- c(parameters_array, paste0(tmp_name, "=", tmp_value))
            }
        }
    }

    parameters_array <- ifelse(length(parameters_array) > 0,
                               paste0("?", paste0(parameters_array, collapse = "&")),
                               "")

    return(fromJSON(content(GET(paste0(API_PATH, endpoint, parameters_array)),
                            as = "text", encoding = "UTF-8")))
}

# Source dashboard utils ------------------------
utils <- paste0("../app/utils/", list.files("../app/utils")[!grepl("retrieve_data|reactive_values", list.files("../app/utils"))])
lapply(utils, source)
