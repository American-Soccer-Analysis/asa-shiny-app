# Import libraries ------------------------------
library(bs4Dash)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(shiny.router)
library(eeptools)
library(jsonlite)
library(httr)
library(r2d3)
library(ggplot2)
library(tools)
library(DT)
library(stringi)
library(tidyverse)


# Set universal variables -----------------------
STAGE <- ifelse(grepl("stage", getwd()), "stage/", "")
API_PATH <- paste0("https://app.americansocceranalysis.com/", STAGE, "api/v1/")

# VIOLIN_HEIGHT <- "450px"
# VIOLIN_WIDTH <- "96%"

FIELD_WIDTH <- 80
FIELD_LENGTH <- 115

DATABASE_TIMEZONE <- "America/New_York"

PATTERNS_OF_PLAY <- c("Corner", "Fastbreak", "Free kick", "Penalty", "Regular", "Set piece")
THIRDS_OF_FIELD <- c("Attacking", "Middle", "Defensive")

MLSPA_POSITIONS <- c("GK", "D", "M", "F")


# Source dashboard utils ------------------------
all_utils <- list.files("utils", recursive = TRUE, full.names = TRUE)
utils_to_source <- all_utils[!grepl("retrieve_data|reactive_values", all_utils)]

lapply(utils_to_source, source)


# Source configs --------------------------------
lapply(list.files("config", recursive = TRUE, full.names = TRUE), source)
league_schemas <- names(league_config)


# Initialize shiny router -----------------------
router <- make_router(
    for (league in league_schemas) {
        route(paste0("/", league), home_ui, NA)
        tabs <- flatten(league_config[[league]][["tabs"]])
        for (tab in tabs) {
            header <- tab$route_link
            subheaders <- tab$subheaders
            if (!is.null(subheaders)) {
                for (s in subheaders) {
                    route(paste0("/", league, "/", header, "/", tolower(s)),
                          get(paste(header, tolower(s), "ui", sep = "_")),
                          get(paste(header, tolower(s), "server", sep = "_")))
                }
            } else {
                route(paste0("/", league, "/", header),
                      get(paste(header, "ui", sep = "_")),
                      get(paste(header, "server", sep = "_")))
            }
        }
    }
)


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
