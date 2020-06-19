source("renv/activate.R")

# Import libraries ------------------------------
library(bs4Dash)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(eeptools)
library(jsonlite)
library(httr)
library(r2d3)
library(ggplot2)
library(tools)
library(DT)
library(stringi)
library(tidyverse)

# Set global variables --------------------------
API_PATH <- "https://app.americansocceranalysis.com/api/v1"
VIOLIN_MINUTES_CUTOFF <- 500
VIOLIN_HEIGHT <- "450px"
VIOLIN_WIDTH <- "96%"
START_PLAYER <- 31740   # Dax
FIELD_WIDTH <- 80
FIELD_LENGTH <- 115
DATABASE_TIMEZONE <- "America/New_York"
PATTERNS_OF_PLAY <- c("Corner", "Fastbreak", "Free kick", "Penalty", "Regular", "Set piece")
THIRDS_OF_FIELD <- c("Attacking", "Middle", "Defensive")
MLSPA_POSITIONS <- c("GK", "D", "M", "F")
MAX_MINUTES <- 3000
MAX_SHOTS_TAKEN_FACED <- 125
MAX_KEY_PASSES <- 125

# Custom functions ------------------------------
dummy_function <- function() {}

api_request <- function(path = API_PATH, endpoint, parameters = NULL) {
    parameters_array <- c()

    if (length(parameters) > 0) {
        for (i in 1:length(parameters)) {
            tmp_name <- names(parameters[i])
            tmp_value <- parameters[[tmp_name]]

            if (all(!is.na(tmp_value)) & all(!is.null(tmp_value))) {
                if (length(tmp_value) > 1) {
                    tmp_value <- gsub("\\s+", "%20", paste0(tmp_value, collapse = ","))
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
utils <- paste0("utils/", list.files("utils")[!grepl("retrieve_data|reactive_values", list.files("utils"))])
lapply(utils, source)
