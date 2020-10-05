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
API_PATH <- paste0("https://app.americansocceranalysis.com/", STAGE, "api/v1")

# VIOLIN_HEIGHT <- "450px"
# VIOLIN_WIDTH <- "96%"

FIELD_WIDTH <- 80
FIELD_LENGTH <- 115

DATABASE_TIMEZONE <- "America/New_York"

PATTERNS_OF_PLAY <- c("Corner", "Fastbreak", "Free kick", "Penalty", "Regular", "Set piece")
THIRDS_OF_FIELD <- c("Attacking", "Middle", "Defensive")

TRUNCATED_GAMESTATES <- -2:2
TRUNCATED_GAMESTATES_LABELS <- as.character(c("≤ -2", -1:1, "≥ 2"))
FIELD_ZONES <- 30:1

MLSPA_POSITIONS <- c("GK", "D", "M", "F")


# Source dashboard utils ------------------------
all_utils <- list.files("utils", recursive = TRUE, full.names = TRUE)
utils_to_source <- all_utils[!grepl("retrieve_data|reactive_values", all_utils)]

lapply(utils_to_source, source)


# Source configs --------------------------------
lapply(list.files("config", recursive = TRUE, full.names = TRUE), source)
league_schemas <- names(league_config)


# Initialize shiny router -----------------------
router_list_to_parse <- "router <- make_router(default = "

for (league in league_schemas) {
    router_list_to_parse <- paste0(router_list_to_parse, "route(\"",
                                   league, "\", ",
                                   "home_ui", ", ",
                                   NA, "), ")
    tabs <- flatten(league_config[[league]][["tabs"]])
    for (tab in tabs) {
        header <- tab$route_link
        subheaders <- tab$subheaders
        if (!is.null(subheaders)) {
            for (s in subheaders) {
                router_list_to_parse <- paste0(router_list_to_parse, "route(\"",
                                               paste0(league, "/", header, "/", tolower(s)), "\", ",
                                               tab$ui, ", ",
                                               tab$server,  "), ")
            }
        } else {
            router_list_to_parse <- paste0(router_list_to_parse, "route(\"",
                                           paste0(league, "/", header), "\", ",
                                           tab$ui, ", ",
                                           tab$server,  "), ")
        }
    }
}

router_list_to_parse <- gsub(",\\s+$", ")", router_list_to_parse)

eval(parse(text = router_list_to_parse))


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

get_config_element <- function(league, sidebar_header, route_prefix, league_config, element) {
    return(league_config[[league]][["tabs"]][[sidebar_header]][[route_prefix]][[element]])
}

get_values_from_page <- function(page) {
    league <- gsub("/.*$", "", page)
    subheader <- gsub("^.*/", "", page)
    route_prefix <- gsub("/", "", gsub(league, "", gsub(subheader, "", page)))

    return(list(
        league = league,
        route_prefix = route_prefix,
        subheader = subheader
    ))
}

assemble_key <- function(league, route_prefix, subheader = NULL) {
    if (!is.null(subheader) > 0) {
        keys <- c()
        for (s in subheader) {
            keys <- c(keys, paste0(league, "/", route_prefix, "/", tolower(s)))
        }
    } else {
        keys <- paste0(league, "/", route_prefix)
    }

    return(keys)
}

assemble_endpoint <- function(league, route_prefix = NULL, subheader) {
    return(paste0("/", league, "/", tolower(subheader), ifelse(!is.null(route_prefix), paste0("/", route_prefix), "")))
}
