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

VIOLIN_HEIGHT <- "450px"
VIOLIN_WIDTH <- "96%"

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


# Source all configs ----------------------------
lapply(list.files("config", recursive = TRUE, full.names = TRUE), source)

# League configs --------------------------------
league_schemas <- names(league_config)

# Table configs ---------------------------------
tmp_tables <- do.call(bind_rows, tables_config) %>% mutate(api_name = names(tables_config))

tables_column_name_map <- tmp_tables %>% select(app_name, api_name)
tables_column_tooltip_text <- tmp_tables %>% select(api_name, tooltip_text) %>% filter(!is.na(tooltip_text))
tables_percentage_columns <- unique(tmp_tables$app_name[!is.na(tmp_tables$percentage)])
tables_currency_columns <- unique(tmp_tables$app_name[!is.na(tmp_tables$currency)])
tables_normalize_columns <- unique(tmp_tables$api_name[!is.na(tmp_tables$normalize)])


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


# Create lookup for controlbar panel ------------
controlbar_lookup <- list()

headers <- names(league_config[[league]][["tabs"]])
for (league in league_schemas) {
    for (h in headers) {
        tab_header <- league_config[[league]][["tabs"]][[h]]
        menu_items <- names(tab_header)
        for (m in menu_items) {
            tab_name_prefix <- tab_header[[m]][["route_link"]]
            subheaders <- tab_header[[m]][["subheaders"]]
            if (!is.null(subheaders)) {
                for (s in subheaders) {
                    controlbar_lookup[[paste0(league, "/", tab_name_prefix, "/", tolower(s))]] <- h
                }
            } else {
                controlbar_lookup[[paste0(league, "/", tab_name_prefix)]] <- h
            }
        }
    }
}


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
    page <- gsub("\\?.*$", "", page)
    league <- gsub("/.*$", "", page)
    route_prefix <- gsub("/.*$", "", gsub("^/", "", gsub(league, "", page)))
    subheader <- gsub("/", "", gsub(route_prefix, "", gsub(league, "", page)))

    return(list(
        league = league,
        route_prefix = ifelse(nchar(route_prefix) == 0, NA, route_prefix),
        subheader = ifelse(nchar(subheader) == 0, NA, subheader)
    ))
}

assemble_key <- function(league, route_prefix = NA, subheader = NA) {
    route_prefix <- if (is.null(route_prefix)) NA else route_prefix
    subheader <- if (is.null(subheader)) NA else subheader

    if (!is.na(subheader) > 0) {
        keys <- c()
        for (s in subheader) {
            keys <- c(keys, paste0(league, "/", route_prefix, "/", tolower(s)))
        }
    } else if (!is.na(route_prefix)) {
        keys <- paste0(league, "/", route_prefix)
    } else {
        keys <- league
    }

    return(keys)
}

assemble_endpoint <- function(league, route_prefix = NA, subheader) {
    return(paste0("/", league, "/", tolower(subheader), ifelse(!is.na(route_prefix), paste0("/", route_prefix), "")))
}


# Patch shiny.router ----------------------------
route_link_patched <- function(path) {
    paste0("#!/", path)
}
