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
# API_PATH <- "http://127.0.0.1:8001"

VIOLIN_HEIGHT <- "400px"
VIOLIN_WIDTH <- "96%"
VIOLIN_MINIMUM_MINUTES <- 500

FIELD_WIDTH <- 80
FIELD_LENGTH <- 115

DATABASE_TIMEZONE <- "UTC"

PATTERNS_OF_PLAY <- c("Corner", "Fastbreak", "Free kick", "Penalty", "Regular", "Set piece")
THIRDS_OF_FIELD <- c("Attacking", "Middle", "Defensive")

GOALS_ADDED_ACTION_TYPES <- c("Dribbling", "Fouling", "Interrupting", "Passing", "Receiving", "Shooting")
GOALS_ADDED_GK_ACTION_TYPES <- c("Claiming", "Fielding", "Handling", "Passing", "Shotstopping", "Sweeping")

TRUNCATED_GAMESTATES <- -2:2
TRUNCATED_GAMESTATES_LABELS <- as.character(c("≤ -2", -1:1, "≥ 2"))
FIELD_ZONES <- 30:1

MLSPA_POSITIONS <- c("GK", "D", "M", "F")

MAX_API_LIMIT <- 1000


# Source dashboard utils ------------------------
all_utils <- list.files("utils", recursive = TRUE, full.names = TRUE)
utils_to_source <- all_utils[!grepl("retrieve_data|reactive_values", all_utils)]

lapply(utils_to_source, source)


# Source all configs ----------------------------
config_yaml <- yaml::read_yaml("config.yaml")

league_config <- config_yaml$leagues
tab_config <- config_yaml$tabs
column_config <- config_yaml$columns

# League configs --------------------------------
league_schemas <- sapply(league_config, "[[", "schema")

# Table configs ---------------------------------
tmp_tables <- do.call(dplyr::bind_rows, column_config)

tables_column_name_map <- tmp_tables %>% dplyr::select(app_name, api_name)
tables_column_tooltip_text <- tmp_tables %>% dplyr::select(api_name, tooltip_text) %>% dplyr::filter(!is.na(tooltip_text))
tables_percentage_columns <- unique(tmp_tables$app_name[!is.na(tmp_tables$percentage)])
tables_currency_columns <- unique(tmp_tables$app_name[!is.na(tmp_tables$currency)])
tables_normalize_columns <- unique(tmp_tables$api_name[!is.na(tmp_tables$normalize)])


# Initialize shiny router -----------------------
tab_groups <- sapply(tab_config, names)
router_list_to_parse <- "router <- make_router(default = "

for (league in league_schemas) {
    router_list_to_parse <- paste0(router_list_to_parse, "route(\"",
                                   league, "\", ",
                                   "home_ui", ", ",
                                   NA, "), ")
    for (tab_group in tab_groups) {
        i <- which(tab_groups == tab_group)
        tabs <- tab_config[[i]][[tab_group]]
        for (tab in tabs) {
            if (league %in% tab$leagues) {
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
    }
}

router_list_to_parse <- gsub(",\\s+$", ")", router_list_to_parse)
eval(parse(text = router_list_to_parse))


# Create lookup for controlbar panel ------------
controlbar_lookup <- list()

for (league in league_schemas) {
    for (tab_group in tab_groups) {
        i <- which(tab_groups == tab_group)
        tabs <- tab_config[[i]][[tab_group]]
        for (tab in tabs) {
            tab_name_prefix <- tab$route_link
            subheaders <- tab$subheaders
            if (!is.null(subheaders)) {
                for (s in subheaders) {
                    controlbar_lookup[[paste0(league, "/", tab_name_prefix, "/", tolower(s))]] <- tab_group
                }
            } else {
                controlbar_lookup[[paste0(league, "/", tab_name_prefix)]] <- tab_group
            }
        }
    }
}


# Utility functions -----------------------------
single_request <- function(path, endpoint, parameters) {
    for (param_name in names(parameters)) {
        if (length(parameters[[param_name]]) > 1) {
            parameters[[param_name]] <- paste0(parameters[[param_name]], collapse = ",")
        }
    }

    resp <- httr::GET(
        url = paste0(path, endpoint),
        query = parameters
    ) %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON()

    return(resp)
}

api_request <- function(path = API_PATH, endpoint, parameters = list()) {
    tmp_resp <- single_request(path, endpoint, parameters)
    resp <- tmp_resp

    if (is.data.frame(tmp_resp)) {
        offset <- MAX_API_LIMIT

        while (nrow(tmp_resp) == MAX_API_LIMIT) {
            parameters$offset <- offset
            tmp_resp <- single_request(path, endpoint, parameters)

            resp <- resp %>% bind_rows(tmp_resp)
            offset <- offset + MAX_API_LIMIT
        }
    }

    return(resp)
}

get_config_element <- function(league, tab_group, route_prefix, tab_config, element) {
    tab_groups <- sapply(tab_config, names)
    i <- which(tab_groups == tab_group)

    tab <- tab_config[[i]][[tab_group]]
    route_links <- sapply(tab, "[[", "route_link")
    j <- which(route_links == route_prefix)

    return(tab[[j]][[element]])
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
