# Import libraries ------------------------------
library(shiny)
library(bs4Dash)
library(shinyjs)
library(shinyWidgets)
library(eeptools)
library(jsonlite)
library(httr)
library(r2d3)
library(ggplot2)
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

# Custom functions ------------------------------
api_request <- function(path = API_PATH, endpoint) {
    return(fromJSON(content(GET(paste0(API_PATH, endpoint)),
                            as = "text", encoding = "UTF-8")))
}

# Source dashboard utils ------------------------
utils <- paste0("utils/", list.files("utils")[!grepl("retrieve_data", list.files("utils"))])
lapply(utils, source)
