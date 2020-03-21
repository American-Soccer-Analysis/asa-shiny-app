# Import libraries ------------------------------
library(shiny)
library(bs4Dash)
library(shinyjs)
library(shinyWidgets)
library(eeptools)
library(jsonlite)
library(httr)
library(r2d3)
library(tidyverse)

# Set global variables --------------------------
API_PATH <- "http://68.183.201.252/api/v1"

# Custom functions ------------------------------
jitter_violin <- function(n, rn) {
    max = (n / 2) - 0.5
    min = max * -1
    return(seq(max, min, length.out = n)[rn])
}

reshape_for_violin_d3 <- function(data_frame, season, metric, precision, current_player_id) {
    df <- data_frame %>%
        filter(season_name == season,
               expanded_minutes_played >= 500,
               broad_position != "GK") %>%
        mutate(x_val = round(!!as.symbol(metric), precision),
               broad_position = factor(broad_position, levels = c("FW", "MF", "DF"))) %>%
        group_by(x_val) %>%
        arrange(broad_position) %>%
        mutate(y_val = jitter_violin(n(), row_number())) %>%
        ungroup() %>%
        filter(x_val > 0) %>%
        mutate(current_player = player_id == current_player_id)

    return(df)
}

# Source data and dashboard components ----------
source("retrieve_data.R")
source("sidebar.R")
source("navbar.R")
source("controlbar.R")
source("profile_player.R")
