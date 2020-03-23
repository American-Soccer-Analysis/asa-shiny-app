# Import libraries ------------------------------
library(shiny)
library(promises)
library(future)
library(bs4Dash)
library(shinyjs)
library(shinyWidgets)
library(eeptools)
library(RPostgres)
library(DBI)
library(pool)
library(r2d3)
library(tidyverse)

# Set global variables --------------------------
pool <- dbPool(Postgres(),
               user = getOption("asa_user"),
               password = getOption("asa_password"),
               host = getOption("asa_host"),
               port = 25060,
               dbname = getOption("asa_db_name"),
               sslmode = "require")

onStop(function() {
     poolClose(pool)
})

# Custom functions ------------------------------
jitter_violin <- function(n, rn) {
    max = (n / 2) - 0.5
    min = max * -1
    return(seq(max, min, length.out = n)[rn])
}

reshape_for_violin_d3 <- function(data_frame, season, metric, precision, tooltip_precision, current_player_id) {
    df <- data_frame %>%
        filter(season_name == season,
               expanded_minutes_played >= 500,
               broad_position != "GK") %>%
        mutate(x_val = round(!!as.symbol(metric) / precision) * precision,
               x_tooltip = round(!!as.symbol(metric) / tooltip_precision) * tooltip_precision,
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
