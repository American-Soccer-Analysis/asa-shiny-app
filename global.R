# Import libraries ------------------------------
library(shiny)
# library(promises)
# library(future)
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
VIOLIN_MINUTES_CUTOFF <- 500
VIOLIN_HEIGHT <- "450px"
VIOLIN_WIDTH <- "96%"

# Custom functions ------------------------------
jitter_violin <- function(n, rn) {
    max = (n / 2) - 0.5
    min = max * -1
    return(seq(max, min, length.out = n)[rn])
}

violin_d3 <- function(data_frame, metric, metric_percentage, precision, tooltip_precision,
                      x_axis_title, x_axis_suffix, x_axis_absolute, annotation_suffix, player, season) {

    if (metric_percentage) {
        data_frame[[metric]] <- data_frame[[metric]] * 100
    }

    df <- data_frame %>%
        filter(season_name == season,
               expanded_minutes_played >= VIOLIN_MINUTES_CUTOFF,
               broad_position != "GK") %>%
        mutate(x_val = round(!!as.symbol(metric) / precision) * precision,
               x_tooltip = round(!!as.symbol(metric) / tooltip_precision) * tooltip_precision,
               broad_position = factor(broad_position, levels = c("FW", "MF", "DF"))) %>%
        group_by(x_val) %>%
        arrange(broad_position) %>%
        mutate(y_val = jitter_violin(n(), row_number())) %>%
        ungroup() %>%
        filter(x_val > 0) %>%
        mutate(current_player = player_id == player) %>%
        select(x_val, y_val, player_id, player_name, current_player, broad_position, x_tooltip)

    if (nrow(df) > 0) {
        if (sum(df$current_player) > 0) {
            r2d3(data = df,
                 width = VIOLIN_WIDTH,
                 height = VIOLIN_HEIGHT,
                 script = "www/d3_violin_dots.js",
                 css = "www/d3_violin_dots.css",
                 options = list(x_axis_title = x_axis_title,
                                x_axis_suffix = x_axis_suffix,
                                x_axis_absolute = x_axis_absolute,
                                annotation_suffix = annotation_suffix))
        } else {
            p("This player either did not meet the minimum threshold for playing time (500 minutes), or had a near-zero total for this season.")
        }
    } else {
        p("Not enough data yet for this season.")
    }
}

# Source data and dashboard components ----------
# source("retrieve_data.R")
source("sidebar.R")
source("navbar.R")
source("controlbar.R")
source("profile_player.R")
