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

# TODO: Figure out how to define dependencies in a single place that also makes renv happy


# League-specific constants ---------------------
LEAGUE_SCHEMA <- "mls"
LEAGUE_NAME <- "Major League Soccer"

# Menu items and options ------------------------
tables_menu_items <-
    list(tables_xgoals = list(name = "xGoals",
                              tabName = "tables_xgoals",
                              icon = "futbol-o",
                              subheaders = c("Players", "Teams", "Goalkeepers", "Games")),
         tables_xpass = list(name = "xPass",
                             tabName = "tables_xpass",
                             icon = "bullseye",
                             subheaders = c("Players", "Teams")),
         tables_goals_added = list(name = "Goals Added (g+)",
                                   tabName = "tables_goals_added",
                                   icon = "google-plus",
                                   subheaders = c("Players")),
         tables_salaries = list(name = "Salaries",
                                tabName = "tables_salaries",
                                icon = "usd",
                                subheaders = c("Players", "Teams")))
