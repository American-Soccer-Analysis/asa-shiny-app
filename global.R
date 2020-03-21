# Import libraries ------------------------------
library(shiny)
library(bs4Dash)
library(shinyjs)
library(shinyWidgets)
library(RPostgres)
library(eeptools)
library(jsonlite)
library(httr)
library(tidyverse)

# Set global variables --------------------------
API_PATH <- "http://68.183.201.252/api/v1"

# Source data and dashboard components ----------
source("retrieve_data.R")
source("sidebar.R")
source("navbar.R")
source("controlbar.R")
source("profile_player.R")
