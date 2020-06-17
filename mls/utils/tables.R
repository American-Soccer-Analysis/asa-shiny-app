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

# Control bar -----------------------------------
tables_cb_slider <- function(header, subheader, tables_rv, max_value, label_name, variable_name) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    sliderInput(inputId = paste("tables", header, subheader, variable_name, sep = "_"),
                label = label_name,
                min = 0,
                max = max_value,
                value = tables_rv[[paste(header, subheader, sep = "_")]][[variable_name]],
                step = 1,
                ticks = FALSE,
                width = "100%")
}

tables_cb_date_filter <- function(header, subheader, tables_rv, all_seasons) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    div(
        radioGroupButtons(inputId = paste("tables", header, subheader, "date_type", sep = "_"),
                          label = "Filter Dates By",
                          choices = c("Season", "Date Range"),
                          justified = TRUE,
                          selected = tables_rv[[paste(header, subheader, sep = "_")]][["date_type"]],
                          width = "100%"),
        conditionalPanel(condition = paste0("input.", paste("tables", header, subheader, "date_type", sep = "_"), " == 'Season'"),
                         pickerInput(inputId = paste("tables", header, subheader, "season_name", sep = "_"),
                                     label = "Seasons",
                                     choices = all_seasons,
                                     selected = tables_rv[[paste(header, subheader, sep = "_")]][["season_name"]],
                                     multiple = TRUE,
                                     options = list(`selected-text-format` = "count > 3",
                                                    `actions-box` = TRUE))),
        conditionalPanel(condition = paste0("input.", paste("tables", header, subheader, "date_type", sep = "_"), " == 'Date Range'"),
                         dateRangeInput(inputId = paste("tables", header, subheader, "date_range", sep = "_"),
                                        label = "Date Range",
                                        start = tables_rv[[paste(header, subheader, sep = "_")]][["start_date"]],
                                        end = tables_rv[[paste(header, subheader, sep = "_")]][["end_date"]],
                                        min = paste0(min(all_seasons), "-01-01"),
                                        max = paste0(max(all_seasons), "-12-31"),
                                        separator = "  to  "))
    )
}

tables_cb_picker <- function(header, subheader, tables_rv, label_name, variable_name, available_values, available_names = NULL) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    if (!is.null(available_names)) {
        names(available_values) <- available_names
    }

    pickerInput(inputId = paste("tables", header, subheader, variable_name, sep = "_"),
                label = label_name,
                choices = available_values,
                selected = tables_rv[[paste(header, subheader, sep = "_")]][[variable_name]],
                multiple = TRUE,
                options = list(`selected-text-format` = "count > 3",
                               `actions-box` = TRUE))
}

tables_cb_switch <- function(header, subheader, tables_rv, label_name, variable_name) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    prettyCheckbox(inputId = paste("tables", header, subheader, variable_name, sep = "_"),
                   label = label_name,
                   value = tables_rv[[paste(header, subheader, sep = "_")]][[variable_name]],
                   icon = icon("check"))
}

tables_cb_home_away <- function(header, subheader, tables_rv) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    selected_options <- c()
    for (s in c("home_only", "away_only")) {
        if (tables_rv[[paste(header, subheader, sep = "_")]][[s]]) {
            selected_options <- c(selected_options, s)
        }
    }

    checkboxGroupButtons(inputId = paste("tables", header, subheader, "home_away", sep = "_"),
                         labels = "Filter by Venue",
                         choices = list(home_only = "Home Only",
                                        away_only = "Away Only"),
                         selected = selected_options)
}

tables_cb_refresh <- function(header, subheader) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    actionButton(paste("tables", header, subheader, "refresh", sep = "_"), "Refresh Results", width = "100%")
}


# Wrapper div -----------------------------------
tables_div <- div(
    uiOutput("tables_header"),
    uiOutput("tables_subheader"),
    uiOutput("tables_body")
)

# Header ----------------------------------------
tables_header <- function(tab) {
    bs4Box(
        div(class = "header_background",
            h2(tables_menu_items[[tab]][["name"]])),
        width = 12
    )
}

# Subheader -------------------------------------
tables_subheader <- function(tab) {
    bs4Box(
        radioGroupButtons(inputId = "tables_subheader",
                          choices = tables_menu_items[[tab]][["subheaders"]]),
        width = 12
    )
}

# Interactive tables ----------------------------
tables_rv_to_df <- function(header, subheader) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    endpoint <- paste("/mls", subheader, header, sep = "/")
    rv_key <- paste(header, subheader, sep = "_")

    parameters <- tables_rv[[rv_key]]
    parameters <- parameters[!(grepl("data_frame", names(parameters)))]

    false_parameters <- which(sapply(parameters, isFALSE))

    if (length(false_parameters) > 0) {
        parameters <- parameters[-false_parameters]
    }

    if ("date_type" %in% names(parameters)) {
        if (parameters[["date_type"]] == "Season") {
            parameters <- parameters[!(grepl("start_date", names(parameters)))]
            parameters <- parameters[!(grepl("end_date", names(parameters)))]
        } else if (parameters[["date_type"]] == "Date Range") {
            parameters <- parameters[-(names(parameters) == "season_name")]
        }
        parameters <- parameters[!(grepl("date_type", names(parameters)))]
    }

    df <- api_request(endpoint = endpoint, parameters = parameters)

    return(df)
}

tables_body <- function(header, subheader) {
    header <- gsub("^tables_", "", header)
    subheader <- tolower(subheader)

    rv_key <- paste(header, subheader, sep = "_")
    df <- tables_rv[[rv_key]][["data_frame"]]

    if (!is.data.frame(df)) {
        bs4Box(
            p("Search yielded zero results."),
            width = 12
        )
    } else {
        bs4Box(
            DT::datatable(
                df,
                extensions = "Buttons",
                options = list(pageLength = 50,
                               autoWidth = TRUE,
                               dom = 'Bfrtip',
                               buttons = list('copy',
                                              list(extend = 'csv',
                                                   filename = paste0('EPPDashboard_WebTraffic__', format(Sys.Date(), "%m_%d_%Y"))),
                                              list(extend = 'excel',
                                                   filename = paste0('EPPDashboard_WebTraffic__', format(Sys.Date(), "%m_%d_%Y")),
                                                   title = "Test",
                                                   messageTop = paste0('Exported ', format(Sys.time(), "%H:%M %p"), ' on ', format(Sys.Date(), "%B %d, %Y"), '.')),
                                              list(extend = 'pdf',
                                                   filename = paste0('EPPDashboard_WebTraffic__', format(Sys.Date(), "%m_%d_%Y")),
                                                   title = "Test",
                                                   messageTop = paste0('Exported ', format(Sys.time(), "%H:%M %p"), ' on ', format(Sys.Date(), "%B %d, %Y"), '.')),
                                              list(extend = 'print',
                                                   title = "Test",
                                                   messageTop = paste0('Exported ', format(Sys.time(), "%H:%M %p"), ' on ', format(Sys.Date(), "%B %d, %Y"), '.')))),
                rownames = FALSE,
                style = "bootstrap",
                autoHideNavigation = TRUE,
                escape = FALSE,
                selection = "none"
            ),
            width = 12
        )
    }
}
