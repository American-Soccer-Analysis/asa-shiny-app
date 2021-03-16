# Wrapper div -----------------------------------
players_ui <- div(
    uiOutput("profiles_players_header"),
    uiOutput("profiles_players_violins") %>% withSpinner(color = "#27aae1")
    # uiOutput("profiles_players_touches") %>% withSpinner(color = "#27aae1")
)


# Profile header --------------------------------
profiles_players_header <- function(page, players_rv, all_players) {
    league <- get_values_from_page(page)$league

    if (is.null(players_rv[[league]])) {
        return(div())
    }

    selected_player_id <- players_rv[[league]][["profiles_players_name"]]
    player_dict <- (all_players[[league]] %>% filter(player_id == selected_player_id))[1,]
    player_positions <- ifelse(is.na(player_dict$secondary_general_position),
                               player_dict$primary_general_position,
                               paste(player_dict$primary_general_position, player_dict$secondary_general_position, sep = ", "))

    bs4Box(
        div(class = "header_background",
            div(class = "profiles_players_headshot",
                img(src = paste0("player_headshots/", selected_player_id, ".png"))),
            div(class = "profiles_players_demographics",
                h3(player_dict$player_name),
                p(HTML(paste0(player_positions,
                              ifelse(is.na(player_dict$birth_date), "", paste0(" &nbsp;|&nbsp; Age: ", floor(age_calc(player_dict$birth_date, units = "years")))),
                              ifelse(is.na(player_dict$height_ft), "", paste0(" &nbsp;|&nbsp; Height: ", player_dict$height_ft, "'", player_dict$height_in, "\"")),
                              ifelse(is.na(player_dict$weight_lb), "", paste0(" &nbsp;|&nbsp; Weight: ", player_dict$weight_lb, " lbs"))))),
                p(player_dict$home_town, player_dict$birth_place))
        ),
        width = 12
    )
}


# Control bar -----------------------------------
profiles_players_controlbar <- function(page, players_rv, players_dropdown) {
    league <- get_values_from_page(page)$league

    div(
        column(12,
               h4("Player Settings"),
               selectizeInput(inputId = "profiles_players_name",
                              label = "Player",
                              choices = setNames(players_dropdown[[league]][["value"]], players_dropdown[[league]][["label"]]),
                              selected = players_rv[[league]][["profiles_players_name"]],
                              width = "100%",
                              options = list(placeholder = 'Start typing a player\'s name...',
                                             maxOptions = 25,
                                             render = I("{
                                                           option: function(item, escape) {
                                                               return '<div><div class = \"players_dropdown_wrapper\"><img class=\"players_dropdown_img\"' +
                                                               'src=\"player_headshots/' + item.value + '.png\" /></div><div class = \"players_dropdown_txt\">' +
                                                               item.label + '</div></div>';
                                                           }
                                                         }"
                                             ))),
               actionButton("profiles_players_refresh", "Refresh", width = "100%"))
    )
}


# D3 p96 violin plots ---------------------------
jitter_violin <- function(n, rn) {
    max = (n / 2) - 0.5
    min = max * -1
    return(seq(max, min, length.out = n)[rn])
}

violin_d3 <- function(data_frame, metric, is_percentage, tooltip_precision,
                      x_axis_title, x_axis_suffix, x_axis_absolute, annotation_suffix, player, league) {

    if (is_percentage) {
        data_frame[[metric]] <- data_frame[[metric]] * 100
    }

    precision <- (max(data_frame[[metric]], na.rm = TRUE) - min(data_frame[[metric]], na.rm = TRUE)) / 80

    df <- data_frame %>%
        mutate(x_val = round(!!as.symbol(metric) / precision) * precision,
               x_tooltip = round(!!as.symbol(metric) / tooltip_precision) * tooltip_precision,
               broad_position = factor(broad_position, levels = c("FW", "MF", "DF"))) %>%
        group_by(x_val) %>%
        arrange(broad_position, !!as.symbol(metric)) %>%
        mutate(y_val = jitter_violin(n(), row_number())) %>%
        ungroup()

    if (x_axis_absolute) {
        df <- df %>%
            filter(x_val > 0)
    }

    r2d3(data = df,
         width = VIOLIN_WIDTH,
         height = VIOLIN_HEIGHT,
         script = "www/d3/profiles_players_violins.js",
         css = "www/d3/profiles_players_violins.css",
         options = list(x_axis_title = x_axis_title,
                        x_axis_suffix = x_axis_suffix,
                        x_axis_absolute = x_axis_absolute,
                        annotation_suffix = annotation_suffix))
}

violin_plots_profiles_players <- function(page, players_rv) {
    league <- get_values_from_page(page)$league
    player_id <- players_rv[[league]][["profiles_players_name"]]

    if (is.null(players_rv[[league]][[player_id]])) {
        players_rv[[league]][[player_id]] <- list()

        tmp_stages <- if (league == "mls") c("Regular Season", "MLS is Back Group Stage") else c("Regular Season", "NWSL Challenge Cup Group Stage")

        tmp_date_range_df <- api_request(endpoint = assemble_endpoint(league, "stats", "players"),
                                         parameters = list(player_id = player_id,
                                                           minutes_only = TRUE,
                                                           stage_name = tmp_stages))

        tmp_player_dates_df <- api_request(endpoint = assemble_endpoint(league, "stats", "players"),
                                           parameters = list(player_id = player_id,
                                                             dates_only = TRUE))

        if (any(tmp_date_range_df$player_minutes >= VIOLIN_MINIMUM_MINUTES)) {
            players_rv[[league]][[player_id]][["date_type"]] <- "Season"

            tmp_season <- (tmp_date_range_df %>%
                               filter(player_minutes >= VIOLIN_MINIMUM_MINUTES) %>%
                               summarize(max(season_name)))[[1]]

            players_rv[[league]][[player_id]][["season_name"]] <- tmp_season
            players_rv[[league]][[player_id]][["minimum_minutes"]] <- round(min(tmp_date_range_df$player_minutes[tmp_date_range_df$season_name == tmp_season],
                                                                                tmp_date_range_df$league_median_minutes[tmp_date_range_df$season_name == tmp_season]))
        } else {
            players_rv[[league]][[player_id]][["date_type"]] <- "Date Range"
            players_rv[[league]][[player_id]][["season_name"]] <- max(tmp_date_range_df$season_name)
            players_rv[[league]][[player_id]][["minimum_minutes"]] <- sum(tmp_date_range_df$player_minutes)
        }

        players_rv[[league]][[player_id]][["start_date"]] <- tmp_player_dates_df$min_date[1]
        players_rv[[league]][[player_id]][["end_date"]] <- tmp_player_dates_df$max_date[1]
        players_rv[[league]][[player_id]][["stage_name"]] <- tmp_stages

    }

    if (is.null(players_rv[[league]][[player_id]][["data_frame"]])) {
        players_rv[[league]][[player_id]][["data_frame"]] <- players_rv_to_violins_df(page, players_rv)
    }

    df <- players_rv[[league]][[player_id]][["data_frame"]]

    if (!is.data.frame(df)) {
        bs4Box(
            p("Search yielded zero results."),
            width = 12
        )
    } else {
        bs4Box(
            h3("p96 Performance Metrics", class = "card_header"),
            bs4TabSetPanel(
                id = "profiles_players_violin_tabs",
                side = "left",
                bs4TabPanel(
                    tabName = "xGoals",
                    active = TRUE,
                    violin_d3(data_frame = df,
                              metric = "xg_p96",
                              is_percentage = FALSE,
                              tooltip_precision = 0.01,
                              x_axis_title = "xGoals per 96 Minutes Played",
                              x_axis_suffix = "",
                              x_axis_absolute = TRUE,
                              annotation_suffix = " xG",
                              player = player_id,
                              league)
                ),
                bs4TabPanel(
                    tabName = "xAssists",
                    active = FALSE,
                    violin_d3(data_frame = df,
                              metric = "xa_p96",
                              is_percentage = FALSE,
                              tooltip_precision = 0.01,
                              x_axis_title = "xAssists per 96 Minutes Played",
                              x_axis_suffix = "",
                              x_axis_absolute = TRUE,
                              annotation_suffix = " xA",
                              player = player_id,
                              league)
                ),
                bs4TabPanel(
                    tabName = "Involvement",
                    active = FALSE,
                    violin_d3(data_frame = df,
                              metric = "involvement",
                              is_percentage = TRUE,
                              tooltip_precision = 0.1,
                              x_axis_title = "Share of Team's Touches (%)",
                              x_axis_suffix = "%",
                              x_axis_absolute = FALSE,
                              annotation_suffix = "%",
                              player = player_id,
                              league)
                ),
                bs4TabPanel(
                    tabName = "Pass Quality",
                    active = FALSE,
                    violin_d3(data_frame = df,
                              metric = "pass_quality",
                              is_percentage = FALSE,
                              tooltip_precision = 0.1,
                              x_axis_title = "Passes Completed Over/Under Expected (per 100 Passes)",
                              x_axis_suffix = "",
                              x_axis_absolute = FALSE,
                              annotation_suffix = " passes",
                              player = player_id,
                              league)
                ),
                bs4TabPanel(
                    tabName = "Progressive Passes",
                    active = FALSE,
                    violin_d3(data_frame = df,
                              metric = "progressive_passes_p96",
                              is_percentage = FALSE,
                              tooltip_precision = 0.1,
                              x_axis_title = "Progressive Passes per 96 Minutes Played",
                              x_axis_suffix = "",
                              x_axis_absolute = TRUE,
                              annotation_suffix = " passes",
                              player = player_id,
                              league)
                ),
                bs4TabPanel(
                    tabName = "Dribbling",
                    active = FALSE,
                    violin_d3(data_frame = df,
                              metric = "successful_dribbles_p96",
                              is_percentage = FALSE,
                              tooltip_precision = 0.1,
                              x_axis_title = "Successful Dribbles per 96 Minutes Played",
                              x_axis_suffix = "",
                              x_axis_absolute = TRUE,
                              annotation_suffix = " dribbles",
                              player = player_id,
                              league)
                ),
                bs4TabPanel(
                    tabName = "Ball Security",
                    active = FALSE,
                    violin_d3(data_frame = df,
                              metric = "ball_security",
                              is_percentage = FALSE,
                              tooltip_precision = 0.1,
                              x_axis_title = "Completed Passes per Turnover",
                              x_axis_suffix = "",
                              x_axis_absolute = FALSE,
                              annotation_suffix = " passes",
                              player = player_id,
                              league)
                ),
                bs4TabPanel(
                    tabName = "Defensive Actions",
                    active = FALSE,
                    violin_d3(data_frame = df,
                              metric = "defensive_actions_p96",
                              is_percentage = FALSE,
                              tooltip_precision = 0.1,
                              x_axis_title = "Defensive Actions per 96 Minutes Played",
                              x_axis_suffix = "",
                              x_axis_absolute = TRUE,
                              annotation_suffix = " actions",
                              player = player_id,
                              league)
                ),
                bs4TabPanel(
                    tabName = "Tackle Success",
                    active = FALSE,
                    violin_d3(data_frame = df,
                              metric = "tackle_success",
                              is_percentage = TRUE,
                              tooltip_precision = 0.1,
                              x_axis_title = "Tackle Win Rate (%)",
                              x_axis_suffix = "%",
                              x_axis_absolute = FALSE,
                              annotation_suffix = "%",
                              player = player_id,
                              league)
                ),
                bs4TabPanel(
                    tabName = "Recoveries",
                    active = FALSE,
                    violin_d3(data_frame = df,
                              metric = "recoveries_p96",
                              is_percentage = FALSE,
                              tooltip_precision = 0.1,
                              x_axis_title = "Loose Balls Recovered per 96 Minutes Played",
                              x_axis_suffix = "",
                              x_axis_absolute = FALSE,
                              annotation_suffix = " recoveries",
                              player = player_id,
                              league)
                )
            ),
            width = 12
        )
    }
}

# # D3 touch density maps -------------------------
# touch_heatmap_ggplot_profiles_players <- function(players_reactive_values, field_length = FIELD_LENGTH, field_width = FIELD_WIDTH, padding = 5) {
#     touches <- api_request(endpoint = paste0("/mls/players/", players_reactive_values$profiles_players_name, "/touches?season_name=", players_reactive_values$profiles_players_season)) %>%
#         unnest(touches) %>%
#         unnest(coordinates) %>%
#         mutate(x = x / 100 * FIELD_LENGTH,
#                y = y / 100 * FIELD_WIDTH)
#
#     ggplot(touches, aes(x, y)) +
#         theme_minimal() +
#         stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
#         scale_fill_gradient(low = "#02ACE6", high = "#CF0327") +
#         stat_density_2d(color = "lightgrey", lwd = 0.2) +
#         annotate(geom = "rect", xmin = 0 - padding, xmax = field_length + padding, ymin = 0 - padding, ymax = 0, fill = "#02ACE6") +
#         annotate(geom = "rect", xmin = 0 - padding, xmax = field_length + padding, ymin = field_width, ymax = field_width + padding, fill = "#02ACE6") +
#         annotate(geom = "rect", xmin = 0 - padding, xmax = 0, ymin = 0 - padding, ymax = field_width + padding, fill = "#02ACE6") +
#         annotate(geom = "rect", xmin = field_length, xmax = field_length + padding, ymin = 0 - padding, ymax = field_width + padding, fill = "#02ACE6") +
#         annotate_field(color = "white", fill = NA, x_scale = field_length / 100, y_scale = field_width / 100, limits = FALSE) +
#         scale_x_continuous(limits = c(0 - padding, field_length + padding),
#                            expand = c(0, 0)) +
#         scale_y_continuous(limits = c(0 - padding, field_width + padding),
#                            expand = c(0, 0)) +
#         theme(line = element_blank(),
#               axis.title = element_blank(),
#               axis.text = element_blank(),
#               legend.position = "none",
#               plot.margin = margin(0, 0, 0, 0))
# }
#
# touch_heatmap_profiles_players <- function() {
#     bs4Box(
#         h3("Touch Heatmap", class = "card_header"),
#         plotOutput("profiles_players_touch_heatmap_ggplot"),
#         width = 6
#     )
# }
#

