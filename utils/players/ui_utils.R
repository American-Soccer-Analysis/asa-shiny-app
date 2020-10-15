# Wrapper div -----------------------------------
players_ui <- div(
    uiOutput("profiles_players_header")
    # uiOutput("profiles_players_violins") %>% withSpinner(color = "#27aae1"),
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


# # D3 p96 violin plots ---------------------------
# jitter_violin <- function(n, rn) {
#     max = (n / 2) - 0.5
#     min = max * -1
#     return(seq(max, min, length.out = n)[rn])
# }
#
# violin_d3 <- function(data_frame, metric, is_percentage, precision, tooltip_precision,
#                       x_axis_title, x_axis_suffix, x_axis_absolute, annotation_suffix, player, season) {
#
#     if (is_percentage) {
#         data_frame[[metric]] <- data_frame[[metric]] * 100
#     }
#
#     df <- data_frame %>%
#         filter(season_name == season,
#                expanded_minutes_played >= VIOLIN_MINUTES_CUTOFF,
#                broad_position != "GK") %>%
#         mutate(x_val = round(!!as.symbol(metric) / precision) * precision,
#                x_tooltip = round(!!as.symbol(metric) / tooltip_precision) * tooltip_precision,
#                broad_position = factor(broad_position, levels = c("FW", "MF", "DF"))) %>%
#         group_by(x_val) %>%
#         arrange(broad_position) %>%
#         mutate(y_val = jitter_violin(n(), row_number())) %>%
#         ungroup() %>%
#         filter(x_val > 0) %>%
#         mutate(current_player = player_id == player) %>%
#         select(x_val, y_val, player_id, player_name, current_player, broad_position, x_tooltip)
#
#     if (nrow(df) > 0) {
#         if (sum(df$current_player) > 0) {
#             r2d3(data = df,
#                  width = VIOLIN_WIDTH,
#                  height = VIOLIN_HEIGHT,
#                  script = "d3/profiles_players_violins.js",
#                  css = "d3/profiles_players_violins.css",
#                  options = list(x_axis_title = x_axis_title,
#                                 x_axis_suffix = x_axis_suffix,
#                                 x_axis_absolute = x_axis_absolute,
#                                 annotation_suffix = annotation_suffix))
#         } else {
#             p("This player either did not meet the minimum threshold for playing time (500 minutes), or had a near-zero total for this season.")
#         }
#     } else {
#         p("Not enough data yet for this season.")
#     }
# }
#
# violin_plots_profiles_players <- function(all_players_stats, players_reactive_values) {
#     bs4Box(
#         h3("Comparative Performance Metrics (per 96 Minutes Played)", class = "card_header"),
#         bs4TabSetPanel(
#             id = "player_profile_violin_tabs",
#             side = "left",
#             bs4TabPanel(
#                 tabName = "xGoals",
#                 active = TRUE,
#                 violin_d3(data_frame = all_players_stats,
#                           metric = "xg_p96",
#                           metric_percentage = FALSE,
#                           precision = 0.01,
#                           tooltip_precision = 0.01,
#                           x_axis_title = "xGoals per 96 Minutes Played",
#                           x_axis_suffix = "",
#                           x_axis_absolute = TRUE,
#                           annotation_suffix = " xG",
#                           player = players_reactive_values$profiles_players_name,
#                           season = players_reactive_values$profiles_players_season)
#             ),
#             bs4TabPanel(
#                 tabName = "xAssists",
#                 active = FALSE,
#                 violin_d3(data_frame = all_players_stats,
#                           metric = "xa_p96",
#                           metric_percentage = FALSE,
#                           precision = 0.005,
#                           tooltip_precision = 0.01,
#                           x_axis_title = "xAssists per 96 Minutes Played",
#                           x_axis_suffix = "",
#                           x_axis_absolute = TRUE,
#                           annotation_suffix = " xA",
#                           player = players_reactive_values$profiles_players_name,
#                           season = players_reactive_values$profiles_players_season)
#             ),
#             bs4TabPanel(
#                 tabName = "Involvement",
#                 active = FALSE,
#                 violin_d3(data_frame = all_players_stats,
#                           metric = "involvement",
#                           metric_percentage = TRUE,
#                           precision = 0.15,
#                           tooltip_precision = 0.1,
#                           x_axis_title = "Share of Team's Touches (%)",
#                           x_axis_suffix = "%",
#                           x_axis_absolute = FALSE,
#                           annotation_suffix = "%",
#                           player = players_reactive_values$profiles_players_name,
#                           season = players_reactive_values$profiles_players_season)
#             ),
#             bs4TabPanel(
#                 tabName = "Pass Quality",
#                 active = FALSE,
#                 p("Coming soon.")
#             ),
#             bs4TabPanel(
#                 tabName = "Progressive Passes",
#                 active = FALSE,
#                 violin_d3(data_frame = all_players_stats,
#                           metric = "progressive_passes_p96",
#                           metric_percentage = FALSE,
#                           precision = 0.1,
#                           tooltip_precision = 0.01,
#                           x_axis_title = "Progressive Passes per 96 Minutes Played",
#                           x_axis_suffix = "",
#                           x_axis_absolute = TRUE,
#                           annotation_suffix = " passes",
#                           player = players_reactive_values$profiles_players_name,
#                           season = players_reactive_values$profiles_players_season)
#             ),
#             bs4TabPanel(
#                 tabName = "Dribbling",
#                 active = FALSE,
#                 violin_d3(data_frame = all_players_stats,
#                           metric = "successful_dribbles_p96",
#                           metric_percentage = FALSE,
#                           precision = 0.05,
#                           tooltip_precision = 0.01,
#                           x_axis_title = "Successful Dribbles per 96 Minutes Played",
#                           x_axis_suffix = "",
#                           x_axis_absolute = TRUE,
#                           annotation_suffix = " dribbles",
#                           player = players_reactive_values$profiles_players_name,
#                           season = players_reactive_values$profiles_players_season)
#             ),
#             bs4TabPanel(
#                 tabName = "Ball Security",
#                 active = FALSE,
#                 violin_d3(data_frame = all_players_stats,
#                           metric = "ball_security",
#                           metric_percentage = FALSE,
#                           precision = 2.5,
#                           tooltip_precision = 1,
#                           x_axis_title = "Pass Attempts per Turnover",
#                           x_axis_suffix = "",
#                           x_axis_absolute = FALSE,
#                           annotation_suffix = " attempts",
#                           player = players_reactive_values$profiles_players_name,
#                           season = players_reactive_values$profiles_players_season)
#             ),
#             bs4TabPanel(
#                 tabName = "Defensive Actions",
#                 active = FALSE,
#                 violin_d3(data_frame = all_players_stats,
#                           metric = "defensive_actions_p96",
#                           metric_percentage = FALSE,
#                           precision = 0.25,
#                           tooltip_precision = 0.1,
#                           x_axis_title = "Defensive Actions per 96 Minutes Played",
#                           x_axis_suffix = "",
#                           x_axis_absolute = TRUE,
#                           annotation_suffix = " actions",
#                           player = players_reactive_values$profiles_players_name,
#                           season = players_reactive_values$profiles_players_season)
#             ),
#             bs4TabPanel(
#                 tabName = "Tackle Success",
#                 active = FALSE,
#                 violin_d3(data_frame = all_players_stats,
#                           metric = "tackle_success",
#                           metric_percentage = TRUE,
#                           precision = 1,
#                           tooltip_precision = 0.1,
#                           x_axis_title = "Tackle Win Rate (%)",
#                           x_axis_suffix = "%",
#                           x_axis_absolute = FALSE,
#                           annotation_suffix = "%",
#                           player = players_reactive_values$profiles_players_name,
#                           season = players_reactive_values$profiles_players_season)
#             ),
#             bs4TabPanel(
#                 tabName = "Recoveries",
#                 active = FALSE,
#                 violin_d3(data_frame = all_players_stats,
#                           metric = "recoveries_p96",
#                           metric_percentage = FALSE,
#                           precision = 0.1,
#                           tooltip_precision = 0.1,
#                           x_axis_title = "Loose Balls Recovered per 96 Minutes Played",
#                           x_axis_suffix = "",
#                           x_axis_absolute = FALSE,
#                           annotation_suffix = " recoveries",
#                           player = players_reactive_values$profiles_players_name,
#                           season = players_reactive_values$profiles_players_season)
#             )
#         ),
#         width = 12
#     )
# }
#
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
#         plotOutput("player_profile_touch_heatmap_ggplot"),
#         width = 6
#     )
# }
#

