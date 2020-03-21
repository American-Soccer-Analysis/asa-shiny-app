all_players_json <- content(GET(paste0(API_PATH, "/mls/players")), as = "text")
all_players_tmp <- fromJSON(all_players_json)

all_players <- all_players_tmp %>%
    select(-season_name) %>%
    mutate(birth_date = as.Date(birth_date)) %>%
    arrange(player_name)

players_dropdown <- all_players %>%
     select(value = player_id,
            label = player_name,
            url = headshot_url)

all_players_seasons <- all_players_tmp %>%
    select(player_id, season_name) %>%
    unnest(season_name)
