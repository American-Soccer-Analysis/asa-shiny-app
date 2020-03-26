# Import player demographic data ----------------
all_players_tmp <- fromJSON(content(GET(paste0(API_PATH, "/mls/players")), as = "text", encoding = "UTF-8"))

all_players <- all_players_tmp %>%
    select(-season_name) %>%
    mutate(birth_date = as.Date(birth_date)) %>%
    arrange(player_name)


# Reshape for dropdown menu ---------------------
players_dropdown <- all_players %>%
     mutate(url = paste0("player_headshots/", player_id, ".png")) %>%
     select(value = player_id,
            label = player_name,
            url)

all_players_seasons <- all_players_tmp %>%
    select(player_id, season_name) %>%
    unnest(season_name)


# Import game data ------------------------------
all_games <- fromJSON(content(GET(paste0(API_PATH, "/mls/games")), as = "text", encoding = "UTF-8"))


# Import player stats by season -----------------
all_players_stats <- fromJSON(content(GET(paste0(API_PATH, "/mls/players/stats")), as = "text", encoding = "UTF-8")) %>%
    left_join(all_players %>% select(player_id, player_name, broad_position), "player_id")
