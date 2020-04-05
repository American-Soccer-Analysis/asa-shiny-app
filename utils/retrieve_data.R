# Import player demographic data ----------------
all_players_tmp <- api_request(endpoint = "/mls/players")

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
all_games <- api_request(endpoint = "/mls/games")

# Import player stats by season -----------------
all_players_stats <- api_request(endpoint = "/mls/players/stats") %>%
    left_join(all_players %>% select(player_id, player_name, broad_position), "player_id")
