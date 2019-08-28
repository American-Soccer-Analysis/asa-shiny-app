conn <- dbConnect(Postgres(),
                  user = getOption("asa_user"),
                  password = getOption("asa_password"),
                  host = getOption("asa_host"),
                  port = 25060,
                  dbname = getOption("asa_db_name"),
                  sslmode = "require")

all_players <- dbGetQuery(conn, "select * from mls.players order by player_name")

players_dropdown <- all_players %>% 
     select(value = player_id,
            label = player_name,
            url = headshot_url)

all_players_seasons <- dbGetQuery(conn, "select distinct p.player_id, g.season_name 
                                         from mls.lineups l
                                         left join mls.games g using(game_id)
                                         left join mls.players p using(player_id)
                                         where not g.stage_name = 'Major League Soccer Playoff'
                                         and p.player_id is not null
                                         and minutes_played > 0
                                         order by p.player_id, g.season_name")
