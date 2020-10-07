# Logical indicator for info div ----------------
filtering_hint_ind <- reactiveVal(TRUE)

# Default reactive values -----------------------
tables_rv <- reactiveValues()

for (l in league_schemas) {
    stage_name_rv <- if (l == "mls") c("Regular Season", "MLS is Back Group Stage") else c("Regular Season", "NWSL Challenge Cup Group Stage")

    # xGoals ----------------------------------------
    tables_rv[[paste0(l, "/xgoals/players")]] <- list(
        minimum_minutes = 0,
        minimum_shots = 0,
        minimum_key_passes = 0,
        date_type = "Season",
        season_name = max(all_seasons[[l]]),
        start_date = paste0(max(all_seasons[[l]]), "-01-01"),
        end_date = paste0(max(all_seasons[[l]]), "-12-31"),
        general_position = general_positions[[l]],
        team_id = all_teams[[l]]$team_id,
        shot_pattern = PATTERNS_OF_PLAY,
        split_by_teams = FALSE,
        split_by_seasons = TRUE,
        stage_name = stage_name_rv,
        normalize_by = "None",
        sort_vector = list(list("xG+xA", "desc")),
        fixed_columns = list(leftColumns = 1)
    )

    tables_rv[[paste0(l, "/xgoals/teams")]] <- list(
        date_type = "Season",
        season_name = max(all_seasons[[l]]),
        start_date = paste0(max(all_seasons[[l]]), "-01-01"),
        end_date = paste0(max(all_seasons[[l]]), "-12-31"),
        shot_pattern = PATTERNS_OF_PLAY,
        split_by_seasons = TRUE,
        home_only = FALSE,
        away_only = FALSE,
        home_adjusted = FALSE,
        even_game_state = FALSE,
        stage_name = stage_name_rv,
        normalize_by = "None",
        sort_vector = list(list("Pts", "desc"),
                           list("GD", "desc")),
        fixed_columns = list(leftColumns = 1)
    )

    tables_rv[[paste0(l, "/xgoals/games")]] <- list(
        date_type = "Season",
        season_name = max(all_seasons[[l]]),
        start_date = paste0(max(all_seasons[[l]]), "-01-01"),
        end_date = paste0(max(all_seasons[[l]]), "-12-31"),
        stage_name = stage_name_rv,
        sort_vector = list(list("Date", "desc"),
                           list("Time", "desc")),
        fixed_columns = FALSE
    )

    tables_rv[[paste0(l, "/xgoals/goalkeepers")]] <- list(
        minimum_minutes = 0,
        minimum_shots_faced = 0,
        date_type = "Season",
        season_name = max(all_seasons[[l]]),
        start_date = paste0(max(all_seasons[[l]]), "-01-01"),
        end_date = paste0(max(all_seasons[[l]]), "-12-31"),
        team_id = all_teams[[l]]$team_id,
        shot_pattern = PATTERNS_OF_PLAY,
        split_by_teams = FALSE,
        split_by_seasons = TRUE,
        stage_name = stage_name_rv,
        normalize_by = "None",
        sort_vector = list(list("G-xG", "asc")),
        fixed_columns = list(leftColumns = 1)
    )


    # xPass ----------------------------------------
    tables_rv[[paste0(l, "/xpass/players")]] <- list(
        minimum_minutes = 0,
        minimum_passes = 0,
        date_type = "Season",
        season_name = max(all_seasons[[l]]),
        start_date = paste0(max(all_seasons[[l]]), "-01-01"),
        end_date = paste0(max(all_seasons[[l]]), "-12-31"),
        general_position = general_positions[[l]],
        team_id = all_teams[[l]]$team_id,
        pass_origin_third = THIRDS_OF_FIELD,
        split_by_teams = FALSE,
        split_by_seasons = TRUE,
        stage_name = stage_name_rv,
        normalize_by = "None",
        sort_vector = list(list("Score", "desc")),
        fixed_columns = list(leftColumns = 1)
    )

    tables_rv[[paste0(l, "/xpass/teams")]] <- list(
        date_type = "Season",
        season_name = max(all_seasons[[l]]),
        start_date = paste0(max(all_seasons[[l]]), "-01-01"),
        end_date = paste0(max(all_seasons[[l]]), "-12-31"),
        pass_origin_third = THIRDS_OF_FIELD,
        split_by_seasons = TRUE,
        home_only = FALSE,
        away_only = FALSE,
        stage_name = stage_name_rv,
        normalize_by = "None",
        sort_vector = list(list("ScoreDiff", "desc")),
        fixed_columns = list(leftColumns = 1)
    )


    # Goals Added (g+) -----------------------------
    tables_rv[[paste0(l, "/goals_added/players")]] <- list(
        minimum_minutes = 0,
        date_type = "Season",
        season_name = max(all_seasons[[l]]),
        start_date = paste0(max(all_seasons[[l]]), "-01-01"),
        end_date = paste0(max(all_seasons[[l]]), "-12-31"),
        general_position = general_positions[[l]][general_positions[[l]] != "GK"],
        team_id = all_teams[[l]]$team_id,
        split_by_teams = FALSE,
        split_by_seasons = TRUE,
        stage_name = stage_name_rv,
        goals_added_variation = "Above Average",
        normalize_by = "None",
        sort_vector = list(list("Goals Added", "desc")),
        fixed_columns = list(leftColumns = 1)
    )

    tables_rv[[paste0(l, "/goals_added/teams")]] <- list(
        season_name = max(all_seasons[[l]]),
        zone = FIELD_ZONES,
        gamestate_trunc = TRUNCATED_GAMESTATES,
        split_by_seasons = TRUE,
        stage_name = stage_name_rv,
        normalize_by = "None",
        sort_vector = list(list("Goals Added diff", "desc")),
        fixed_columns = list(leftColumns = 1)
    )


    # Salaries -------------------------------------
    if (l == "mls") {
        tables_rv[[paste0(l, "/salaries/players")]] <- list(
            date_type = "Date Range",
            season_name = max(salaries_seasons[[l]]),
            start_date = salaries_most_recent[[l]],
            end_date = salaries_most_recent[[l]],
            team_id = all_teams[[l]]$team_id,
            position = MLSPA_POSITIONS,
            sort_vector = list(list("Guaranteed Compensation", "desc")),
            fixed_columns = list(leftColumns = 1)
        )

        tables_rv[[paste0(l, "/salaries/teams")]] <- list(
            season_name = max(salaries_seasons[[l]]),
            split_by_teams = TRUE,
            split_by_seasons = FALSE,
            split_by_positions = FALSE,
            sort_vector = list(list("TotalGuar", "desc")),
            fixed_columns = list(leftColumns = 1)
        )
    }
}
