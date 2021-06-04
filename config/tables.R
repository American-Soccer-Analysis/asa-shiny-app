tables_config <- list(
    player_id = list(
        app_name = "Player"
    ),
    player_headshot = list(
        app_name = ""
    ),
    team_id = list(
        app_name = "Team"
    ),
    club_logo = list(
        app_name = ""
    ),
    general_position = list(
        app_name = "Position"
    ),
    season_name = list(
        app_name = "Season"
    ),
    minutes_played = list(
        app_name = "Minutes",
        tooltip_text = "Includes stoppage time."
    ),
    shots = list(
        app_name = "Shots",
        normalize = TRUE
    ),
    shots_on_target = list(
        app_name = "SoT",
        tooltip_text = "Shots on Target",
        normalize = TRUE
    ),
    goals = list(
        app_name = "G",
        tooltip_text = "Goals",
        normalize = TRUE
    ),
    xgoals = list(
        app_name = "xG",
        tooltip_text = "xGoals",
        normalize = TRUE
    ),
    xplace = list(
        app_name = "xPlace",
        tooltip_text = "Difference between post- and pre-shot xG models.",
        normalize = TRUE
    ),
    goals_minus_xgoals = list(
        app_name = "G-xG",
        normalize = TRUE
    ),
    key_passes = list(
        app_name = "KeyP",
        tooltip_text = "Key Passes",
        normalize = TRUE
    ),
    primary_assists = list(
        app_name = "A",
        tooltip_text = "Primary Assists",
        normalize = TRUE
    ),
    xassists = list(
        app_name = "xA",
        tooltip_text = "xAssists",
        normalize = TRUE
    ),
    primary_assists_minus_xassists = list(
        app_name = "A-xA",
        normalize = TRUE
    ),
    xgoals_plus_xassists = list(
        app_name = "xG+xA",
        normalize = TRUE
    ),
    points_added = list(
        app_name = "PA",
        tooltip_text = "Expected points added through scoring goals.",
        normalize = TRUE
    ),
    xpoints_added = list(
        app_name = "xPA",
        tooltip_text = "Expected points added through taking shots.",
        normalize = TRUE
    ),
    attempted_passes = list(
        app_name = "Passes",
        normalize = TRUE
    ),
    pass_completion_percentage = list(
        app_name = "Pass %",
        tooltip_text = "Pass Completion",
        percentage = TRUE
    ),
    xpass_completion_percentage = list(
        app_name = "xPass %",
        tooltip_text = "Expected pass completion percentage.",
        percentage = TRUE
    ),
    passes_completed_over_expected = list(
        app_name = "Score",
        tooltip_text = "Number of passes completed over/under expected.",
        normalize = TRUE
    ),
    passes_completed_over_expected_p100 = list(
        app_name = "Per100",
        tooltip_text = "Passes completed over/under expected, measured per 100 passes."
    ),
    avg_distance_yds = list(
        app_name = "Distance",
        tooltip_text = paste0("Average distance of completed passes, measured in yards. Assumes ", FIELD_LENGTH, "x", FIELD_WIDTH, " field dimensions.")
    ),
    avg_vertical_distance_yds = list(
        app_name = "Vertical",
        tooltip_text = paste0("Average vertical distance of completed passes, measured in yards. Assumes ", FIELD_LENGTH, "x", FIELD_WIDTH, " field dimensions.")
    ),
    share_team_touches = list(
        app_name = "Touch %",
        tooltip_text = "Players' share of their team's number of touches.",
        percentage = TRUE
    ),
    position = list(
        app_name = "Position"
    ),
    base_salary = list(
        app_name = "Base Salary",
        currency = TRUE
    ),
    guaranteed_compensation = list(
        app_name = "Guaranteed Compensation",
        currency = TRUE
    ),
    mlspa_release = list(
        app_name = "Date"
    ),
    shots_faced = list(
        app_name = "Shots Faced",
        normalize = TRUE
    ),
    goals_conceded = list(
        app_name = "Goals Conceded",
        normalize = TRUE
    ),
    saves = list(
        app_name = "Saves",
        normalize = TRUE
    ),
    share_headed_shots = list(
        app_name = "Header %",
        tooltip_text = "Share of shots on target faced that were headed.",
        percentage = TRUE
    ),
    xgoals_gk_faced = list(
        app_name = "xG",
        tooltip_text = "xGoals (post-shot)",
        normalize = TRUE
    ),
    goals_minus_xgoals_gk = list(
        app_name = "G-xG",
        tooltip_text = "Negative values are better.",
        normalize = TRUE
    ),
    goals_divided_by_xgoals_gk = list(
        app_name = "G/xG",
        tooltip_text = "Values closer to zero are better."
    ),
    count_games = list(
        app_name = "Games"
    ),
    shots_for = list(
        app_name = "ShtF",
        tooltip_text = "Shots For",
        normalize = TRUE
    ),
    shots_against = list(
        app_name = "ShtA",
        tooltip_text = "Shots Against",
        normalize = TRUE
    ),
    goals_for = list(
        app_name = "GF",
        tooltip_text = "Goals For (Own goals excluded.)",
        normalize = TRUE
    ),
    goals_against = list(
        app_name = "GA",
        tooltip_text = "Goals Against (Own goals excluded.)",
        normalize = TRUE
    ),
    goal_difference = list(
        app_name = "GD",
        tooltip_text = "Goal Difference (Own goals excluded.)",
        normalize = TRUE
    ),
    xgoals_for = list(
        app_name = "xGF",
        tooltip_text = "xGoals For (Own goals excluded.)",
        normalize = TRUE
    ),
    xgoals_against = list(
        app_name = "xGA",
        tooltip_text = "xGoals Against (Own goals excluded.)",
        normalize = TRUE
    ),
    xgoal_difference = list(
        app_name = "xGD",
        tooltip_text = "xGoal Difference (Own goals excluded.)",
        normalize = TRUE
    ),
    goal_difference_minus_xgoal_difference = list(
        app_name = "GD-xGD",
        normalize = TRUE
    ),
    points = list(
        app_name = "Pts",
        normalize = TRUE
    ),
    xpoints = list(
        app_name = "xPts",
        tooltip_text = "Expected points earned, given the same sample of shots over 1,000 simulations.",
        normalize = TRUE
    ),
    attempted_passes_for = list(
        app_name = "PassF",
        normalize = TRUE
    ),
    pass_completion_percentage_for = list(
        app_name = "PctF",
        tooltip_text = "Pass Completion",
        percentage = TRUE
    ),
    xpass_completion_percentage_for = list(
        app_name = "xPctF",
        tooltip_text = "Expected pass completion percentage.",
        percentage = TRUE
    ),
    passes_completed_over_expected_for = list(
        app_name = "ScoreF",
        tooltip_text = "Number of passes completed over/under expected.",
        normalize = TRUE
    ),
    passes_completed_over_expected_p100_for = list(
        app_name = "Per100F",
        tooltip_text = "Passes completed over/under expected, measured per 100 passes."
    ),
    avg_vertical_distance_for = list(
        app_name = "VertF",
        tooltip_text = paste0("Average vertical distance of completed passes, measured in yards. Assumes ", FIELD_LENGTH, "x", FIELD_WIDTH, " field dimensions.")
    ),
    attempted_passes_against = list(
        app_name = "PassA",
        normalize = TRUE
    ),
    pass_completion_percentage_against = list(
        app_name = "PctA",
        tooltip_text = "Pass Completion",
        percentage = TRUE
    ),
    xpass_completion_percentage_against = list(
        app_name = "xPctA",
        tooltip_text = "Expected pass completion percentage.",
        percentage = TRUE
    ),
    passes_completed_over_expected_against = list(
        app_name = "ScoreA",
        tooltip_text = "Number of passes completed over/under expected.",
        normalize = TRUE
    ),
    passes_completed_over_expected_p100_against = list(
        app_name = "Per100A",
        tooltip_text = "Passes completed over/under expected, measured per 100 passes."
    ),
    avg_vertical_distance_against = list(
        app_name = "VertA",
        tooltip_text = paste0("Average vertical distance of completed passes, measured in yards. Assumes ", FIELD_LENGTH, "x", FIELD_WIDTH, " field dimensions.")
    ),
    passes_completed_over_expected_difference = list(
        app_name = "ScoreDiff",
        tooltip_text = "Number of passes completed over/under expected.",
        normalize = TRUE
    ),
    avg_vertical_distance_difference = list(
        app_name = "VertDiff",
        tooltip_text = paste0("Average vertical distance of completed passes, measured in yards. Assumes ", FIELD_LENGTH, "x", FIELD_WIDTH, " field dimensions.")
    ),
    count_players = list(
        app_name = "N"
    ),
    total_guaranteed_compensation = list(
        app_name = "TotalGuar",
        tooltip_text = "Sum of Guaranteed Compensation",
        currency = TRUE
    ),
    avg_guaranteed_compensation = list(
        app_name = "AvgGuar",
        tooltip_text = "Average Guaranteed Compensation",
        currency = TRUE
    ),
    median_guaranteed_compensation = list(
        app_name = "MedGuar",
        tooltip_text = "Median Guaranteed Compensation",
        currency = TRUE
    ),
    std_dev_guaranteed_compensation = list(
        app_name = "StdDevGuar",
        tooltip_text = "Standard Deviation of Guaranteed Compensation",
        currency = TRUE
    ),
    date = list(
        app_name = "Date"
    ),
    time = list(
        app_name = "Time"
    ),
    home_team_id = list(
        app_name = "Home"
    ),
    home_goals = list(
        app_name = "HG",
        tooltip_text = "Goals (Own goals excluded.)"
    ),
    home_team_xgoals = list(
        app_name = "HxGt",
        tooltip_text = "Team xGoals (Conditional probabilities are applied to ensure the xGoals value of a single possession never exceeds 1.)"
    ),
    home_player_xgoals = list(
        app_name = "HxGp",
        tooltip_text = "Player xGoals"
    ),
    away_team_id = list(
        app_name = "Away"
    ),
    away_goals = list(
        app_name = "AG",
        tooltip_text = "Goals (Own goals excluded.)"
    ),
    away_team_xgoals = list(
        app_name = "AxGt",
        tooltip_text = "Team xGoals (Conditional probabilities are applied to ensure the xGoals value of a single possession never exceeds 1.)"
    ),
    away_player_xgoals = list(
        app_name = "AxGp",
        tooltip_text = "Player xGoals"
    ),
    team_xgoal_difference = list(
        app_name = "xGDt",
        tooltip_text = "Team xGoals (Conditional probabilities are applied to ensure the xGoals value of a single possession never exceeds 1.)"
    ),
    player_xgoal_difference = list(
        app_name = "xGDp",
        tooltip_text = "Player xGoals"
    ),
    final_score_difference = list(
        app_name = "Final",
        tooltip_text = "Final Score Difference (Own goals included.)"
    ),
    home_xpoints = list(
        app_name = "HxPts",
        tooltip_text = "Expected points earned, given the same sample of shots over 1,000 simulations."
    ),
    away_xpoints = list(
        app_name = "AxPts",
        tooltip_text = "Expected points earned, given the same sample of shots over 1,000 simulations."
    ),
    Dribbling_goals_added_raw = list(
        app_name = "Dribbling",
        normalize = TRUE
    ),
    Fouling_goals_added_raw = list(
        app_name = "Fouling",
        normalize = TRUE
    ),
    Interrupting_goals_added_raw = list(
        app_name = "Interrupting",
        normalize = TRUE
    ),
    Passing_goals_added_raw = list(
        app_name = "Passing",
        normalize = TRUE
    ),
    Receiving_goals_added_raw = list(
        app_name = "Receiving",
        normalize = TRUE
    ),
    Shooting_goals_added_raw = list(
        app_name = "Shooting",
        normalize = TRUE
    ),
    total_goals_added_raw = list(
        app_name = "Goals Added",
        normalize = TRUE
    ),
    Dribbling_goals_added_above_avg = list(
        app_name = "Dribbling",
        normalize = TRUE
    ),
    Dribbling_count_actions = list(
        app_name = "Dribbling Actions"
    ),
    Fouling_goals_added_above_avg = list(
        app_name = "Fouling",
        normalize = TRUE
    ),
    Fouling_count_actions = list(
        app_name = "Fouling Actions"
    ),
    Interrupting_goals_added_above_avg = list(
        app_name = "Interrupting",
        normalize = TRUE
    ),
    Interrupting_count_actions = list(
        app_name = "Interrupting Actions"
    ),
    Passing_goals_added_above_avg = list(
        app_name = "Passing",
        normalize = TRUE
    ),
    Passing_count_actions = list(
        app_name = "Passing Actions"
    ),
    Receiving_goals_added_above_avg = list(
        app_name = "Receiving",
        normalize = TRUE
    ),
    Receiving_count_actions = list(
        app_name = "Receiving Actions"
    ),
    Shooting_goals_added_above_avg = list(
        app_name = "Shooting",
        normalize = TRUE
    ),
    Shooting_count_actions = list(
        app_name = "Shooting Actions"
    ),


    Shotstopping_goals_added_above_avg = list(
        app_name = "Shotstopping",
        normalize = TRUE
    ),
    Shotstopping_count_actions = list(
        app_name = "Shotstopping Actions"
    ),
    Handling_goals_added_above_avg = list(
        app_name = "Handling",
        normalize = TRUE
    ),
    Handling_count_actions = list(
        app_name = "Handling Actions"
    ),
    Claiming_goals_added_above_avg = list(
        app_name = "Claiming",
        normalize = TRUE
    ),
    Claiming_count_actions = list(
        app_name = "Claiming Actions"
    ),
    Sweeping_goals_added_above_avg = list(
        app_name = "Sweeping",
        normalize = TRUE
    ),
    Sweeping_count_actions = list(
        app_name = "Sweeping Actions"
    ),
    Fielding_goals_added_above_avg = list(
        app_name = "Fielding",
        normalize = TRUE
    ),
    Fielding_count_actions = list(
        app_name = "Fielding Actions"
    ),

    Shotstopping_goals_added_raw = list(
        app_name = "Shotstopping",
        normalize = TRUE
    ),
    Handling_goals_added_raw = list(
        app_name = "Handling",
        normalize = TRUE
    ),
    Claiming_goals_added_raw = list(
        app_name = "Claiming",
        normalize = TRUE
    ),
    Sweeping_goals_added_raw = list(
        app_name = "Sweeping",
        normalize = TRUE
    ),
    Fielding_goals_added_raw = list(
        app_name = "Fielding",
        normalize = TRUE
    ),

    total_goals_added_above_avg = list(
        app_name = "Goals Added",
        tooltip_text = "Calculated each game against the position at which the player lined up.",
        normalize = TRUE
    ),
    total_count_actions = list(
        app_name = "All Actions"
    ),
    goals_added_above_replacement = list(
        app_name = "Goals Added",
        normalize = TRUE
    ),
    count_actions = list(
        app_name = "All Actions"
    ),

    minutes = list(
        app_name = "Minutes"
    ),
    goals_added_for_Dribbling = list(
        app_name = "DribblingF",
        normalize = TRUE
    ),
    num_actions_for_Dribbling = list(
        app_name = "Dribbling ActionsF"
    ),
    goals_added_for_Fouling = list(
        app_name = "FoulingF",
        normalize = TRUE
    ),
    num_actions_for_Fouling = list(
        app_name = "Fouling ActionsF"
    ),
    goals_added_for_Interrupting = list(
        app_name = "InterruptingF",
        normalize = TRUE
    ),
    num_actions_for_Interrupting = list(
        app_name = "Interrupting ActionsF"
    ),
    goals_added_for_Passing = list(
        app_name = "PassingF",
        normalize = TRUE
    ),
    num_actions_for_Passing = list(
        app_name = "Passing ActionsF"
    ),
    goals_added_for_Receiving = list(
        app_name = "ReceivingF",
        normalize = TRUE
    ),
    num_actions_for_Receiving = list(
        app_name = "Receiving ActionsF"
    ),
    goals_added_for_Shooting = list(
        app_name = "ShootingF",
        normalize = TRUE
    ),
    num_actions_for_Shooting = list(
        app_name = "Shooting ActionsF"
    ),
    total_goals_added_for = list(
        app_name = "Goals AddedF",
        tooltip_text = "Ignores interrupting g+.",
        normalize = TRUE
    ),
    total_count_actions_for = list(
        app_name = "All ActionsF"
    ),

    goals_added_against_Dribbling = list(
        app_name = "DribblingA",
        normalize = TRUE
    ),
    num_actions_against_Dribbling = list(
        app_name = "Dribbling ActionsA"
    ),
    goals_added_against_Fouling = list(
        app_name = "FoulingA",
        normalize = TRUE
    ),
    num_actions_against_Fouling = list(
        app_name = "Fouling ActionsA"
    ),
    goals_added_against_Interrupting = list(
        app_name = "InterruptingA",
        normalize = TRUE
    ),
    num_actions_against_Interrupting = list(
        app_name = "Interrupting ActionsA"
    ),
    goals_added_against_Passing = list(
        app_name = "PassingA",
        normalize = TRUE
    ),
    num_actions_against_Passing = list(
        app_name = "Passing ActionsA"
    ),
    goals_added_against_Receiving = list(
        app_name = "ReceivingA",
        normalize = TRUE
    ),
    num_actions_against_Receiving = list(
        app_name = "Receiving ActionsA"
    ),
    goals_added_against_Shooting = list(
        app_name = "ShootingA",
        normalize = TRUE
    ),
    num_actions_against_Shooting = list(
        app_name = "Shooting ActionsA"
    ),
    total_goals_added_against = list(
        app_name = "Goals AddedA",
        tooltip_text = "Ignores interrupting g+.",
        normalize = TRUE
    ),
    total_count_actions_against = list(
        app_name = "All ActionsA"
    ),
    total_goals_added_differential = list(
        app_name = "Goals Added diff",
        tooltip_text = "Ignores interrupting g+.",
        normalize = TRUE
    )
)
