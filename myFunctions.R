cleanData <- function(train) {
  train %>% 
    mutate(ToLeft = PlayDirection == "left", 
           IsBallCarrier = NflId == NflIdRusher,
           VisitorTeamAbbr = case_when(VisitorTeamAbbr == 'ARI' ~ 'ARZ',
                                       VisitorTeamAbbr == 'BAL' ~ 'BLT',
                                       VisitorTeamAbbr == 'CLE' ~ 'CLV', 
                                       VisitorTeamAbbr == 'HOU' ~ 'HST',
                                       TRUE ~ VisitorTeamAbbr),
           HomeTeamAbbr = case_when(HomeTeamAbbr == 'ARI' ~ 'ARZ',
                                    HomeTeamAbbr == 'BAL' ~ 'BLT',
                                    HomeTeamAbbr == 'CLE' ~ 'CLV',
                                    HomeTeamAbbr == 'HOU' ~ 'HST',
                                    TRUE ~ HomeTeamAbbr),
           GameId = as.factor(GameId),
           PlayId = as.factor(PlayId),
           GameWeather = case_when(str_detect(GameWeather, fixed('rain', ignore_case=TRUE)) ~ 'Wet',
                                   str_detect(GameWeather, fixed('snow', ignore_case=TRUE)) ~ 'Wet',
                                   str_detect(GameWeather, fixed('shower', ignore_case=TRUE)) ~ 'Wet',
                                   TRUE ~ 'Dry'),
           Turf = case_when(str_detect(Turf,fixed('turf', ignore_case=TRUE)) ~ TRUE,
                            str_detect(Turf,fixed('Artifical', ignore_case=TRUE)) ~ TRUE,
                            str_detect(Turf,fixed('Artificial', ignore_case=TRUE)) ~ TRUE,
                            TRUE ~ FALSE),
           TeamOnOffense = ifelse(PossessionTeam == HomeTeamAbbr, "home", "away"),  
           IsOnOffense = Team == TeamOnOffense,  # Is player on offense?
           OffensiveScoreBeforePlay = ifelse(TeamOnOffense == 'home', HomeScoreBeforePlay, VisitorScoreBeforePlay),
           OffensiveScoreDifference = ifelse(TeamOnOffense == 'home', 
                                             HomeScoreBeforePlay-VisitorScoreBeforePlay, 
                                             VisitorScoreBeforePlay-HomeScoreBeforePlay),
           YardsFromOwnGoal = ifelse(as.character(FieldPosition) == PossessionTeam, YardLine, 50 + (50-YardLine)), 
           YardsFromOwnGoal = ifelse(YardLine == 50, 50, YardsFromOwnGoal),  
           YardsToFirstDown = Distance,
           X_std = ifelse(ToLeft, 120-X, X) - 10, # Standardizes X
           Y_std = ifelse(ToLeft, 160/3-Y, Y),
           Dir_std = case_when(ToLeft & Dir < 90 ~ Dir + 360,
                               !ToLeft & Dir > 270 ~ Dir - 360,
                               TRUE ~ Dir),
           Dir_std = case_when(ToLeft ~ Dir_std - 180,
                               TRUE ~ Dir_std), # 0= moving to player left, 90= straight ahead, 180=moving right, 270=moving backwards
           Orientation_std = case_when(ToLeft & Orientation < 90 ~ Orientation + 360,
                                       !ToLeft & Orientation > 270 ~ Orientation - 360,
                                       TRUE ~ Orientation),
           Orientation_std = case_when(ToLeft ~ Orientation_std - 180,
                                       TRUE ~ Orientation_std),
           X_std_end = S*cos((90-Dir_std)*pi/180)*(.5) + X_std + .5*A*cos((90-Dir_std)*pi/180)*(.5^2), 
           Y_std_end = S*sin((90-Dir_std)*pi/180)*(.5) + Y_std + .5*A*sin((90-Dir_std)*pi/180)*(.5^2),
           Playerft = as.integer(str_sub(PlayerHeight, 0, 1)), 
           PlayerIn = as.integer(str_sub(PlayerHeight, 3)),
           PlayerHeight = (Playerft * 12) + PlayerIn,
           PlayerBirthDate = difftime(TimeSnap, mdy(PlayerBirthDate), units = c("days")),
           PlayerBirthDate = as.numeric(round(PlayerBirthDate / 365, 2))) %>%
    mutate(Orientation_std = case_when(Orientation_std < 0 ~ Orientation_std + 360,
                                       TRUE ~ Orientation_std),
           Dir_std = case_when(Dir_std < 0 ~ Dir_std + 360,
                               TRUE ~ Dir_std),
           Within4Mins = case_when(GameClock<4 & Quarter %in% c(2,4) ~ TRUE, 
                                   TRUE ~ FALSE),
           InRedZone = YardsFromOwnGoal >= 80 | YardsFromOwnGoal<=10) %>%
    replace_na(list(Dir_std=0,Orientation_std=0)) %>%
    rename(PlayerAge = PlayerBirthDate) %>%
    select(Yards, # outcome variable to predict
           GameId, Season, Week, HomeTeamAbbr, VisitorTeamAbbr, Team, PossessionTeam, # game details
           PlayId, Quarter, GameClock, Down, YardsToFirstDown, FieldPosition, # play details
           TimeSnap, TimeHandoff, PlayDirection, Within4Mins,  # play details
           HomeScoreBeforePlay, VisitorScoreBeforePlay, # score details
           OffensiveScoreBeforePlay, OffensiveScoreDifference, # score details
           YardsFromOwnGoal, IsOnOffense, ToLeft, InRedZone, # play details
           X_std, Y_std, X_std_end, Y_std_end, # player location
           S, A, Dis, Orientation_std, Dir_std, # player direction details
           PlayerCollegeName, Position, PlayerAge, PlayerWeight, PlayerHeight, JerseyNumber, DisplayName, NflId, # player details
           NflIdRusher, IsBallCarrier, # rusher details
           DefensePersonnel, DefendersInTheBox, OffensePersonnel, OffenseFormation, # offense/defense details
           WindDirection, WindSpeed, Humidity, Temperature, GameWeather, # weather conditions
           Turf, StadiumType, Location, Stadium)# stadium details
}

calcWinsLosses <- function(train) {
  #within game winning or losing
  gameWinners <- train %>%
    group_by(GameId) %>%
    summarise(home_team_final_score = max(HomeScoreBeforePlay), 
              away_team_final_score = max(VisitorScoreBeforePlay), 
              LastPossessionTeam = last(PossessionTeam))
  
  train <- inner_join(train, gameWinners, by = "GameId") %>%
    mutate(winner = case_when((home_team_final_score > away_team_final_score) | 
                                (home_team_final_score == away_team_final_score & LastPossessionTeam == HomeTeamAbbr) ~ HomeTeamAbbr,
                              TRUE ~ VisitorTeamAbbr),
           loser = case_when((home_team_final_score < away_team_final_score) | 
                               (home_team_final_score == away_team_final_score & LastPossessionTeam == VisitorTeamAbbr) ~ HomeTeamAbbr,
                             TRUE ~ VisitorTeamAbbr))
  
  
  cumulativeWins <- train %>%
    select(GameId, winner, loser, Week, Season) %>%
    distinct() %>%
    gather(team, outcome, winner:loser) %>%
    mutate(win = case_when(team=='winner' ~ 1, 
                           TRUE ~ 0),
           game = 1) %>%
    rename(name=outcome) %>%
    select(GameId, Season, Week, name, win, game) %>%
    group_by(name, Season) %>%
    arrange(Week) %>%
    mutate(cumwins = cumsum(win),
           cumweeks = cumsum(game),
           cumwinpct = cumwins/max(cumweeks,.01)) %>%
    ungroup() %>%
    select(GameId, Week, Season, name, cumwinpct)
  inseason_wins <- cumulativeWins %>%
    mutate(cumwinpct = lag(cumwinpct, n=1L)) %>%
    replace_na(list(cumwinpct=0))
  
  train <- train %>%
    inner_join(inseason_wins %>% select(GameId, cumwinpct, name), by = c("GameId","PossessionTeam"="name"))
  
  return(train)
}

calcRunStats <- function(train) {
  # define run success, one row per play
  play_is_run_success <- train %>%
    mutate(RunSuccess = case_when(Yards > YardsToFirstDown/2 ~ TRUE,
                                  Yards >=15 ~ TRUE,
                                  Yards >=3 & YardsFromOwnGoal<10 ~ TRUE,
                                  TRUE ~ FALSE)) %>%
    select(Yards, GameId, Season, Week, PossessionTeam, Quarter, GameClock, PlayId, RunSuccess, NflIdRusher) %>%
    distinct()
  
  # summarise to game level stats, one row per game per team
  game_level_run_stats <- play_is_run_success %>%
    group_by(GameId, PossessionTeam, Season, Week) %>%
    summarise(RushesPerGame = n(),
              MeanRunSuccess = mean(RunSuccess),
              RunSuccessPerGame = sum(RunSuccess),
              MeanYardsPerAttempt = mean(Yards),
              YardsPerGame = sum(Yards)) %>%
    ungroup() 
  
  # grab last games yards, one row per game per team
  last_games_yards <- game_level_run_stats %>%
    group_by(PossessionTeam) %>%
    arrange(Season, Week) %>%
    mutate(LastGamesYards = lag(YardsPerGame, n=1L)) %>%
    ungroup() %>%
    select(Season, Week, PossessionTeam, LastGamesYards, YardsPerGame)
  
  # summarise to team level stats, one row per team
  team_level_run_stats <- game_level_run_stats %>%
    group_by(PossessionTeam) %>%
    summarise(MeanRushesPerGame = mean(RushesPerGame),
              MeanRunSuccess = mean(MeanRunSuccess),
              MeanYardsPerAttempt = mean(MeanYardsPerAttempt),
              MeanYardsPerGame = mean(YardsPerGame)) %>%
    ungroup() 
  
  # calculate lagged values, one row per play
  within_game_running_totals <- play_is_run_success %>%
    mutate(isRunPlay = 1) %>%
    group_by(GameId, PossessionTeam, Season, Week) %>%
    arrange(Quarter, -GameClock) %>%
    mutate(LastPlayYards = lag(Yards, n=1L),
           CurrentGameYards = lag(cumsum(Yards), n=1L),
           CurrentGameAttempts = lag(cumsum(isRunPlay), n=1L),
           CurrentGameSuccessRate = lag((cumsum(RunSuccess)/cumsum(isRunPlay)), n=1L)) %>%
    ungroup() %>%
    replace_na(list(CurrentGameYards = 0,
                    CurrentGameAttempts = 0)) %>%
    select(PlayId, PossessionTeam, LastPlayYards, CurrentGameYards, CurrentGameAttempts, CurrentGameSuccessRate)
  
  # merge together
  derived_running_features <- within_game_running_totals %>% 
    left_join(team_level_run_stats, by=c('PossessionTeam')) %>%
    mutate(LastPlayYards = case_when(is.na(LastPlayYards) ~ MeanYardsPerAttempt,
                                     TRUE ~ LastPlayYards),
           CurrentGameSuccessRate = case_when(is.na(CurrentGameSuccessRate) ~ MeanRunSuccess,
                                              TRUE ~ CurrentGameSuccessRate),
           ProgressIntoRunScheme = CurrentGameAttempts/MeanRushesPerGame,
           RelativeRunSuccess = (CurrentGameYards/max(CurrentGameAttempts,.01))/MeanYardsPerAttempt)
  
  train <- train %>%
    left_join(derived_running_features, by=c('PlayId','PossessionTeam'))
  return(train)
}

calcBlockerStats <- function(train) {
  # define run success, one row per play
  play_is_blocking_success <- train %>%
    filter(IsOnOffense) %>%
    mutate(IsDownLineman = case_when(Position %in% c('T','G','C','OT','OG','TE') ~ TRUE,
                                     TRUE ~ FALSE)) %>%
    filter(IsDownLineman) %>%
    select(GameId, Season, Week, PossessionTeam, PlayId, Quarter, GameClock,Yards,
           X_std, Y_std, X_std_end, Y_std_end, S, A, Dis, Orientation_std, Dir_std) %>%
    mutate(YardsDownField = X_std_end - X_std) %>%
    group_by(PlayId, Season, Week, PossessionTeam) %>%
    summarise(OlinemenPushDownField = mean(YardsDownField),
              OffensiveSD_X = sd(X_std_end),
              OffensiveSD_Y = sd(Y_std_end)) %>%
    ungroup()
  
  train <- train  %>% 
    left_join(play_is_blocking_success,  by=c('PlayId','Season','Week','PossessionTeam'))
  return(train)
}

calcGameFeatures <- function(train) {
  train %>% 
    select(GameId, Season, Week, HomeTeamAbbr, VisitorTeamAbbr, # game details
           WindDirection, WindSpeed, Humidity, Temperature, GameWeather, # weather conditions
           Turf, StadiumType, Location, Stadium) %>% # stadium details
    distinct() %>%
    select(GameId, HomeTeamAbbr, VisitorTeamAbbr, GameWeather, Turf)
}

calcPlayerFeatures <- function(train) {
  train %>% 
    mutate(Team = case_when(Team == 'away' ~ VisitorTeamAbbr,
                            Team == 'home' ~ HomeTeamAbbr)) %>%
    select(GameId, PlayId, Team,
           PlayerCollegeName, Position, PlayerAge, PlayerWeight, PlayerHeight, JerseyNumber, DisplayName, NflId) %>% # player details
    distinct()
}

calcPlayFeatures <- function(train) {
  train %>%
    mutate(TimeToHandoff = as.numeric(TimeHandoff-TimeSnap)) %>% 
    select(GameId, PlayId, Yards, PossessionTeam, cumwinpct, Quarter, GameClock, Down, YardsToFirstDown,
           TimeToHandoff, OffensiveScoreDifference, OffensiveScoreBeforePlay,
           DefendersInTheBox, OffenseFormation,
           MeanRushesPerGame, MeanRunSuccess, MeanYardsPerAttempt, MeanYardsPerGame,
           YardsFromOwnGoal,
           InRedZone, Within4Mins,
           LastPlayYards, CurrentGameYards, CurrentGameAttempts, CurrentGameSuccessRate,
           ProgressIntoRunScheme, RelativeRunSuccess, 
           OlinemenPushDownField,OffensiveSD_X, OffensiveSD_Y) %>%
    distinct() %>% 
    replace_na(list(OffenseFormation = 'Unknown'))
}

calcRunnerFeatures <- function(train) {
  train %>% 
    filter(IsBallCarrier) %>%
    select(GameId, PlayId, Team,
           Dir_std, X_std, X_std_end, Y_std, Y_std_end, Dir_std, S, A, Dis, Orientation_std, NflId, Position, DisplayName) %>%
    distinct() %>%
    mutate(runner_position = case_when(Position %in% c('RB','HB', 'FB') ~ 'RB',
                                       Position == 'QB' ~ 'QB',
                                       Position %in% c('WR','CB') ~ 'WR',
                                       TRUE ~ 'Other')) %>% 
    mutate(Position = case_when(Position %in% c('RB','HB', 'FB') ~ 'RB',
                                Position == 'QB' ~ 'QB',
                                Position %in% c('WR','CB') ~ 'WR',
                                TRUE ~ 'Other')) %>% 
    mutate(NflId = factor(NflId),
           Run_type = case_when(Dir_std < 30 & Y_std > 160/6 ~ 'Boundary Outside',
                                Dir_std < 30 & Y_std < 160/6 ~ 'Field Outside',
                                30 <= Dir_std & Dir_std < 60 & Y_std > 160/6  ~ 'Boundary Power',
                                30 <= Dir_std & Dir_std < 60 & Y_std < 160/6  ~ 'Field Power',
                                60 <= Dir_std & Dir_std < 120  ~ 'Inside',
                                120 <= Dir_std & Dir_std < 150 & Y_std > 160/6 ~ 'Field Power',
                                120 <= Dir_std & Dir_std < 150 & Y_std < 160/6 ~ 'Boundary Power',
                                Dir_std >= 150 & Y_std > 160/6 ~ 'Field Outside',
                                Dir_std >= 150 & Y_std < 160/6 ~ 'Boundary Outside')) %>%
    select(GameId, PlayId, S, A, Dis, DisplayName, Run_type) %>%
    distinct()
}

splitIntoOffense <- function(train) {
  train %>%
    filter(!IsBallCarrier) %>%
    select(GameId, PlayId, Team, 
           Dir_std, X_std, Y_std, X_std_end, Y_std_end,
           S, A, Dis, Orientation_std, NflId, Position, IsOnOffense,
           PlayerAge, PlayerWeight, PlayerHeight) %>% 
    distinct() %>% 
    filter(Position != 'QB') %>% 
    filter(IsOnOffense) %>% 
    select(-Team, -IsOnOffense, -Dis) %>% 
    rename(NflId_offense = NflId, 
           Position_offense=Position,
           Dir_std_offense = Dir_std,
           X_std_offense=X_std, 
           Y_std_offense = Y_std,
           X_std_end_offense = X_std_end,
           Y_std_end_offense = Y_std_end,
           Orientation_std_offense = Orientation_std,
           PlayerAge_offense = PlayerAge,
           PlayerWeight_offense = PlayerWeight,
           PlayerHeight_offense = PlayerHeight,
           S_offense = S,
           A_offense = A)
} 

splitIntoDefense <- function(train) {
  train %>%
    filter(!IsBallCarrier) %>%
    select(GameId, PlayId, Team, 
           Dir_std, X_std, Y_std, X_std_end, Y_std_end,
           S, A, Dis, Orientation_std, NflId, Position, IsOnOffense,
           PlayerAge, PlayerWeight, PlayerHeight) %>% 
    distinct() %>% 
    filter(Position != 'QB') %>% 
    filter(!IsOnOffense) %>% 
    select(-Team, -IsOnOffense, -Dis, -Dis) %>% 
    rename(NflId_defense = NflId, 
           Dir_std_defense = Dir_std,
           Position_defense=Position,
           X_std_defense = X_std, 
           Y_std_defense = Y_std,
           X_std_end_defense = X_std_end,
           Y_std_end_defense = Y_std_end,
           Orientation_std_defense = Orientation_std,
           PlayerAge_defense = PlayerAge,
           PlayerWeight_defense = PlayerWeight,
           PlayerHeight_defense = PlayerHeight,
           S_defense = S,
           A_defense = A)
  
}

calcRunnerDefenderDistances <- function(train) {
  DefensivePlayers <- splitIntoDefense(train)
  train %>% 
    filter(IsBallCarrier) %>%
    mutate(Position = case_when(Position %in% c('RB','HB', 'FB') ~ 'RB',
                                Position == 'QB' ~ 'QB',
                                Position %in% c('WR','CB') ~ 'WR',
                                TRUE ~ 'Other')) %>% 
    select(GameId, PlayId, Team, 
           Dir_std, X_std, Y_std, X_std_end, Y_std_end,
           S, A, Dis, Orientation_std, NflId, Position, IsOnOffense,
           PlayerAge, PlayerWeight, PlayerHeight) %>% 
    distinct() %>% 
    filter(IsOnOffense) %>% 
    select(-Team, -IsOnOffense, -Dis,) %>% 
    rename(NflId_offense = NflId, 
           Position_offense=Position,
           Dir_std_offense = Dir_std,
           X_std_offense=X_std, 
           Y_std_offense = Y_std,
           X_std_end_offense = X_std_end,
           Y_std_end_offense = Y_std_end,
           Orientation_std_offense = Orientation_std,
           PlayerAge_offense = PlayerAge,
           PlayerWeight_offense = PlayerWeight,
           PlayerHeight_offense = PlayerHeight,
           S_offense = S,
           A_offense = A) %>%
    left_join(DefensivePlayers, by=c('GameId','PlayId')) %>% 
    mutate(Position_defense = case_when(Position_defense %in% c('SS','FS','CB','DB','FB','WR','S') ~ 'Skill',
                                        Position_defense %in% c('DE','ILB','DT','OLB','NT', 
                                                                'LB','MLB','G','OT','DL','C') ~ 'Interior',
                                        TRUE ~ 'Interior')) %>%
    group_by(GameId, PlayId, NflId_offense, Position_offense) %>%
    mutate(TacklerDistance_start = sqrt((X_std_offense-X_std_defense)^2 + (Y_std_offense-Y_std_defense)^2),
           TacklerDistance_end = sqrt((X_std_end_offense-X_std_end_defense)^2 + (Y_std_end_offense-Y_std_end_defense)^2),
           TacklerDistance = (.65*TacklerDistance_start+.35*TacklerDistance_end)/2,
           # distance between players at t=1 step
           XDistance = X_std_end_offense-X_std_defense,
           YDistance = Y_std_end_offense-Y_std_defense,
           isBlocked = case_when(TacklerDistance_end>TacklerDistance_start ~ TRUE,
                                 X_std_end_defense > X_std_defense ~ TRUE,
                                 TRUE ~ FALSE)) %>%
    ungroup() 
}  

summariseRunnerDistances <- function(distances_tibb) {
  distances_tibb %>%
    group_by(PlayId) %>% 
    mutate(num_not_blocked = sum(!isBlocked)) %>%
    mutate(effectiveDownfieldSpeed_offense = sin(Orientation_std_offense*pi/180)*S_offense,
           effectiveDownfieldAcceleration_offense = sin(Dir_std_offense*pi/180)*A_offense,
           effectiveCrossfieldAcceleration_offense = abs(cos(Dir_std_offense*pi/180))*A_offense,
           xDistanceTravelled_offense = X_std_end_offense - X_std_offense,
           effectiveDownfieldSpeed_defense = (-sin(Orientation_std_defense*pi/180))*S_defense,
           effectiveDownfieldAcceleration_defense = (-sin(Dir_std_offense*pi/180))*A_defense,
           effectiveCrossfieldAcceleration_defense = abs(cos(Dir_std_offense*pi/180))*A_defense,
           xDistanceTravelled_defense = -(X_std_end_defense - X_std_defense)) %>%
    mutate(isBlocked = case_when(num_not_blocked == 0 & TacklerDistance == min(TacklerDistance) ~ FALSE, 
                                 TRUE~isBlocked)) %>%
    ungroup() %>%
    filter(!isBlocked) %>%
    group_by(GameId, PlayId, NflId_offense, Position_offense) %>%
    arrange(TacklerDistance) %>% 
    mutate(target = row_number(),
           DefendersBlocked = 11-n()) %>%
    ungroup() %>%
    mutate(YardstoTackler = X_std_defense-X_std_offense) %>%
    select(GameId, PlayId, 
           NflId_offense, NflId_defense,
           YardstoTackler, TacklerDistance, 
           target, DefendersBlocked,
           Position_offense,
           effectiveDownfieldSpeed_offense,  effectiveDownfieldAcceleration_offense,
           effectiveCrossfieldAcceleration_offense, xDistanceTravelled_offense,
           effectiveDownfieldSpeed_defense, effectiveDownfieldAcceleration_defense,
           effectiveCrossfieldAcceleration_defense, xDistanceTravelled_defense) %>%
    rename(RunnerId = NflId_offense, 
           DefenderId = NflId_defense,
           TargetTackler = target,
           Runner_type = Position_offense,
           effectiveDownfieldSpeed = effectiveDownfieldSpeed_offense,  
           effectiveDownfieldAcceleration = effectiveDownfieldAcceleration_offense, 
           effectiveCrossfieldAcceleration = effectiveCrossfieldAcceleration_offense, 
           xDistanceTravelled = xDistanceTravelled_offense) %>%
    group_by(GameId, PlayId, Runner_type, DefendersBlocked,
             effectiveDownfieldSpeed, effectiveDownfieldAcceleration, 
             effectiveCrossfieldAcceleration, xDistanceTravelled) %>%
    summarise(MinimumTacklerYardsAway = YardstoTackler[TargetTackler==1],
              MinimumTacklerDistanceAway = TacklerDistance[TargetTackler==1],
              MinimumTacklerEffectiveDownfieldSpeed = effectiveDownfieldSpeed_defense[TargetTackler==1],
              MinimumTacklerEffectiveDownfieldAcceleration = effectiveDownfieldAcceleration_defense[TargetTackler==1],
              MinimumTacklerEffectiveCrossfieldAcceleration = effectiveCrossfieldAcceleration_defense[TargetTackler==1],
              MinimumTacklerXDistanceTravelled = xDistanceTravelled_defense[TargetTackler==1],
              UnblockedDefendersYardsAway = mean(YardstoTackler),
              UnblockedDefendersDistanceAway = mean(YardstoTackler),
              NearestTacklerId = DefenderId[TargetTackler==1],
              DefensiveSD_Yards = sd(YardstoTackler),
              DefensiveSD_Dis = sd(TacklerDistance)) %>%
    ungroup() 
}

deriveFeatures <- function(train, return_long = FALSE) {
  train <- calcWinsLosses(train)
  train <- calcRunStats(train)
  train <- calcBlockerStats(train)
  
  game_features <- calcGameFeatures(train)
  player_features <- calcPlayerFeatures(train)
  play_features <- calcPlayFeatures(train)
  runner_features <- calcRunnerFeatures(train)
  
  runner_defender_distances <- calcRunnerDefenderDistances(train)
  
  if (return_long) {
    my_features <- runner_defender_distances %>% 
      select(GameId, PlayId, NflId_defense, Position_defense,
             TacklerDistance, XDistance, YDistance, isBlocked) %>%
      rename(NflId = NflId_defense) %>%
      left_join(runner_features, by=c('GameId','PlayId')) %>%
      left_join(play_features, by=c('GameId','PlayId')) %>%
      left_join(game_features, by=c("GameId")) %>%
      mutate(HomeTeamAdvantage = case_when(PossessionTeam==HomeTeamAbbr ~ TRUE,
                                           TRUE ~ FALSE)) %>%
      select(-HomeTeamAbbr, -VisitorTeamAbbr)
  } else {
    my_features <- summariseRunnerDistances(runner_defender_distances)  %>% 
      left_join(runner_features, by=c('GameId','PlayId')) %>%
      left_join(play_features, by=c('GameId','PlayId')) %>%
      left_join(game_features, by=c("GameId")) %>%
      mutate(HomeTeamAdvantage = case_when(PossessionTeam==HomeTeamAbbr ~ TRUE,
                                           TRUE ~ FALSE)) %>%
      select(-HomeTeamAbbr, -VisitorTeamAbbr)
  }
   return(my_features)
}

deriveTestSetFeatures <- function(new_obs, return_long=FALSE) {
  #curr_play <- new_obs %>% distinct(PlayId) %>% pull()
  
  #new_train2 <- calcWinsLosses(rbind(new_train, new_obs))
  #new_train2 <- calcRunStats(new_train2)
  #new_obs <- new_train %>% filter(PlayId == curr_play)
  new_obs <- calcBlockerStats(new_obs)
  
  game_features <- calcGameFeatures(new_obs)
  player_features <- calcPlayerFeatures(new_obs)
  play_features <- calcPlayFeatures(new_obs)
  runner_features <- calcRunnerFeatures(new_obs)
  
  runner_defender_distances <- calcRunnerDefenderDistances(new_obs)
  
  if (return_long) {
    my_features <- runner_defender_distances %>% 
      select(GameId, PlayId, NflId_defense, Position_defense,
             TacklerDistance, XDistance, YDistance, isBlocked) %>%
      rename(NflId = NflId_defense) %>%
      left_join(runner_features, by=c('GameId','PlayId')) %>%
      left_join(play_features, by=c('GameId','PlayId')) %>%
      left_join(game_features, by=c("GameId")) %>%
      mutate(HomeTeamAdvantage = case_when(PossessionTeam==HomeTeamAbbr ~ TRUE,
                                           TRUE ~ FALSE)) %>%
      select(-HomeTeamAbbr, -VisitorTeamAbbr)
  } else {
    my_features <- summariseRunnerDistances(runner_defender_distances)  %>% 
      left_join(runner_features, by=c('GameId','PlayId')) %>%
      left_join(play_features, by=c('GameId','PlayId')) %>%
      left_join(game_features, by=c("GameId")) %>%
      mutate(HomeTeamAdvantage = case_when(PossessionTeam==HomeTeamAbbr ~ TRUE,
                                           TRUE ~ FALSE)) %>%
      select(-HomeTeamAbbr, -VisitorTeamAbbr)
  }
  return(my_features)
}

formatTestObservation <- function(curr_play, cleanTrainingData) {
  new_cleaned <- cleanData(curr_play %>% mutate(Yards = 0)) 
  # find last-played game 
  last_game_id <- cleanTrainingData %>% 
    filter(PossessionTeam==unique(new_cleaned$PossessionTeam),
           Season==max(Season)) %>%
    filter(Week==max(Week)) %>% 
    distinct(GameId) %>%
    pull()
  # find last play team
  teams_last_play <- derived_features %>% 
    filter(GameId == last_game_id,
           PossessionTeam==unique(new_cleaned$PossessionTeam)) %>%
    filter(Quarter==max(Quarter)) %>%
    filter(GameClock == min(GameClock)) %>%
    distinct(cumwinpct, MeanRushesPerGame, MeanRunSuccess, MeanYardsPerAttempt, MeanYardsPerGame, PossessionTeam)
  # join new features as columns
  new_cleaned <- new_cleaned %>% left_join(teams_last_play, by='PossessionTeam')
}

formatPlaysForPlotting <- function(cleanData, featureData, isTestSet=FALSE) {
  if (isTestSet) {
    featureData_long <- deriveTestSetFeatures(cleanData,return_long = TRUE)
  } else {
    featureData_long <- deriveFeatures(cleanData,return_long = TRUE)
  }
  
  common_columns_long <- names(cleanData)[names(cleanData) %in% names(featureData_long)]
  common_columns_long <- common_columns_long[!(common_columns_long %in% c('PlayId','GameId','NflId'))]
  
  plotData <- cleanData %>% 
    left_join(featureData_long %>% select(-common_columns_long), by=c('PlayId','GameId','NflId')) 
  
  common_columns <- names(plotData)[names(plotData) %in% names(featureData)]
  common_columns <- common_columns[!(common_columns %in% c('PlayId','GameId'))]
  
  plotData <- plotData %>%
    left_join(featureData %>% select(-common_columns), by=c('PlayId','GameId')) 
  
  return(plotData)
}

createListforPlotting <- function( t ) {c( setNames( t[[2]], t[[1]] ) )}

plotFeaturizedPlays <- function(plotData, sample_plays, cdfData=NULL) {
  plotData <-  plotData %>% 
    filter(PlayId %in% sample_plays)
  play_types <- plotData %>% 
    filter(!is.na(Run_type)) %>%
    distinct(PlayId, Runner_type, Run_type, PossessionTeam) %>% 
    unite(Play, Runner_type,Run_type, sep=': ') %>%
    unite(Play, PossessionTeam, Play, sep=' ')
  play_types_list <- createListforPlotting(play_types)
  p <- plotData %>% 
    ggplot(aes(X_std, Y_std, fill=IsOnOffense))  + 
    geom_point(color='black', size = 4, alpha=.3, shape=21)
  if (!is.null(cdfData)) {
    scalar_pred_min <- cdfData %>% 
      filter(empirical_distribution>0) %>%
      filter(Yards == min(Yards)) %>%
      pull(Yards)
    scalar_pred_max <- cdfData %>%
      filter(empirical_distribution==max(empirical_distribution)) %>%
      filter(Yards == min(Yards)) %>%
      pull(Yards)
    p <- p + geom_rect(aes(xmin=YardsFromOwnGoal+scalar_pred_min,xmax=YardsFromOwnGoal+scalar_pred_max,ymin=-Inf,ymax=Inf), alpha=.005, fill='red')
  }
  p <- p +
    geom_segment(aes(x = X_std, y = Y_std, xend = X_std_end, yend = Y_std_end), arrow = arrow(length = unit(.5,"cm"))) + 
    geom_point(data = filter(plotData, IsBallCarrier), size = 1.5, pch = 21,fill = "blue") +
    geom_point(data = filter(plotData, isBlocked), size = 1.5, pch=21, fill='black', color='black') +
    geom_point(data = filter(plotData, !isBlocked), size = 1.5, pch=21, fill='green', color='green') +
    geom_point(data = filter(plotData, !isBlocked,NflId==NearestTacklerId), size = 1.5, pch=21, fill='red', color='red') +
    scale_fill_brewer(palette = "Set2")+ 
    geom_vline(aes(xintercept = YardsFromOwnGoal), colour = "black", lty = 2) + 
    geom_vline(aes(xintercept = YardsFromOwnGoal+Yards), colour = "red", lty = 1) + 
    geom_vline(data = filter(plotData, !is.na(OlinemenPushDownField)),
               aes(xintercept=YardsFromOwnGoal + OlinemenPushDownField), color='blue', lty=2) + 
    geom_vline(data = filter(plotData %>% group_by(PlayId) %>%  fill(UnblockedDefendersYardsAway,.direction = 'updown'), IsBallCarrier),
               aes(xintercept=X_std + UnblockedDefendersYardsAway), color='green', lty=2) + 
    scale_x_continuous(breaks = seq(0,10,by=.5)*10) + 
    labs(x = "Distance from offensive team's own end zone", 
         y = "Y coordinate",
         title = 'Sample plays and derived features',
         subtitle = "Offense moving left to right") +
    theme_bw(14) + 
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major.y =element_blank(),
          legend.position='none') + 
    facet_wrap(vars(PlayId), nrow=2, scales='free_x',labeller=as_labeller(play_types_list))+ 
    ylim(0,53.3)
  return(p)
}

constructModelMatrix <- function(model_matrix) {
  model_matrix_full <- model.matrix(~-1 + 
                                      cumwinpct + 
                                      as.factor(Quarter) + 
                                      as.factor(Down) +
                                      #as.factor(PossessionTeam) + 
                                      YardsToFirstDown + 
                                      TimeToHandoff + 
                                      OffensiveScoreBeforePlay + 
                                      OffensiveScoreDifference +
                                      DefendersInTheBox + 
                                      as.factor(OffenseFormation) + 
                                      MeanRushesPerGame +
                                      MeanRunSuccess + 
                                      YardsFromOwnGoal + 
                                      MeanYardsPerAttempt + 
                                      MeanYardsPerGame +
                                      OlinemenPushDownField + 
                                      OffensiveSD_X + 
                                      OffensiveSD_Y + 
                                      DefensiveSD_Yards + 
                                      DefensiveSD_Dis + 
                                      DefendersBlocked + 
                                      MinimumTacklerYardsAway + 
                                      MinimumTacklerDistanceAway + 
                                      UnblockedDefendersYardsAway + 
                                      UnblockedDefendersDistanceAway  +
                                      as.factor(Runner_type) +
                                      S + 
                                      A + 
                                      Dis + 
                                      as.factor(Run_type) + 
                                      as.logical(Within4Mins) + 
                                      as.logical(InRedZone) + 
                                      as.logical(HomeTeamAdvantage) + 
                                      effectiveDownfieldSpeed +
                                      effectiveDownfieldAcceleration +
                                      effectiveCrossfieldAcceleration + 
                                      xDistanceTravelled +
                                      MinimumTacklerEffectiveDownfieldSpeed + 
                                      MinimumTacklerEffectiveDownfieldAcceleration +
                                      MinimumTacklerEffectiveCrossfieldAcceleration + 
                                      MinimumTacklerXDistanceTravelled,
                                    data=model_matrix)
  return(model_matrix_full)
}

transform2CDF <- function(prediction,empirical_distributions, yardsfromowngoal=50, id=paste(round(runif(1),4)*1e4), binned=TRUE, transformed=FALSE, point=FALSE) { 
  if(point) {
    preds <- tibble(Yards = seq(-99,99,by=1)) %>%
      mutate(Class=case_when(prediction>yardsfromowngoal ~ (100-yardsfromowngoal),
                             TRUE ~prediction)) %>%
      mutate(empirical_distribution = case_when(Yards<Class  ~ 0,
                                                Yards>=Class ~ 1)) 
      
  } else {
    if (binned) {
      bins <- empirical_distributions %>% 
        distinct(Class, Yards) %>% 
        mutate(Class2 = as.numeric(as.character(Class))) %>%
        arrange(Class2)
      #if  (prediction=='10') {browser()}
      preds <- tibble(Yards = seq(-99,99,by=1)) %>%
        mutate(Class=prediction) %>%
        left_join(empirical_distributions, by=c('Class','Yards')) %>%
        replace_na(list(empirical_distribution = 0)) %>% 
        mutate(empirical_distribution = case_when(Yards < -yardsfromowngoal ~ 0,
                                                  Yards > (100-yardsfromowngoal) ~ 0,
                                                   TRUE ~ empirical_distribution))
      if (sum(preds$empirical_distribution)==0) {
        new_pred <- bins %>% 
          mutate(diff = (100-yardsfromowngoal)-Yards) %>% 
          filter(diff == min(abs(diff))) %>%
          pull(Class)
        preds <- tibble(Yards = seq(-99,99,by=1)) %>%
          mutate(Class=new_pred) %>%
          left_join(empirical_distributions, by=c('Class','Yards')) %>%
          replace_na(list(empirical_distribution = 0)) %>%
          mutate(empirical_distribution = case_when(Yards < -yardsfromowngoal ~ 0,
                                                    Yards > (100-yardsfromowngoal) ~ 1,
                                                    TRUE ~ empirical_distribution))
      }
    } else {
      if (!transformed) {
        prediction_in_yards <- round(prediction)
        bins <- empirical_distributions %>% 
          filter(Class == prediction_in_yards) 
        preds <- tibble(Yards = seq(-99,99,by=1)) %>%
          mutate(Class=prediction_in_yards) 
      } else {
        prediction_in_yards <- round(exp(prediction)-20)
        bins <- empirical_distributions %>% 
          mutate(test = (Class-log(prediction_in_yards+20))) %>%
          filter(abs(test)==min(abs(test))) %>%
          select(-test)
        tt <- unique(bins$Class)
        preds <- tibble(Yards = seq(-99,99,by=1)) %>%
          mutate(Class=tt) 
      }
      preds <- preds %>%
        left_join(bins, by=c('Class','Yards')) %>% 
        replace_na(list(empirical_distribution = 0)) %>%
        mutate(empirical_distribution = case_when(Yards < -yardsfromowngoal ~ 0,
                                                  Yards > (100-yardsfromowngoal) ~ 0,
                                                  TRUE ~ empirical_distribution)) %>%
        arrange(Yards)
      if (sum(preds$empirical_distribution)==0) {
        if (!transformed) {
          new_pred <- empirical_distributions %>% 
            mutate(diff = (100-yardsfromowngoal)-Class) %>% 
            filter(abs(diff) == min(abs(diff))) %>%
            distinct(Class) %>%
            pull(Class)
          bins <- empirical_distributions %>% 
            filter(Class == round(new_pred)) 
          preds <- tibble(Yards = seq(-99,99,by=1)) %>%
            mutate(Class=new_pred) %>%
            left_join(bins, by=c('Class','Yards')) %>%
            replace_na(list(empirical_distribution = 0)) %>%
            mutate(empirical_distribution = case_when(Yards < -yardsfromowngoal ~ 0,
                                                      Yards > (100-yardsfromowngoal) ~ 0,
                                                      TRUE ~ empirical_distribution))
        } else {
          new_yards <- empirical_distributions %>% 
            mutate(diff = (100-yardsfromowngoal)-Yards) %>% 
            filter(abs(diff) == min(abs(diff))) %>%
            distinct(Yards) %>%
            pull(Yards)
          bins <- empirical_distributions %>% 
            mutate(test = (Class-log(new_yards+20))) %>%
            filter(abs(test)==min(abs(test))) %>%
            select(-test)
          preds <- tibble(Yards = seq(-99,99,by=1)) %>%
            mutate(Class=unique(bins$Class)[1]) %>%
            left_join(bins, by=c('Class','Yards')) %>%
            replace_na(list(empirical_distribution = 0)) %>%
            mutate(empirical_distribution = case_when(Yards < -yardsfromowngoal ~ 0,
                                                      Yards > (100-yardsfromowngoal) ~ 0,
                                                      TRUE ~ empirical_distribution))
        }
      }
    }
  }
  preds <- preds %>%
    arrange(Class,  Yards) %>%
    mutate(empirical_distribution = empirical_distribution/sum(empirical_distribution)) %>%
    mutate(empirical_distribution = cumsum(empirical_distribution)) %>%
    mutate(PlayId = id)
  return(preds)
}

calcEmpiricalBins <- function(train_matrix, binned=TRUE, transformed=FALSE) {
  if (binned) {
    train_matrix %>% 
      mutate(Yards = as.factor(Yards)) %>%
      group_by(Class) %>% 
      mutate(bin_count = n()) %>%
      ungroup() %>% 
      group_by(Yards) %>% 
      #mutate(empirical_distribution = n()/bin_count) %>%
      mutate(empirical_distribution = n()) %>%
      ungroup() %>% 
      distinct(empirical_distribution, Class, Yards) %>%
      arrange(Class, Yards) %>% 
      #group_by(Class) %>%
      #mutate(empirical_distribution = cumsum(empirical_distribution)) %>% 
      #ungroup() %>%
      mutate(Yards = as.numeric(levels(Yards))[Yards])
  } else {
    yard_counts <- train_matrix %>% 
      #mutate(Class = case_when(Yards>8 ~ log(28), TRUE~Class)) %>%
      mutate(Yards = as.factor(Yards)) %>%
      mutate(Class = as.factor(Class)) %>%
      group_by(Yards) %>% 
      mutate(yards_count = n()) %>%
      ungroup() %>% 
      distinct(yards_count, Class, Yards) %>%
      mutate(Yards = as.numeric(levels(Yards))[Yards]) %>%
      arrange(Yards) %>%
      mutate(Class=as.character(Class))
    if (!transformed) {
      b1 <- tibble(Yards = seq(-99,99,by=1),
                   Yards_t =  Yards,
                   Class =  as.character(Yards_t))  %>%
        left_join(yard_counts, by=c('Yards','Class')) %>%
        replace_na(list(yards_count=0)) %>%
        mutate(Class = as.character(Class)) %>%
        arrange(Yards)
      b1 <- b1 %>% 
        bind_rows(b1 %>% mutate(Yards = Yards-1) %>% filter(Yards>-99)) %>%
        bind_rows(b1 %>% mutate(Yards = Yards-2) %>% filter(Yards>-98)) %>%
        bind_rows(b1 %>% mutate(Yards = Yards+1) %>% filter(Yards<99)) %>%
        bind_rows(b1 %>% mutate(Yards = Yards+2) %>% filter(Yards<98))
    } else {
      b1 <- tibble(Yards = seq(-19,99,by=1),
                   Yards_t =  log(Yards+20),
                   Class =  as.character(Yards_t)) %>%
            bind_rows(tibble(Yards = seq(-99,-20,by=1),
                             Yards_t =  0,
                             Class =  as.character(Yards_t))) %>%
        left_join(yard_counts, by=c('Yards','Class')) %>%
        replace_na(list(yards_count=0)) %>%
        mutate(Class = as.character(Class)) %>%
        arrange(Yards)
      b1 <- b1 %>% 
        bind_rows(b1 %>% mutate(Class = lead(Class, n=1L)) %>% filter(!is.na(Class))) %>%
        bind_rows(b1 %>% mutate(Class = lead(Class, n=2L)) %>% filter(!is.na(Class))) %>%
        bind_rows(b1 %>% mutate(Class = lead(Class, n=3L)) %>% filter(!is.na(Class))) %>%
        bind_rows(b1 %>% mutate(Class = lag(Class, n=1L)) %>% filter(!is.na(Class))) %>%
        bind_rows(b1 %>% mutate(Class = lag(Class, n=2L)) %>% filter(!is.na(Class))) 
    }
    b1 %>% 
      mutate(Class = as.factor(Class)) %>%
      group_by(Class) %>%
      mutate(bin_count = sum(yards_count)) %>%
      ungroup() %>%
      mutate(Class = as.numeric(levels(Class))[Class]) %>% 
      arrange(Class, Yards) %>%
      mutate(Class = as.factor(Class)) %>%
      group_by(Class) %>%
      mutate(empirical_distribution = yards_count/pmax(.01,bin_count)) %>%
      mutate(empirical_distribution = cumsum(empirical_distribution)) %>% 
      ungroup() %>%
      mutate(Class = as.numeric(levels(Class))[Class]) %>% 
      select(Class, Yards, empirical_distribution) %>% 
      distinct()
  }
}

calcCdfError <- function(predictions, summarise=TRUE) {
  predictions <- predictions %>% 
    mutate(Realized_cumulative = case_when(Yards < Realized_Yards ~ 0,
                                           Yards >= Realized_Yards ~ 1)) %>%
    mutate(loss = (empirical_distribution-Realized_cumulative)^2) %>% 
    group_by(PlayId) %>%
    mutate(play_loss = sum(loss),) %>% 
    ungroup() %>% 
    mutate(N=n_distinct(PlayId))
  if (summarise) {
    p <- predictions %>% 
      distinct(PlayId, play_loss, N) %>% 
      mutate(total_loss = sum(play_loss, na.rm=TRUE)) %>%
      distinct(N, total_loss) %>% 
      summarise(C = total_loss/(N*199))
    return(p)
  } else {
    return(predictions)
  }
}

plotCdfError <- function(predictions) {
  train_preds <- calcCdfError(predictions, summarise=FALSE) %>% 
    distinct(Realized_Yards, PlayId) %>%
    arrange(Realized_Yards) %>%
    mutate(ID = 1:n()) %>%
    right_join(predictions, by=c('Realized_Yards','PlayId')) 
  train_preds %>% 
    ggplot(aes(x=Yards, y=ID)) + 
    geom_raster(aes(fill=empirical_distribution)) + 
    #scale_fill_gradientn(colours=c("red",'white',"blue"),) +
    scale_fill_gradientn(colours=c("white","blue"),) +
    geom_tile(data = train_preds %>% distinct(Realized_Yards, ID), 
              aes(x=Realized_Yards,y=ID), color='red') + 
    xlim(c(-10,50)) + 
    theme_bw()
}

plotFeatureAssociation <- function(featureData, feature) {
  if (feature %in% c('GameId','PlayId','Yards')) {
    return(NA)
  }
  if (feature == 'DefendersBlocked') {
    p <- featureData %>%
      select(Yards, feature) %>% 
      ggplot(aes_string(x=feature,y='Yards')) + geom_point() + theme_bw() + geom_smooth()
    return(p)
  }
  typpe <- typeof(featureData %>% select(feature) %>% pull)
  if (typpe=='double') {
    p <- featureData %>%
      select(Yards, feature) %>% 
      ggplot(aes_string(x=feature,y='Yards')) + geom_point() + theme_bw() + geom_smooth()
    return(p)
  } else {
    p <-  featureData %>%
      select(Yards, feature) %>% 
      ggplot(aes_string(x=feature,y='Yards')) + geom_boxplot() + theme_bw()
    return(p)
  }
}
