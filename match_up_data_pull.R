library(hoopR)


library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(ggimage)
library(ggtext)
library(extrafont)
library(readr)


season <- year_to_season(most_recent_nba_season() - 1)

pull_matchup_box <- function(game_id){
  
  print(game_id)
  pull_df <- rbind(nba_boxscorematchupsv3(game_id = game_id, season = season)[[1]], nba_boxscorematchupsv3(game_id = game_id)[[2]]) 
  
  pull_df <- tryCatch(
    expr = {
      rbind(nba_boxscorematchupsv3(game_id = game_id, season = season)[[1]], nba_boxscorematchupsv3(game_id = game_id)[[2]])
    },
    error = function(e) {
      print("Error occurred while retrieving data from the API.")
      return(NULL)  # Return NULL if an error occurs
    }
  )
  
  if (is.null(pull_df)) {
    return(NULL)  # Return NULL if pull_df is NULL (i.e., an error occurred)
  }
  
  Sys.sleep(3)
  
  df <- pull_df %>%
    rename(OFF_PLAYER_ID = person_id, DEF_PLAYER_ID = matchups_person_id) %>%
    mutate(OFF_PLAYER_NAME = paste0(first_name, " " , family_name),
           DEF_PLAYER_NAME = paste0(matchups_first_name, " " , matchups_family_name),
           MATCHUP_MIN = matchup_minutes_sort/60,
           off_team_id = team_id,
           def_team_id = ifelse(team_id == home_team_id, away_team_id, home_team_id)) %>%
  select(game_id, off_team_id, OFF_PLAYER_ID, OFF_PLAYER_NAME, 
           def_team_id, DEF_PLAYER_ID, DEF_PLAYER_NAME, 
           MATCHUP_MIN, percentage_total_time_both_on)
  
  return(df)

}

#Get List of Playoff Games
gamelog <-  nba_leaguegamelog(league_id = '00', season = season,
                              season_type = 'Playoffs')[[1]] %>%
  filter(!is.na(WL)) %>%
  select(GAME_ID, GAME_DATE)

#Get Matchup Dataframe
raw_matchup_df <- do.call(rbind, lapply(unique(gamelog$GAME_ID), pull_matchup_box))


#Join in helper columns

team_colors <- read.csv('Data/teamColors.csv') %>%
  select(TEAM_ID, TEAM_NAME, TEAM_ABBREVIATION, Primary.Color)


matchup_df <- raw_matchup_df %>% 
  left_join(team_colors, by = c('off_team_id' = 'TEAM_ID')) %>%
  left_join(team_colors, by = c('def_team_id' = 'TEAM_ID')) %>%
  rename(off_team_abv = TEAM_ABBREVIATION.x, off_team_name = TEAM_NAME.x, off_team_color = Primary.Color.x,
         def_team_abv = TEAM_ABBREVIATION.y, def_team_name = TEAM_NAME.y, def_team_color = Primary.Color.y) %>%
  mutate(OFF_PLAYER_NAME = str_replace_all(OFF_PLAYER_NAME, "[^[:alnum:][:space:]]", ""),
         DEF_PLAYER_NAME = str_replace_all(DEF_PLAYER_NAME, "[^[:alnum:][:space:]]", ""))


saveRDS(matchup_df, "matchups.rds")

   



##Redoing Gamelog

gamelog <-  nba_leaguegamelog(league_id = '00', season = season,
                              season_type = 'Playoffs')[[1]] %>% 
  mutate(MATCHUP = str_replace_all(MATCHUP, c("@" = "vs.")))  %>%
  filter(!is.na(WL)) %>%
  select(TEAM_NAME, TEAM_ID, GAME_ID, GAME_DATE, MATCHUP) %>%
  group_by(MATCHUP, TEAM_ID, TEAM_NAME) %>%
  arrange(MATCHUP,GAME_DATE) %>%
  mutate(game_number = paste0("Game ", row_number())) %>%
  ungroup() %>%
  arrange(MATCHUP) %>%
  mutate(version = format(Sys.time(), "%a %b %d %X"),
         )


unique_matchups <- gamelog %>%
  group_by(TEAM_ID, MATCHUP) %>%
  summarize(matchup_start = min(GAME_DATE)) %>%
  arrange(matchup_start) %>%
  ungroup() %>%
  group_by(TEAM_ID) %>%
  mutate(round = row_number()) %>%
  ungroup()

gamelog_enchanced <- gamelog %>%
  left_join(gamelog %>% select('GAME_ID', 'TEAM_NAME') %>% rename('OPP_NAME' = 'TEAM_NAME'),
            by = c('GAME_ID')) %>%
  filter(OPP_NAME != TEAM_NAME) %>%
  left_join(unique_matchups, by = c('TEAM_ID', 'MATCHUP')) %>%
  mutate(matchup_full = paste0("Round ", round, ": ", MATCHUP, ", ", game_number)) %>%
  left_join(team_colors %>% select(TEAM_NAME, Primary.Color) %>% rename('TEAM_COLOR' = 'Primary.Color'),
            by = c('TEAM_NAME'))  %>%
  left_join(team_colors %>% select(TEAM_NAME, Primary.Color) 
              %>% rename('OPP_NAME' = 'TEAM_NAME', 'OPP_COLOR' = 'Primary.Color'),
            by = c('OPP_NAME'))  %>%
  group_by(TEAM_NAME, TEAM_ID) %>%
  arrange(TEAM_NAME, round, game_number) %>%
  mutate(season = season)



df_def <- matchup_df %>% 
  group_by(game_id, def_team_id, DEF_PLAYER_ID) %>%
  arrange(DEF_PLAYER_ID, -MATCHUP_MIN) %>%
  mutate(min_played = sum(MATCHUP_MIN),
         matchup_min_end_def = cumsum(MATCHUP_MIN),
         matchup_min_start_def = matchup_min_end_def - MATCHUP_MIN,
         pct_matchup_time_def = MATCHUP_MIN/min_played,
         pct_matchup_time_scaled_def = scales::rescale(pct_matchup_time_def)) %>%
  select(game_id, def_team_id, OFF_PLAYER_ID, DEF_PLAYER_ID, matchup_min_start_def,
         matchup_min_end_def,  pct_matchup_time_scaled_def)




df <- matchup_df %>% 
  group_by(game_id, off_team_id, OFF_PLAYER_ID, OFF_PLAYER_NAME) %>%
  arrange(OFF_PLAYER_NAME, -MATCHUP_MIN) %>%
  mutate(min_played = sum(MATCHUP_MIN),
         matchup_min_end = cumsum(MATCHUP_MIN),
         matchup_min_start = matchup_min_end - MATCHUP_MIN,
         pct_matchup_time = MATCHUP_MIN/min_played,
         OFF_PLAYER_NAME_LAST = sub("^\\S+\\s", "", OFF_PLAYER_NAME),
         DEF_PLAYER_NAME_LAST = sub("^\\S+\\s", "", DEF_PLAYER_NAME),
         pct_matchup_time_scaled = scales::rescale(pct_matchup_time),
         DEF_PLAYER_HEADSHOT = paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", DEF_PLAYER_ID, ".png"),
         OFF_PLAYER_HEADSHOT = paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", OFF_PLAYER_ID, ".png")) %>%
  inner_join(gamelog_enchanced %>% select(GAME_ID, TEAM_ID, MATCHUP, game_number, matchup_full) %>% mutate(TEAM_ID = as.integer(TEAM_ID)),
             by = c('game_id' = 'GAME_ID', 'off_team_id' = 'TEAM_ID')) %>%
  inner_join(df_def, by = c('game_id', 'OFF_PLAYER_ID',  'def_team_id', 'DEF_PLAYER_ID')) %>%
  mutate(season = season)











write.csv(df, paste0("Data/matchups - ", season, ".csv"))
write.csv(gamelog_enchanced, paste0("Data/gamelog - ", season, ".csv"))


# List all files that start with "gamelog -" and end with ".csv"
df_csv <- list.files("Data/", pattern = "matchups -.*\\.csv$", full.names = TRUE)

# Read and combine all matching CSVs
df_combined <- df_csv %>%
  lapply(read_csv) %>%
  bind_rows()



write.csv(df_combined, paste0("Data/matchups.csv"))




# List all files that start with "gamelog -" and end with ".csv"
gamelog_csv <- list.files("Data/", pattern = "^gamelog -.*\\.csv$", full.names = TRUE)

# Read and combine all matching CSVs
gamelog_combined <- gamelog_csv %>%
  lapply(read_csv) %>%
  bind_rows()


write.csv(gamelog_combined, paste0("Data/gamelog.csv"))

