library(hoopR)


library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(png)
library(ggimage)
library(magick)
library(ggtext)
library(extrafont)
library(forcats)

library(zoo)
loadfonts(device="win")




theme_saurabh <- function () { 
  theme(
    text=element_text(family='Roboto Mono'),
    plot.title=element_text(size=18, family = 'Roboto Black'),
    plot.subtitle=element_text(size=9, family = 'Roboto Slab'),
    plot.caption=element_text(size=6, family = 'Roboto Mono'),
    axis.title =element_text(size=10, family = 'Roboto Mono'),
    panel.background = element_blank(),
    
  )
}




#Get List of Playoff Games
gamelog <-  nba_leaguegamelog(league_id = '00', season = year_to_season(most_recent_nba_season() - 1),
                              season_type = 'Playoffs')[[1]] %>%
  filter(!is.na(WL))


home_df <- gamelog %>%
  filter(grepl('vs.', MATCHUP)) %>%
  select(TEAM_ID, TEAM_ABBREVIATION, GAME_ID, GAME_DATE) %>%
  rename(home_id = TEAM_ID, home_team = TEAM_ABBREVIATION)
  

away_df <- gamelog %>%
  filter(grepl('@', MATCHUP)) %>%
  select(TEAM_ID, TEAM_ABBREVIATION, GAME_ID) %>%
  rename(away_id = TEAM_ID, away_team = TEAM_ABBREVIATION)


df <- home_df %>%
  left_join(away_df, by = 'GAME_ID') %>%
  rowwise() %>%
  mutate(team1_id = min(home_id, away_id),
         team1_name = min(home_team, away_team),
         team2_id = max(home_id, away_id),
         team2_name = max(home_team, away_team),
         matchup = paste0(team1_name, ' v ', team2_name)) %>%
  group_by(matchup, team1_id, team1_name, team2_id, team2_name) %>%
  summarise(GAME_DATE = max(GAME_DATE)) %>%
  arrange()
  

get_final_four <- function(season){
  
  #Get List of Playoff Games
  gamelog <-  nba_leaguegamelog(league_id = '00', season = year_to_season(season - 1),
                                season_type = 'Playoffs')[[1]] %>%
    filter(!is.na(WL))
  
  
  df <- gamelog %>%
    group_by(TEAM_ID, TEAM_ABBREVIATION) %>%
    summarise(series_end = max(GAME_DATE)) %>%
    arrange(desc(series_end))  %>%
    ungroup() %>%
    mutate(row = row_number()) %>%
    filter(row < 5) %>%
    mutate(finals = ifelse(row < 3, 1, 0), 
           conf_finals = 1,
           season = season)
  
  return(df)

}


seq(1947, 2023)

# Create the list of NBA seasons
seasons <- seq(1947, 2023)

# Initialize an empty data frame to store the combined results
final_4 <- data.frame()

# Loop through each season, call the "get_sportsodds" function, and combine the results
for (season in seasons) {
  
  print(season)
  # Call the "get_sportsodds" function for the current season
  temp_df <- get_final_four(season)
  
  # Combine the current data frame with the existing data
  final_4 <- rbind(final_4, temp_df)
  
  Sys.sleep(3)
}

# Create a dataframe with the new rows
new_rows <- data.frame(
  TEAM_ID = c(1610612750, 1610612742, 1610612738, 1610612754),
  TEAM_ABBREVIATION = c("MIN", "DAL", "BOS", "IND"),
  series_end = as.Date(c("2024-06-01", "2024-06-01", "2024-06-01", "2024-06-01")),
  row = c(1, 2, 3, 4),
  finals = c(0 , 0, 0, 0),
  conf_finals = c(1, 1, 1, 1),
  season = c(2024, 2024, 2024, 2024),
  stringsAsFactors = FALSE
)


##Window 5
window = 5


final_4_manual <- rbind(final_4, new_rows)

parity_f_df5 <- final_4_manual %>%
  arrange(season) %>%
  filter(finals == 1) %>%
  mutate(row = row_number()) %>%
  mutate(unique_finalists_5 = rollapply(TEAM_ID, width= window*2, 
                                        FUN=function(x) length(unique(x[!is.na(x)])), partial=TRUE, align='left'),
         season_start = season,
         season_end = season + window - 1) %>%
  filter(row %% 2 == 1,
         season_end <= 2024,
         season_start >= 1984) %>%
  select( season_end, unique_finalists_5)


parity_cf_df5 <- final_4_manual %>%
  arrange(season) %>%
  mutate(row = row_number()) %>%
  mutate(unique_c_finalists_5 = rollapply(TEAM_ID, width= window*4, 
                                        FUN=function(x) length(unique(x[!is.na(x)])), partial=TRUE, align='left'),
         season_start = season,
         season_end = season + window - 1) %>%
  filter(row %% 4 == 1,
         season_end <= 2024,
         season_start >= 1984) %>%
  select(season_end, unique_c_finalists_5)


##Window 3
window = 3

parity_f_df3 <- final_4_manual %>%
  arrange(season) %>%
  filter(finals == 1) %>%
  mutate(row = row_number()) %>%
  mutate(unique_finalists_3 = rollapply(TEAM_ID, width= window*2, 
                                        FUN=function(x) length(unique(x[!is.na(x)])), partial=TRUE, align='left'),
         season_start = season,
         season_end = season + window - 1) %>%
  filter(row %% 2 == 1,
         season_end <= 2024,
         season_start >= 1984) %>%
  select( season, season_start, season_end, unique_finalists_3)


parity_cf_df3 <- final_4_manual %>%
  arrange(season) %>%
  mutate(row = row_number()) %>%
  mutate(unique_c_finalists_3 = rollapply(TEAM_ID, width= window*4, 
                                          FUN=function(x) length(unique(x[!is.na(x)])), partial=TRUE, align='left'),
         season_start = season,
         season_end = season + window - 1) %>%
  filter(row %% 4 == 1,
         season_end <= 2024,
         season_start >= 1984) %>%
  select( season, season_start, season_end, unique_c_finalists_3)


parity_df <- parity_f_df3 %>%
  left_join(parity_cf_df3, by = c('season', 'season_start', 'season_end')) %>%
  left_join(parity_f_df5, by = c('season_end')) %>%
  left_join(parity_cf_df5, by = c( 'season_end')) %>%
  pivot_longer(cols = c(unique_finalists_3, unique_c_finalists_3, unique_finalists_5, unique_c_finalists_5), 
               names_to = "finals", values_to = "unique_teams")  %>%
  mutate(line_color = case_when(finals == 'unique_finalists_3' ~  '#AD002B',
                                finals == 'unique_c_finalists_3' ~  '#ED003B',
                                finals == 'unique_finalists_5' ~  '#006bb7',
                                finals == 'unique_c_finalists_5' ~  '#4AAFF7')) %>%
  filter((finals != 'unique_finalists_5') | (season_end != 2024))



parity_df %>%
  filter(finals != 'unique_finalists_3', finals != 'unique_c_finalists_3') %>%
  ggplot(aes(x = season_end, 
             y = unique_teams, 
             color = line_color,
             group = finals)
  ) +
  geom_line(linewidth = 1) +
  geom_point(linewidth = 1) +
  scale_color_identity() + 
  theme_saurabh() +
  labs(title = "", 
       subtitle= "",
       x="5-Year Window Ending", y="Unique Teams",
       caption = 'Data: stats.nba via HoopR | Viz: @SaurabhOnTap') + 
  theme(legend.position="none",
        axis.ticks.y = element_blank()) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 3))  +
  scale_x_continuous(limits = c(2003, 2024),  breaks = seq(1999, 2024, 3))





parity_df %>%
  filter(finals != 'unique_finalists_5', finals != 'unique_c_finalists_5') %>%
  ggplot(aes(x = season_end, 
             y = unique_teams, 
             color = line_color,
             group = finals)
  ) +
  geom_line(linewidth = 1) +
  geom_point(linewidth = 1) +
  scale_color_identity() + 
  theme_saurabh() +
  labs(title = "", 
       subtitle= "",
       x="3-Year Window Ending", y="Unique Teams",
       caption = 'Data: stats.nba via HoopR | Viz: @SaurabhOnTap') + 
  theme(legend.position="none",
        axis.ticks.y = element_blank()) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 2))  +
  scale_x_continuous( breaks = seq(1989, 2023, 3))



ggsave("parity_2024.png", height = 6, width = 6) 

