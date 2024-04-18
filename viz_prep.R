library(hoopR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(stringr)
library(png)
library(ggimage)
library(magick)
library(ggtext)
library(extrafont)
library(forcats)


gamelog <-  nba_leaguegamelog(league_id = '00', season = year_to_season(most_recent_nba_season() - 1),
                              season_type = 'Playoffs')[[1]] %>% 
  filter(!is.na(WL)) %>%
  mutate(MATCHUP = str_replace_all(MATCHUP, c("@" = "vs.")))  %>%
  select(GAME_ID, GAME_DATE, MATCHUP) %>%
  group_by(GAME_ID) %>%
  distinct(.keep_all = TRUE) %>%
  ungroup() %>%
  group_by(MATCHUP) %>%
  arrange(MATCHUP,GAME_DATE) %>%
  mutate(game_number = paste0("Game ", row_number()))

matchup_df <- readRDS('matchups.rds')


selected_matchup <- 'GSW vs. SAC'
selected_game <- gamelog[gamelog$MATCHUP == selected_matchup,]$game_number[2]
selected_game_id <- (gamelog %>% filter(MATCHUP == selected_matchup, game_number == selected_game))$GAME_ID





##viz_create

df <- matchup_df %>% filter(game_id == selected_game_id) %>%
  arrange(OFF_PLAYER_NAME, -MATCHUP_MIN) %>%
  group_by(off_team_id, OFF_PLAYER_ID, OFF_PLAYER_NAME) %>%
  mutate(min_played = sum(MATCHUP_MIN),
         MATCHUP_MIN_cumsum = cumsum(MATCHUP_MIN) - MATCHUP_MIN/2) %>%
  ungroup() %>%
  mutate(pct_matchup_time = MATCHUP_MIN/min_played,
         DEF_PLAYER_NAME_LAST = sub("^\\S+\\s", "", DEF_PLAYER_NAME),
         pct_matchup_time_scaled = scales::rescale(pct_matchup_time),
         DEF_PLAYER_HEADSHOT = paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", DEF_PLAYER_ID, ".png"),
         OFF_PLAYER_HEADSHOT = paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", OFF_PLAYER_ID, ".png")) 


df <- df  %>%
  filter(off_team_id == unique(df$off_team_id)[1]) %>%
  arrange(-min_played, -MATCHUP_MIN)


plot <- generate_team_def(df)

print(plot)

generate_team_def_plot <- function(df){
  
  #Get Only Top 10
  df <- df %>% filter(OFF_PLAYER_NAME %in% (unique(df$OFF_PLAYER_NAME)[1:10]))
  
  
  
  
  # Reorder levels of OFF_PLAYER_NAME by total MIN
  df_totals_summary <- df %>%
    group_by(OFF_PLAYER_NAME, DEF_PLAYER_NAME) %>%
    summarize(total_MIN = sum(MATCHUP_MIN)) %>%
    group_by(OFF_PLAYER_NAME) %>%
    summarize(total_MIN = sum(total_MIN)) %>%
    arrange(-desc(total_MIN)) %>%
    pull(OFF_PLAYER_NAME)
  
  df$OFF_PLAYER_NAME <- factor(df$OFF_PLAYER_NAME, levels = df_totals_summary)
  df$DEF_PLAYER_NAME <- fct_inorder(df$DEF_PLAYER_NAME, df$OFF_PLAYER_NAME)
  
  
  bar_width <- 0.35
  bar_spacing <- 0.5
  headshot_cutoff <- 2
  
  
  
  # Create horizontal stacked bar chart
  g <- ggplot(df, aes(x = MATCHUP_MIN, y = OFF_PLAYER_NAME)) +
    geom_bar(stat = "identity", color = "white", width = bar_width, 
             aes(alpha = pct_matchup_time_scaled, fill = off_team_color)) +
    scale_fill_identity() +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"))   +
    geom_text(data = df, aes(x = MATCHUP_MIN_cumsum, y = OFF_PLAYER_NAME, fill=NULL,
                             label = ifelse(df$MATCHUP_MIN > 1.5, df$DEF_PLAYER_NAME_LAST, "")),
              color=ifelse(df$pct_matchup_time_scaled > 0.3, 'white', '#303030'),
              size = 2.4, hjust = 0.5) + 
    geom_image(data = df %>% filter(MATCHUP_MIN > 1.5),
               aes(x = MATCHUP_MIN_cumsum, 
                   y = OFF_PLAYER_NAME, image = DEF_PLAYER_HEADSHOT), 
               size = 0.1, position = position_nudge(y = bar_spacing)) +
    theme(legend.position="none") + 
    theme_saurabh() +
    labs(title=paste0(selected_game , ": ",  df$off_team_abv[1], "/", df$def_team_abv[1],  ' Defensive Match Ups'),
         subtitle = paste0("How ", df$def_team_abv[1], " Matched Up with the ", df$off_team_name),
         caption = 'Data: stats.nba.com via nba_api | Viz: @SaurabhOnTap') + 
    xlab("Minutes Matched Up") + 
    ylab("") +
    coord_cartesian(ylim = c(1, length(unique(df$OFF_PLAYER_ID))*1.025
    )) 
  
  return(g)

}
