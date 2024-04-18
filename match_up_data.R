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
library(ggpath)

loadfonts(device="win")



library(hoopR)

df <- nba_boxscorematchupsv3(game_id = "0022200021")[[1]]


theme_saurabh <- function () { 
  theme(
    text=element_text(family='Roboto Mono'),
    plot.title=element_text(size=14, family = 'Roboto Black', color = '#101010'),
    plot.subtitle=element_text(size=10, family = 'Roboto Slab'),
    plot.caption=element_text(size=7, family = 'Roboto Mono'),
    panel.background = element_blank(),
    
  )
}


df_orig <- read.csv('match_ups_sample.csv')   %>%
  separate(MATCHUP_MIN, c("minutes", "seconds"), sep = ":", convert = TRUE) %>%
  mutate(MATCHUP_MIN = minutes + seconds/60) %>%
  select(-c(minutes, seconds, MATCHUP_TIME_SEC))  %>%
  select(off_team_id, OFF_PLAYER_ID, OFF_PLAYER_NAME, off_team_color, off_team_abv, off_team_name,
         def_team_id, DEF_PLAYER_ID, DEF_PLAYER_NAME, def_team_color, def_team_abv, def_team_name,
         MATCHUP_MIN) %>%
  arrange(OFF_PLAYER_NAME, -MATCHUP_MIN) %>%
  group_by(off_team_id, OFF_PLAYER_ID, OFF_PLAYER_NAME) %>%
  mutate(min_played = sum(MATCHUP_MIN),
         MATCHUP_MIN_cumsum = cumsum(MATCHUP_MIN) - MATCHUP_MIN/2) %>%
  ungroup() %>%
  mutate(pct_matchup_time = MATCHUP_MIN/min_played,
         DEF_PLAYER_NAME_LAST = sub("^\\S+\\s", "", DEF_PLAYER_NAME),
         pct_matchup_time_scaled = scales::rescale(pct_matchup_time),
         DEF_PLAYER_HEADSHOT = paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", DEF_PLAYER_ID, ".png"),
          OFF_PLAYER_HEADSHOT = paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", OFF_PLAYER_ID, ".png"))  %>%
 # filter(off_team_id == 1610612755) %>%
  arrange(-min_played, -MATCHUP_MIN)
  

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



df_cumsum <- df %>%
  group_by(OFF_PLAYER_NAME) %>%
  summarize(MATCHUP_MIN_cumsum = cumsum(MATCHUP_MIN) - MATCHUP_MIN/2)



bar_width <- 0.35
bar_spacing <- 0.5
headshot_cutoff <- 2



# Create horizontal stacked bar chart
ggplot(df, aes(x = MATCHUP_MIN, y = OFF_PLAYER_NAME)) +
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
  labs(title=paste0("Game 1: ",  df$off_team_abv[1], "/", df$def_team_abv[1],  ' Defensive Match Ups'),
       subtitle = paste0("How ", df$def_team_abv[1], " Matched Up with the ", df$off_team_name),
       caption = 'Data: stats.nba.com via nba_api | Viz: @SaurabhOnTap') + 
  xlab("Defensive Match Ups") + 
  ylab("") +
  coord_cartesian(ylim = c(1, length(unique(df$OFF_PLAYER_ID))*1.025
                           )) 



ggsave(paste(df$off_team_abv[1], "_def_by_", df$def_team_abv[1], ".png"), height = 6, width = 6) 





highlight_player <- "De'Aaron Fox"
    



# Create horizontal stacked bar chart
ggplot(df, aes(x = MATCHUP_MIN, y = OFF_PLAYER_NAME)) +
  geom_bar(stat = "identity", color = "white", width = bar_width, 
           alpha = ifelse(df$DEF_PLAYER_NAME == highlight_player, 1, 0.4),
           fill =  ifelse(df$DEF_PLAYER_NAME == highlight_player, df$def_team_color, df$off_team_color)
           )+
  scale_fill_identity() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))   +
  geom_text(data = df, aes(x = MATCHUP_MIN_cumsum, y = OFF_PLAYER_NAME, fill=NULL,
                           label = ifelse(df$MATCHUP_MIN > 1.5, df$DEF_PLAYER_NAME_LAST, "")),
            color=ifelse(df$DEF_PLAYER_NAME == highlight_player, 'white', '#303030'),
            size = 2.4, hjust = 0.5) + 
  geom_image(data = df %>% filter(MATCHUP_MIN > 1.5, DEF_PLAYER_NAME == highlight_player),
             aes(x = MATCHUP_MIN_cumsum, 
                 y = OFF_PLAYER_NAME, image = DEF_PLAYER_HEADSHOT), 
             size = 0.1, position = position_nudge(y = bar_spacing)) +
  theme(legend.position="none") + 
  theme_saurabh() +
  labs(title=paste0("Game 1: ",  df$off_team_abv[1], "/", df$def_team_abv[1],  ' Defensive Match Ups'),
       subtitle = paste0("How ", df$def_team_abv[1], " Deployed ", highlight_player, " Defensively"),
       caption = 'Data: stats.nba.com via nba_api | Viz: @SaurabhOnTap') + 
  xlab("Defensive Match Ups") + 
  ylab("") +
  coord_cartesian(ylim = c(1, length(unique(df$OFF_PLAYER_ID))*1.025
  )) 

ggsave(paste(highlight_player, "_def_deployement", ".png"), height = 6, width = 6) 






###Series Level Flow




gamelog <-   readRDS('gamelog.rds')


matchup_df <- readRDS('matchups.rds') 


higlight_player <- 'Stephen Curry'
series <- 'SAC vs. GSW'

print(generate_series_plot(matchup_df, series, higlight_player))


generate_series_plot <- function(df, highlight_series, highlight_player){


  filtered_gamelog <- gamelog %>% filter(MATCHUP == highlight_series) %>% select(GAME_ID, game_number)

  
  df <- df %>%   right_join(filtered_gamelog, by = c('game_id' = 'GAME_ID')) %>%
    filter(OFF_PLAYER_NAME == higlight_player) %>% group_by(off_team_id, OFF_PLAYER_ID, OFF_PLAYER_NAME) %>%
    group_by(game_number)  %>%
    arrange( -MATCHUP_MIN)  %>%
    mutate(min_played = sum(MATCHUP_MIN),
           MATCHUP_MIN_cumsum = cumsum(MATCHUP_MIN) - MATCHUP_MIN/2) %>%
    ungroup() %>%
    mutate(pct_matchup_time = MATCHUP_MIN/min_played,
           DEF_PLAYER_NAME_LAST = sub("^\\S+\\s", "", DEF_PLAYER_NAME),
           pct_matchup_time_scaled = scales::rescale(pct_matchup_time),
           DEF_PLAYER_HEADSHOT = paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", DEF_PLAYER_ID, ".png"),
           OFF_PLAYER_HEADSHOT = paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", OFF_PLAYER_ID, ".png"))  %>%
    arrange(-min_played, -MATCHUP_MIN)


  
  
  bar_width <- 0.35
  bar_spacing <- 0.2 + .04*length(unique(df$game_number))
  
  # Create horizontal stacked bar chart
  g <- ggplot(df, aes(x = MATCHUP_MIN, y = game_number)) +
    geom_bar(stat = "identity", color = "white", width = bar_width, 
             aes(alpha = pct_matchup_time_scaled, fill = off_team_color)) +
    scale_fill_identity() +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"))   +
    geom_text( aes(x = MATCHUP_MIN_cumsum, y = game_number, fill=NULL,
                             label = ifelse(df$MATCHUP_MIN > 1.5, df$DEF_PLAYER_NAME_LAST, "")),
              color='white',
              size = 2.4, hjust = 0.5) + 
    geom_from_path(data = df %>% filter(MATCHUP_MIN > 1.5),
                   aes(x = MATCHUP_MIN_cumsum, 
                       y = game_number, path = DEF_PLAYER_HEADSHOT), 
                   width = 0.1, height = 0.0735, position = position_nudge(y = bar_spacing)) +
    theme(legend.position="none") + 
    theme_saurabh() +
    labs(title=paste0("Game 1: ",  df$off_team_abv[1], "/", df$def_team_abv[1],  ' Defensive Match Ups'),
         subtitle = paste0("How ", df$def_team_abv[1], " Matched Up with the ", df$off_team_name),
         caption = 'Data: stats.nba.com via nba_api | Viz: @SaurabhOnTap') + 
    xlab("Defensive Match Ups") + 
    ylab("") +
    coord_cartesian(ylim = c(1, length(unique(df$game_number))*1.025
    ))  + 
    scale_y_discrete(limits=rev)
  
  
  return(g)
  
  }





