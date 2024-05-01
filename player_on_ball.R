library(hoopR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(janitor)
library(ggpath)

loadfonts()






theme_saurabh <- function () { 
  theme(
    text=element_text(family='Roboto Mono'),
    plot.title=element_text(size=18, family = 'Roboto Black'),
    plot.subtitle=element_text(size=9, family = 'Roboto Slab'),
    plot.caption=element_text(size=6, family = 'Roboto Mono'),
    axis.title =element_text(size=7, family = 'Roboto Mono'),
    panel.background = element_blank(),
    
  )
}
#Get List of Playoff Games
gamelog <-  nba_leaguegamelog(league_id = '00', season = year_to_season(most_recent_nba_season() - 1),
                              season_type = 'Playoffs')[[1]] %>%
  filter(!is.na(WL)) %>%
  select(TEAM_ID, TEAM_NAME, GAME_ID, GAME_DATE)




team_id <- gamelog %>% pull(TEAM_ID) %>% head(1)
game_date <- gamelog %>% pull(GAME_DATE) %>% head(1)
game_id <- gamelog %>% pull(GAME_ID) %>% head(1)


df <- nba_leaguedashptstats(team_id = team_id, pt_measure_type  = 'Possessions',
                            date_to = game_date, date_from = game_date, season_type  = 'Playoffs')[[1]] %>%
    mutate(game_id = game_id)



dfs <- list()

# Iterate through each row of gamelog
for (i in 1:nrow(gamelog)) {
  # Extract TEAM_ID, GAME_DATE, and GAME_ID
  team_id <- gamelog[i, "TEAM_ID"]
  game_date <- gamelog[i, "GAME_DATE"]
  game_id_temp <- as.character(gamelog[i, "GAME_ID"])
  
  print(game_id_temp)
  
  
  # Fetch dataframe from nba_leaguedashptstats
  df <- nba_leaguedashptstats(team_id = team_id, pt_measure_type = 'Possessions',
                              date_to = game_date, date_from = game_date, season_type = 'Playoffs')[[1]] %>%
    mutate(game_id = game_id_temp)  %>%
    janitor::clean_names()
  
  # Append dataframe to the list
  dfs[[i]] <- df
  
  Sys.sleep(3)
  
}


# Combine all dataframes into one
combined_df <- do.call(rbind, dfs) %>%
  janitor::clean_names()   %>% 
  mutate(time_of_poss = as.numeric(time_of_poss),
         min = as.numeric(min),
         time_of_poss_pct = time_of_poss/min,
         player_name_last = sub("^\\S+\\s", "", player_name)) %>%
  select(game_id, player_id, player_name, player_name_last ,time_of_poss_pct,
         team_id, team_abbreviation, min, time_of_poss, time_of_poss_pct) %>% 
  left_join(gamelog_enchanced %>% 
              janitor::clean_names() %>%
              select(team_id, game_id, game_number, matchup, opp_name, 
                     matchup_full, team_color, opp_color), by = c('team_id', 'game_id')) %>%
  mutate(player_headshot =  paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", player_id, ".png"))








for(team in unique(combined_df$team_abbreviation)){
  print(team)
  generate_charts(team)
}

team_filter = "PHX"



generate_charts <- function(team_filter){

  
  # Combine all dataframes into one
  viz_df_bar <- combined_df %>%
    group_by(game_id, team_id) %>%
    arrange(-time_of_poss) %>%
    mutate(time_of_poss_cumsum =  cumsum(time_of_poss),
           time_of_poss_scale = scales::rescale(time_of_poss),
    ) %>%
    dplyr::filter(team_abbreviation == team_filter) %>%
    ungroup() %>%
    group_by(game_id, team_id) %>%
    arrange(-time_of_poss)
    
    
  
  bar_width <- 0.35
  bar_spacing <- 0.2 + .04*length(unique(viz_df_bar$game_number))
  
  # Create horizontal stacked bar chart
  g1 <- ggplot(viz_df_bar, aes(x = time_of_poss, y = game_number)) +
    geom_bar(stat = "identity", color = "white", width = bar_width, 
             aes(alpha = time_of_poss, fill = team_color)) +
    scale_fill_identity() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"))   +
    geom_text( aes(x = time_of_poss_cumsum - time_of_poss/2 , y = game_number, fill=NULL,
                   label = ifelse(viz_df_bar$time_of_poss > 1.5, viz_df_bar$player_name_last, "")),
               color='white',
               size = 2.4, hjust = 0.5, fontface = 'bold') + 
    geom_from_path(data = viz_df_bar %>% filter(time_of_poss > 1.5),
                   aes(x = time_of_poss_cumsum - time_of_poss/2, 
                       y = game_number, path = player_headshot), 
                   width = 0.1, height = 0.0735, position = position_nudge(y = bar_spacing)) +
    theme(legend.position="none") + 
    theme_saurabh() +
    labs(title=paste0("Who Had the Ball "),
         subtitle = paste0(viz_df_bar$matchup[1], ", 2024"),
         caption = 'Data: stats.nba.com via nba_api | Viz: @SaurabhOnTap',
         x = "Time on Ball (Min.)", y = "") + 
    coord_cartesian(ylim = c(1, length(unique(viz_df_bar$game_number))*1.025
    ))  + 
    scale_y_discrete(limits=rev)
  
  
  
  
  ggsave(plot = g1, filename = paste0("Plots/", viz_df_bar$team_name[1], " - ", viz_df_bar$opp_name[1], " - Bar.png"), 
    height = 6, width = 6) 
  
  
  player_list <- combined_df %>%
    group_by(game_id, team_id) %>%
    arrange(-time_of_poss) %>%
    slice(1:3) %>%
    pull(player_id) %>%
    unique()
    
  
  
  # Combine all dataframes into one
  viz_df_line_master <- combined_df %>%
    filter(player_id %in%  player_list)
  
  
  viz_df_line <- viz_df_line_master %>%
                        filter(team_abbreviation == team_filter) 
    
  
  g2 <- ggplot(viz_df_line, aes(x = game_number, y = time_of_poss, group = player_id,
                          color = team_color)) +
    geom_line(size = 1.5) +
    geom_point(size = 3) +
    scale_color_identity() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"))   +
    geom_text(data = viz_df_line %>% filter(game_number == max(viz_df_line$game_number)),
              aes(x = game_number , y = time_of_poss, fill=NULL,
                  label = player_name_last),
              color='#404040',
              size = 4, hjust = 0, 
              position = position_nudge(x = 0.25, y = -0.125)) +
    geom_from_path( data = viz_df_line %>% filter(game_number == max(viz_df_line$game_number)),
               aes(x = game_number , y = time_of_poss, fill=NULL,
                   path = player_headshot, color = NULL),
               width = 0.1, height = 0.0735, position = position_nudge(x = 0.125, y = -.125), 
               ) + 
    labs(title=paste0("Who Had the Ball "),
         subtitle = paste0(viz_df_bar$matchup[1], ", 2024"),
         caption = 'Data: stats.nba.com via nba_api | Viz: @SaurabhOnTap') + 
    theme_saurabh() +
    xlab("") + 
    ylab("Time on Ball (Min.)") +
    coord_cartesian(xlim = c(1, length(unique(viz_df_line$game_number))*1.025),
                    ylim = c(0, max(viz_df_line$time_of_poss)*1.025)
    )
  
  
  
  
  ggsave(plot = g2, filename = paste0("Plots/", viz_df_line$team_name[1], " - ", viz_df_line$opp_name[1], " - Line.png"), 
         height = 6, width = 6) 
}
