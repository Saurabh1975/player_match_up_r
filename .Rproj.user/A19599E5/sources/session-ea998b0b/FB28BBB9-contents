#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinymaterial)
library(shinyjs)

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
library(ggpath)
library(Cairo)
loadfonts(device="win")


options(shiny.usecairo  = TRUE)



###My Visual Theme
theme_saurabh <- function () { 
  theme(
    text=element_text(family='Roboto Mono'),
    plot.title=element_text(size=14, family = 'Roboto Black', color = '#101010'),
    plot.subtitle=element_text(size=10, family = 'Roboto Slab'),
    plot.caption=element_text(size=7, family = 'Roboto Mono'),
    panel.background = element_blank(),
    
  )
}


generate_team_def_plot <- function(df, selected_game){
  
  
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
    geom_from_path(data = df %>% filter(MATCHUP_MIN > 1.5),
               aes(x = MATCHUP_MIN_cumsum, 
                   y = OFF_PLAYER_NAME, path = DEF_PLAYER_HEADSHOT), 
               width = 0.1, height = 0.0735, position = position_nudge(y = bar_spacing)) +
    theme(legend.position="none") + 
    theme_saurabh() +
    labs(title=paste0(selected_game , ": ",  df$off_team_abv[1], "/", df$def_team_abv[1],  ' Defensive Match Ups'),
         subtitle = paste0("How ", df$def_team_abv[1], " Matched Up with the ", df$off_team_name),
         caption = 'Data: stats.nba.com via hoopR | Viz: @SaurabhOnTap') + 
    xlab("Minutes Matched Up") + 
    ylab("") +
    coord_cartesian(ylim = c(1, length(unique(df$OFF_PLAYER_ID))*1.025
    )) 
  
  return(g)
  
}




get_player_def_chart <- function(df, highlight_player, selected_game){
  

  
  player_team = (df %>% filter(DEF_PLAYER_NAME ==  highlight_player))$def_team_id[1]
  
  
  #Get Only Top 10
  df <- df %>% filter(def_team_id == player_team) 
  df <- df %>%
    filter(OFF_PLAYER_NAME %in% (unique(df$OFF_PLAYER_NAME)[1:10]))
  

  
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
    geom_from_path(data = df %>% filter(MATCHUP_MIN > 1.5, DEF_PLAYER_NAME == highlight_player),
               aes(x = MATCHUP_MIN_cumsum, 
                   y = OFF_PLAYER_NAME, path = DEF_PLAYER_HEADSHOT), 
               width = 0.1, height = 0.0735, position = position_nudge(y = bar_spacing)) +
    theme(legend.position="none") + 
    theme_saurabh() +
    labs(title=paste0(selected_game , ": ",  df$off_team_abv[1], "/", df$def_team_abv[1],  ' Defensive Match Ups'),
         subtitle = paste0("How ", df$def_team_abv[1], " Deployed ", highlight_player, " Defensively"),
         caption = 'Data: stats.nba.com via nba_api | Viz: @SaurabhOnTap') + 
    xlab("Minutes Matched Up") + 
    ylab("") +
    coord_cartesian(ylim = c(1, length(unique(df$OFF_PLAYER_ID))*1.025
    )) 
  
  
  
  return(g)
  
}



generate_series_plot <- function(df, highlight_series, highlight_player){
  
  

  
  
  filtered_gamelog <- gamelog %>% filter(MATCHUP == highlight_series) %>% select(GAME_ID, game_number)
  
  print("In Generate Function")
  print(highlight_series)
  print(highlight_player)
  print(df)
  print(filtered_gamelog)
  
  
  
  df <- df %>%   right_join(filtered_gamelog, by = c('game_id' = 'GAME_ID')) %>%
    filter(OFF_PLAYER_NAME == highlight_player) %>% group_by(off_team_id, OFF_PLAYER_ID, OFF_PLAYER_NAME) %>%
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
    labs(title=paste0(df$off_team_abv[1], "/", df$def_team_abv[1],  ' Series Defense'),
         subtitle = paste0("How ", df$def_team_abv[1], " matched up with ", highlight_player, " through the series"),
         caption = 'Data: stats.nba.com via nba_api | Viz: @SaurabhOnTap') + 
    xlab("Minutes Matched Up") + 
    ylab("") +
    coord_cartesian(ylim = c(1, length(unique(df$game_number))*1.025
    ))  + 
    scale_y_discrete(limits=rev)
  
  
  return(g)
  
}


gamelog <-   readRDS('gamelog.rds')

last_updated <- gamelog$version[1]

matchup_df <- readRDS('matchups.rds') 



initial_game_list = unique(gamelog %>% filter(MATCHUP == unique(gamelog$MATCHUP)[1]))$game_number
initial_game_id <- (gamelog %>% filter(MATCHUP == unique(gamelog$MATCHUP)[1], game_number == 'Game 1'))$GAME_ID
intial_player_list = unique((matchup_df %>% filter(game_id == initial_game_id) %>% arrange(DEF_PLAYER_NAME))$DEF_PLAYER_NAME)


# Define UI for application that draws a histogram
ui <- 
  material_page(
    title = "Defensive Matchup",
    include_nav_bar = FALSE,
    include_fonts = T,
    material_row(
      material_column(
        width=1,
      ),
      shiny::tags$p("Defensive Matchups.r", 
                    style = "font-family: 'Roboto Slab'; font-size:24pt; vertical-align: baseline;")
    ),
    material_row(
        material_column(
          width=1,
        ),
      tags$footer(
        "App by ",
        tags$a(
          "@SaurabhOnTap",
          target = "_blank",
          href = "https://twitter.com/SaurabhOnTap"
        ),
        paste0("| Note: Initial load may be slow | Last Updated: ", last_updated),
        style = "font-family: 'Roboto Slab'; width: 100%; color: black; text-align: left; font-size:8pt;"
      )
    ),
    material_row(
      material_column(
        width=2,
        offset=1,
        align="center",
        material_dropdown(
          input_id='matchup_select',
          label='Select Series',
          choices = unique(gamelog$MATCHUP) ,
          selected = unique(gamelog$MATCHUP)[1],
          multiple = FALSE
        )
      ),
      material_column(
        width=2,
        offset=0.5,
        material_dropdown(
          input_id='game_select',
          label='Select Series',
          choices = unique(gamelog %>% filter(MATCHUP == unique(gamelog$MATCHUP)[1]))$game_number),
          selected = 'Game 1',
          multiple = FALSE
        )
      ),
    material_tabs(
      tabs = c( "Team Level" = "team_level", "Player Level" = "player_level")
    ),
    material_row(),
    material_tab_content(
      tab_id = "team_level", 
    material_row(
      material_column(
        width=6,
        imageOutput(outputId = "matchup_plot_1")
      ),
      material_column(
        width=6,
        imageOutput(outputId = "matchup_plot_2")
      )
    ),
    ),
    material_tab_content(
      tab_id = "player_level",
      material_row(
        material_column(
          width=2,
          offset = 4,
          material_dropdown(
            input_id='player_select',
            label='Select Player',
            choices = intial_player_list,
          selected = intial_player_list[1],
          multiple = FALSE
        ),

        )
      ),
      material_row(
        material_column(
          width=6,
          imageOutput(outputId = "player_plot")
        ),
        material_column(
          width=6,
          imageOutput(outputId = "player_series_plot")
        )
      )
      )
  )
 

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  values <- reactiveValues()

  
  update_team_charts <- function(df){
    
    
    
    
    
    
    selected_matchup <- input$matchup_select
    selected_game <- input$game_select
    selected_game_id <- (gamelog %>% filter(MATCHUP == selected_matchup, game_number == selected_game))$GAME_ID
    
    
    
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
    
    team_1_df <- df  %>%
      filter(off_team_id == unique(df$off_team_id)[1]) %>%
      arrange(-min_played, -MATCHUP_MIN)
    
    team_2_df <- df  %>%
      filter(off_team_id == unique(df$off_team_id)[2]) %>%
      arrange(-min_played, -MATCHUP_MIN)
    
    #values$team_plot_1 <-  generate_team_def_plot(team_1_df, selected_game)
    
    
    if(!file.exists(paste0("t1", selected_matchup, selected_game, '.png'))){
      print("generating new file")
      ggsave(plot = generate_team_def_plot(team_1_df, selected_game), 
             filename = paste0("t1", selected_matchup, selected_game, '.png'), height = 6, width = 6) 
    }
    else{
      print("file exists!")
    }
    

    
    
    values$team_plot_1_image <-paste0("t1", selected_matchup, selected_game, '.png')

    
    if(!file.exists(paste0("t2", selected_matchup, selected_game, '.png'))){
      ggsave(plot = generate_team_def_plot(team_2_df, selected_game), 
             filename = paste0("t2", selected_matchup, selected_game, '.png'), height = 6, width = 6) 
    }
    else{
      print("file exists!")
    }
    
    
    
    
    values$team_plot_2_image <-paste0("t2", selected_matchup, selected_game, '.png')
    
    #values$team_plot_2 <-  generate_team_def_plot(team_2_df, selected_game)
  }
  
  
  update_player_charts <- function(df){
    

    

    
    selected_matchup <- input$matchup_select
    selected_game <- input$game_select
    selected_game_id <- (gamelog %>% filter(MATCHUP == selected_matchup, game_number == selected_game))$GAME_ID
    
    
    print("filter player df")
    
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
    

    print("filterd df, about to save")
    
    
    
    ggsave(plot = get_player_def_chart(df, input$player_select, selected_game), 
           filename = paste0(input$player_select, selected_matchup, selected_game, '.png'), height = 6, width = 6) 
    
    
    
    
    values$player_plot_image <-paste0(input$player_select, selected_matchup, selected_game, '.png')
  }
  
  update_player_series_chart <- function(df){
    
    print("Sending Over the following")
    print(input$player_select)
    print(input$matchup_select)    
    
    ggsave(plot = generate_series_plot(matchup_df, highlight_series  =  input$matchup_select, 
                                       highlight_player = input$player_select), 
           filename = paste0(input$player_select, input$matchup_select, "_series", '.png'), height = 6, width = 6) 
    

    
    
    values$player_series_image <- paste0(input$player_select, input$matchup_select, "_series", '.png')
    
    
    
    
    
  }
  

    
  
  observeEvent({
  input$matchup_select
  }, {
    
    
    series <- input$matchup_select
    
    game_options <- unique((gamelog %>% filter(MATCHUP == series))$game_number)
    
    
    
    selected_game_id <- (gamelog %>% filter(MATCHUP == series, game_number == 'Game 1'))$GAME_ID
    player_list = unique((matchup_df %>% filter(game_id == selected_game_id) %>% arrange(DEF_PLAYER_NAME))$DEF_PLAYER_NAME)

    default_player = (matchup_df %>% filter(game_id == selected_game_id) %>% arrange(-MATCHUP_MIN))$DEF_PLAYER_NAME[1] 
    
    
    update_material_dropdown(session, 'player_select',
                             value = default_player,
                             choices = player_list)
    
    
    
        
    update_material_dropdown(session, 'game_select',
                             value = 'Game 1',
                             choices =  game_options)
    
    update_team_charts(df)
    
  })



  observeEvent({
    input$game_select
  },{
    

    series <- input$matchup_select

    game_options <- unique((gamelog %>% filter(MATCHUP == series))$game_number)


    selected_game_id <- (gamelog %>% filter(MATCHUP == series, game_number == input$game_select))$GAME_ID
    player_list = unique((matchup_df %>% filter(game_id == selected_game_id) %>% arrange(DEF_PLAYER_NAME))$DEF_PLAYER_NAME)


    
    default_player = (matchup_df %>% filter(game_id == selected_game_id) %>% arrange(-MATCHUP_MIN))$DEF_PLAYER_NAME[1] 
    
    
    update_material_dropdown(session, 'player_select',
                             value = default_player,
                             choices = player_list)
    

    update_team_charts(df)
    
    update_player_charts(df)
    

  })

  observeEvent({
    input$player_select
  },{
    update_player_charts(df)
    update_player_series_chart(df)
  })

  output$matchup_plot_1 <-  renderImage({
    list(src = values$team_plot_1_image,
         width = 600,
         height = 600)
  }, deleteFile = FALSE)
  
  output$matchup_plot_2 <-  renderImage({
    list(src = values$team_plot_2_image,
         width = 600,
         height = 600)
  }, deleteFile = FALSE)
  
  output$player_plot <- renderImage({
    list(src = values$player_plot_image,
         width = 600,
         height = 600)
  }, deleteFile = TRUE)
  output$player_series_plot <- renderImage({
    list(src = values$player_series_image,
         width = 600,
         height = 600)
  }, deleteFile = TRUE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
