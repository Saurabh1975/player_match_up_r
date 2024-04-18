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
    geom_image(data = df %>% filter(MATCHUP_MIN > 1.5, DEF_PLAYER_NAME == highlight_player),
               aes(x = MATCHUP_MIN_cumsum, 
                   y = OFF_PLAYER_NAME, image = DEF_PLAYER_HEADSHOT), 
               size = 0.1, position = position_nudge(y = bar_spacing)) +
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



gamelog <-   readRDS('gamelog.rds')

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
          plotOutput(outputId = "matchup_plot_1")
        ),
        material_column(
          width=6,
          plotOutput(outputId = "matchup_plot_2")
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
          material_column(
            width=6,
            plotOutput(outputId = "player_plot")
          ),
        )
      )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  values <- reactiveValues()
  
  matchup_input <- reactiveVal()
  game_select <- reactiveVal()
  player_select <- reactiveVal()
  
  
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
    
    values$team_plot_1 <-  generate_team_def_plot(team_1_df, selected_game)
    
    values$team_plot_2 <-  generate_team_def_plot(team_2_df, selected_game)
  }
  
  
  update_player_charts <- function(df){
    
    
    
    
    
    
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
    
    
    print('creating player chart)')
    print(input$player_select)
    values$player_plot <-  get_player_def_chart(df, input$player_select, selected_game)
    
    player_list = unique((matchup_df %>% filter(game_id == selected_game_id) %>% arrange(DEF_PLAYER_NAME))$DEF_PLAYER_NAME)
    
    
    
  }
  
  
  observeEvent({
    input$matchup_select
  }, {
    matchup_input(input$matchup_select)
    
    game_choices <- unique((gamelog %>% filter(MATCHUP == matchup_input()))$game_number)
    
    update_material_dropdown(session = session, input_id = 'game_select', 
                             value = game_choices[1],
                             choices = game_choices)
    
    selected_game_id <- (gamelog %>% filter(MATCHUP == matchup_input(), game_number == game_choices[1]))$GAME_ID[1]
    print(selected_game_id)
    
    
    player_choices <- unique((matchup_df %>% filter(game_id == selected_game_id))$DEF_PLAYER_NAME)
    
   print(player_choices)
   print(typeof(player_choices))
    
    
    
    
    update_material_dropdown(session = session, input_id = 'player_select', 
                             value = player_choices[1],
                             choices = player_choices)
  })
  
  
  observeEvent({
    input$game_select
  },{
    
  })
  
  observeEvent({
    input$player_select
  },{
  })
  
  
  output$matchup_plot_1 <- renderPlot(values$team_plot_1,
                                      width=600,
                                      height=600)
  
  output$matchup_plot_2 <- renderPlot(values$team_plot_2,
                                      width=600,
                                      height=600)
  
  output$player_plot <- renderPlot(values$player_plot,
                                   width=600,
                                   height=600)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
