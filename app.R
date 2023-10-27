library(shiny)
library(ggplot2)
library(png)
library(magick)
library(shinyjs)
library(shinyalert)
library(dplyr)
library(tidyverse)
library(gtsummary)
library(gapminder)

load("Cricket.RData")
load("Cricket_Match.RData")
load("Teams_Table.RData")
load("Cricket_Coord.RData")

ui <- fluidPage(
  titlePanel("ODI"),
  tabsetPanel(
    tabPanel("Player ODI Data",
             sidebarLayout(
               sidebarPanel(
                 selectInput("country", "Select Country:",
                             choices = Country_name,
                             selected = Country_name[5]),
                 uiOutput("player"),
                 uiOutput("ballorbat")
               ),
               mainPanel(
                 plotOutput("img"),
                 tableOutput("data"),
                 plotOutput("plot")
               )
             )
    ),
    tabPanel("Team ODI Data", 
             tabsetPanel(
               tabPanel("World View",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput(
                              inputId = "y_world",
                              label = "Years : ",
                              min = min(year(table$`Match Date`)),
                              max = max(year(table$`Match Date`)),
                              value = c(min(year(table$`Match Date`)), max(year(table$`Match Date`)))
                            )
                          ),
                          mainPanel(
                            plotOutput("world")
                          )
                        )
               ),
               tabPanel("Team Overview",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("team", "Select Team : ",
                                        choices = unique(table$Winner)),
                            uiOutput("years")
                          ),
                          mainPanel(
                            verbatimTextOutput("team_summary"),
                            tableOutput("h2h")
                          )
                        )
                      ),
               tabPanel("Match Up",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("team1", "Select Team 1 : ",
                                        choices = unique(table$Winner),
                                        selected = "Australia"),
                            selectInput("team2", "Select Team 2 : ",
                                        choices = unique(table$Winner),
                                        selected = "England"),
                            uiOutput("years_match")
                          ),
                          mainPanel(
                            verbatimTextOutput("match_summary")
                          )
                        )
                    )
             )
    ),
  
    tabPanel("Cricket Match", 
             sidebarLayout(
               sidebarPanel(
                 span(textOutput("hint")),
                 selectInput("team_select_1", "Select team 1", 
                             choices = c(Country_name, "new_team")),
                 conditionalPanel(
                   condition = "input.team_select_1 == 'new_team'", 
                   selectInput("player_names", "Select Players (exect 11 players):", 
                               choices = Player_name, multiple = TRUE),
                   span(textOutput("player_counter")),
                   uiOutput("no_of_player")
                 ),
                 selectInput("team_select_2", "Select team 2", 
                             choices = c(Country_name, "new_team")),
                 conditionalPanel(
                   condition = "input.team_select_2 == 'new_team'", 
                   selectInput("player_names_2", "Select Players (exect 11 players):", 
                               choices = Player_name, multiple = TRUE),
                   span(textOutput("player_counter_2")),
                   uiOutput("no_of_player_2")
                 ),
                 actionButton("start", "Start Match")
               ),
               mainPanel(
                  verbatimTextOutput("team_output")
               )
             )
              # UI components specific to Cricket Match tab
             # Add your UI elements here
    )
  )
)

server <- function(input, output) {
  
  team <- reactive({
    input$team
  })
  
  team_1 <- reactive({
    input$team1
  })
  
  team_2 <- reactive({
    input$team2
  })
  
  team_table <- reactive({
    team_filter(team())
  })
  
  team_1_table <- reactive({
    team_filter(team_1())
  })
  
  match_table <- reactive({
    team_1_table() %>% filter(Opponent == team_2())
  })
  
  y_world_1 <- reactive({
    t <- input$y_world
    t[1]
  })
  
  y_world_2 <- reactive({
    t <- input$y_world
    t[2]
  })
  
  output$world <- renderPlot({
    teams <- unique(table$Loser)[c(1:6, 8:19, 24:28)]
    W_L <- NULL
    t <- table %>% filter((year(`Match Date`) >= y_world_1()) & (year(`Match Date`) <= y_world_2()))
    for (i in 1:length(teams)) {
      win <- sum(t$Winner == teams[i])
      loss <- sum(t$Loser == teams[i])
      if (loss == 0) {
        W_L <- append(W_L, NA)
      }
      else {
        W_L <- append(W_L, win/loss)
      }
    }
    WL <- data.frame(teams, W_L)
    data <- left_join(coord, WL, by = "teams")
    ggplot(data, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = W_L), color = "black")
  })
  
  output$years <- renderUI({
    sliderInput(
      inputId = "y",
      label = "Years : ",
      min = min(year(team_table()$`Match Date`)),
      max = max(year(team_table()$`Match Date`)),
      value = c(min(year(team_table()$`Match Date`)), max(year(team_table()$`Match Date`)))
    )
  })
  
  output$years_match <- renderUI({
    sliderInput(
      inputId = "ym",
      label = "Years : ",
      min = min(year(match_table()$`Match Date`)),
      max = max(year(match_table()$`Match Date`)),
      value = c(min(year(match_table()$`Match Date`)), max(year(match_table()$`Match Date`)))
    )
  })
  
  year_team_1 <- reactive({
    t <- input$y
    t[1]
  })
  year_team_2 <- reactive({
    t <- input$y
    t[2]
  })
  
  year_match_1 <- reactive({
    t <- input$ym
    t[1]
  })
  
  year_match_2 <- reactive({
    t <- input$ym
    t[2]
  })
  
  output$team_summary <- renderPrint({
    t <- team_table()
    t1 <- (t %>% filter((as.integer(year(t$'Match Date')) >= year_team_1()) & (as.integer(year(t$'Match Date')) <= year_team_2())))
    colnames(t1)[3] <- paste0(team(), " Score")
    colnames(t1)[4] <- "Opponent Score"
    colnames(t1)[7] <- "Winning Score"
    colnames(t1)[8] <- "Losing Score"
    t1 <- t1[c(3, 4, 7, 8, 11)]
    summary(t1)
  })
  
  output$h2h <- renderTable({
    t2 <- team_table()
    t <- t2 %>% filter((as.integer(year(t2$'Match Date')) >= year_team_1()) & (as.integer(year(t2$'Match Date')) <= year_team_2()))
    Team <- unique(t$Opponent)
    other <- Team
    Won <- NULL
    Lost <- NULL
    for (i in 1:length(other)) {
      t1 <- t %>% filter(Opponent == other[i])
      Won <- append(Won, length((t1 %>% filter(Winner == team()))$Winner))
      Lost <- append(Lost, length((t1 %>% filter(Winner == other[i]))$Winner))
    }
    
    d <- data.frame(Team, Won, Lost, Won/Lost)
    colnames(d)[4] <- "W_L Ratio"
    d$'W_L Ratio'[is.infinite(d$'W_L Ratio')] <- NA
    d$'W_L Ratio' <- as.double(d$'W_L Ratio')
    d %>% arrange(desc(d$'W_L Ratio'))
  })
  
  output$match_summary <- renderPrint({
    t <- match_table()
    t <- t %>% filter((year(`Match Date`) >= year_match_1()) & (year(`Match Date`) <= year_match_2()))
    colnames(t)[3] <- paste0(team_1(), " Score")
    colnames(t)[4] <- paste0(team_2(), " Score")
    colnames(t)[7] <- "Winning Score"
    colnames(t)[8] <- "Losing Score"
    t <- t[c(3, 4, 7, 8, 11)]
    summary(t)
  })
  
  output$player <- renderUI({
    selectInput("selectedplayer", "Select a Player:", choices = Player_name[[input$country]])
  }) 
  
  output$ballorbat <- renderUI({
    selectInput("selectb", "Batting or Bowlling :", choices = c("Batting" , "Bowling"))
  })
  
  
  
  
  output$img <- renderPlot({
    img <- image_read(image_links[[input$country]][[input$selectedplayer]])
    plot(img)
  })
  
  output$data <- renderTable({
    Player_Data[[input$country]][[input$selectedplayer]][[input$selectb]][[2]]
  })
  
  output$plot <- renderPlot({
    Player_Data[[input$country]][[input$selectedplayer]][[input$selectb]][[3]]
  })
  
  
  
  
  
  output$player_counter <- renderText({
    "Number of Player Selected"
  })
  
  output$no_of_player <- renderText( {
    length(input$player_names)
  })
  
  output$hint <- renderText({
    "Select 'new team' to create a team"
  })
  
  output$player_counter_2 <- renderText({
    "Number of Player Selected"
  })
  
  output$no_of_player_2 <- renderText( {
    length(input$player_names_2)
  })
  
  
  
  observeEvent(input$start, {
    if (input$team_select_1 == 'new_team'){
      pyr_1 <- input$player_names
      team_1 <- NULL
      for (pyr in pyr_1){
        team_1[[pyr]] <- All_Player_Data[[pyr]]
      }
    } else {
      pyr_1 <- Player_name[[input$team_select_1]]
      team_1 <- Player_Data[[input$team_select_1]]
    }
    
    if (input$team_select_2 == 'new_team'){
      pyr_2 <- input$player_names_2
      team_2 <- NULL
      for (pyr in pyr_2){
        team_2[[pyr]] <- All_Player_Data[[pyr]]
      }
    } else {
      pyr_2 <- Player_name[[input$team_select_2]]
      team_2 <- Player_Data[[input$team_select_2]]
    }
    
    output$team_output <- renderPrint({
      print(Cricket_Match(team_1,team_2,pyr_1,pyr_2))
    })
    
    
  })
 
  
 
}

shinyApp(ui = ui, server = server)
