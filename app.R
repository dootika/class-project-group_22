library(shiny)
library(ggplot2)
library(png)
library(magick)
library(shinyjs)
library(shinyalert)

load("../Cricket.RData")

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
             # UI components specific to Team ODI Data tab
             # Add your UI elements here
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
                               choices = All_Player_Name, multiple = TRUE),
                   span(textOutput("player_counter")),
                   uiOutput("no_of_player")
                 ),
                 selectInput("team_select_2", "Select team 2", 
                             choices = c(Country_name, "new_team")),
                 conditionalPanel(
                   condition = "input.team_select_2 == 'new_team'", 
                   selectInput("player_names_2", "Select Players (exect 11 players):", 
                               choices = All_Player_Name, multiple = TRUE),
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
