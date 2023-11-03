library(shiny)
library(ggplot2)
library(png)
library(magick)
library(shinyjs)
library(shinyalert)
library(dplyr)
library(shinythemes)

load("../Cricket.RData")

ui <- fluidPage(theme = shinytheme("superhero"),
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
             
             #################
             ###########
    ),
  
    tabPanel("Cricket Match", 
             sidebarLayout(
               sidebarPanel(
                 span(textOutput("hint")),
                 selectInput("team_select_1", "Select team 1", 
                             choices = c(Country_name, "new_team (1)")),
                 conditionalPanel(
                   condition = "input.team_select_1 == 'new_team (1)'", 
                   selectInput("player_names", "Select Players (exect 11 players):", 
                               choices = All_Player_Name, multiple = TRUE),
                   span(textOutput("player_counter")),
                   uiOutput("no_of_player")
                 ),
                 selectInput("team_select_2", "Select team 2", 
                             choices = c(Country_name, "new_team (2)")),
                 conditionalPanel(
                   condition = "input.team_select_2 == 'new_team (2)'", 
                   selectInput("player_names_2", "Select Players (exect 11 players):", 
                               choices = All_Player_Name, multiple = TRUE),
                   span(textOutput("player_counter_2")),
                   uiOutput("no_of_player_2")
                 ),
                 actionButton("start", "Start Match")
               ),
               mainPanel(
                 h1(textOutput("winorloss",container = span)),
                 h2(textOutput("team1",container = span)),
                 h3(textOutput("batting",container = span)),
                 dataTableOutput("team1_table"),
                 plotOutput("team_1_plot_bat"),
                 h3(textOutput("bowling",container = span)),
                 dataTableOutput("team1_table_"),
                 plotOutput("team_1_plot_bal"),
                 
                 h2(textOutput("team2",container = span)),
                 h3(textOutput("batting_",container = span)),
                 dataTableOutput("team2_table"),
                 plotOutput("team_2_plot_bat"),
                 h3(textOutput("bowling_",container = span)),
                 dataTableOutput("team2_table_"),
                 plotOutput("team_2_plot_bal"),
                 
                  
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
    if (input$team_select_1 == 'new_team (1)'){
      pyr_1 <- input$player_names
      team_1 <- NULL
      for (pyr in pyr_1){
        team_1[[pyr]] <- All_Player_Data[[pyr]]
      }
    } else {
      pyr_1 <- Player_name[[input$team_select_1]]
      team_1 <- Player_Data[[input$team_select_1]]
    }
    
    if (input$team_select_2 == 'new_team (2)'){
      pyr_2 <- input$player_names_2
      team_2 <- NULL
      for (pyr in pyr_2){
        team_2[[pyr]] <- All_Player_Data[[pyr]]
      }
    } else {
      pyr_2 <- Player_name[[input$team_select_2]]
      team_2 <- Player_Data[[input$team_select_2]]
    }
    data <- Cricket_Match(team_1,team_2,pyr_1,pyr_2)
    
    
    output$team_1_plot_bat <- renderPlot({
      ggplot(data[[1]][[1]], aes(x = batsman_name, y = Runs, col = factor(four), size = six , shape = batsman_outer)) + 
        geom_point() + 
        labs(title ="Batsman Performance" ,x = "Batsman Name", y = "Total Runs", 
             shape = "Wicketeer", color = "No of Four", size = "No of Six")
    })
    
    output$team_2_plot_bat <- renderPlot({
      ggplot(data[[2]][[1]], aes(x = batsman_name, y = Runs, col = factor(four), size = six , shape = batsman_outer)) + 
        geom_point() + 
        labs(title ="Batsman Performance" ,x = "Batsman Name", y = "Total Runs", 
             shape = "Wicketeer", color = "No of Four", size = "No of Six")
    })
    
    output$team_1_plot_bal <- renderPlot({
      ggplot(data[[1]][[2]], aes(x = "", y = Runs, fill = Baller_name)) +
        geom_bar(width = 1, stat = "identity") +
        geom_text(aes(label = paste(Baller_name, "\n", "Wickets:", Wicket,"\n","Runs:" , Runs)), 
                  position = position_stack(vjust = 0.5), size = 3, vjust = 1) +
        coord_polar("y") +
        theme_void() +
        theme(legend.position = "none")
      
    })
    
    output$team_2_plot_bal <- renderPlot({
      ggplot(data[[2]][[2]], aes(x = "", y = Runs, fill = Baller_name)) +
        geom_bar(width = 1, stat = "identity") +
        geom_text(aes(label = paste(Baller_name, "\n", "Wickets:", Wicket,"\n","Runs:" , Runs)), 
                  position = position_stack(vjust = 0.5), size = 3, vjust = 1) +
        coord_polar("y") +
        theme_void() +
        theme(legend.position = "none")
      
    })
    
   
    output$team1 <- renderText({
      paste("First inning",":")
    })
    
    output$batting <- renderText({
      paste("Batting :",input$team_select_1)
    })
    output$team1_table <- renderDataTable({
     
      data[[1]][[1]]
    })
   
    output$bowling <- renderText({
      paste("Bowling :",input$team_select_2)
    })
    output$team1_table_ <- renderDataTable({
      
      data[[1]][[2]]
    })
    
    output$team2 <- renderText({
      paste("Second inning",":")
    })
    
    output$batting_ <- renderText({
      paste("Batting :",input$team_select_2)
    })
    output$team2_table <- renderDataTable({
      
      data[[2]][[1]]
    })
    
    output$bowling_ <- renderText({
      paste("Bowling :",input$team_select_1)
    })
    output$team2_table_ <- renderDataTable({
      
      data[[2]][[2]]
    })

    
    result <- substr(data[[3]],1,6)
    res <- substr(data[[3]],7,55)
    if(result == "Team 1"){
      output$winorloss <- renderText({
        paste(input$team_select_1,res)
      })
    } else {
      output$winorloss <- renderText({
        paste(input$team_select_2,res)
      })
    }
    
  })
 
  
 
}

shinyApp(ui = ui, server = server)
