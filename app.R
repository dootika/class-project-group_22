
library(shiny)
library(ggplot2)
library(png)

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
}

shinyApp(ui = ui, server = server)