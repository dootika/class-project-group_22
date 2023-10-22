#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(shiny)
library(ggpubr)
load("../Teams_Table.Rdata")
#runExample("06_tabsets")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ODI Teams"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(
              inputId = "y",
              label = "Years : ",
              min = min(year(table$`Match Date`)),
              max = max(year(table$`Match Date`)),
              value = c(1980, 2020)
            ),
            checkboxGroupInput(
              inputId = "teams",
              label = "Teams : ",
              choices = unique(table$`Team 1`),
              selected = "Australia"
            ),
            checkboxGroupInput(
              inputId = "code",
              label = "Colour codedoded on : ",
              choices = c("Win_Loss", "Teams"),
              selected = c("Win_Loss", "Teams")
            ),
            radioButtons(
              inputId = "exc",
              label = ("Exclusive Teams : "),
              choices = c("Yes", "No"),
              selected = "No"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           verbatimTextOutput("tab1"),
           verbatimTextOutput("tab2"),
           textOutput("meh"),
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  y1 <- reactive({
    t <- input$y
    t[1]
  })
  y2 <- reactive({
    t <- input$y
    t[2]
  })
  teams <- reactive({
    input$teams
  })
  code <- reactive({
    input$code
  })
  exc <- reactive({
    input$exc
  })
  output$meh <- renderText({
    length(code())
  })
  output$tab1 <- renderPrint({
    t1 <- NULL
    if (exc() == "No") {
      t1 <- table %>% filter(`Team 1` %in% teams() | `Team 2` %in% teams()) %>% filter(as.integer(year(`Match Date`)) >= as.integer(y1())) %>% filter(as.integer(year(`Match Date`)) <= as.integer(y2()))
    }
    if (exc() == "Yes") {
      t1 <- table %>% filter((`Team 1` %in% teams()) & (`Team 2` %in% teams())) %>% filter(as.integer(year(`Match Date`)) >= as.integer(y1())) %>% filter(as.integer(year(`Match Date`)) <= as.integer(y2()))
    }
#    t1 <- table %>% filter(`Team 1` %in% teams() | `Team 2` %in% teams()) %>% filter(as.integer(year(`Match Date`)) >= as.integer(y1())) %>% filter(as.integer(year(`Match Date`)) <= as.integer(y2()))
    summary(t1)
  })
  output$tab2 <- renderPrint({
    t1 <- NULL
    if (exc() == "No") {
      t1 <- table %>% filter(`Team 1` %in% teams() | `Team 2` %in% teams()) %>% filter(as.integer(year(`Match Date`)) >= as.integer(y1())) %>% filter(as.integer(year(`Match Date`)) <= as.integer(y2()))
    }
    if (exc() == "Yes") {
      t1 <- table %>% filter((`Team 1` %in% teams()) & (`Team 2` %in% teams())) %>% filter(as.integer(year(`Match Date`)) >= as.integer(y1())) %>% filter(as.integer(year(`Match Date`)) <= as.integer(y2()))
    }
    Team <- NULL
    for (t in teams()) {
      tw <- t1 %>% filter(Winner == t)
      tl <- t1 %>% filter(Loser == t)
      Win <- NULL
      Win_Score <- tw$Winner_Score
      Loss_Score <- tl$Loser_Score
      Score <- append(Win_Score, Loss_Score)
      for (i in 1:length(Win_Score)) {
        Win <- append(Win, "Won")
      }
      for (i in 1:length(Loss_Score)) {
        Win <- append(Win, "Lost")
      }
      Date <- append(tw$`Match Date`, tl$`Match Date`)
      Name <- NULL
      for (i in 1:(length(Win_Score) + length(Loss_Score))) {
        Name <- append(Name, t)
      }
      dataset <- data.frame(Name, Score, Win, Date)
      Team <- rbind(Team, dataset)
    }
    Team <- Team %>% arrange(Date)
    summary(Team)
  })
  # output$plot <- renderPlot({
  #   if (exc() == "No") {
  #     t1 <- table %>% filter(`Team 1` %in% teams() | `Team 2` %in% teams()) %>% filter(as.integer(year(`Match Date`)) >= as.integer(y1())) %>% filter(as.integer(year(`Match Date`)) <= as.integer(y2()))
  #   }
  #   if (exc() == "Yes") {
  #     t1 <- table %>% filter((`Team 1` %in% teams()) & (`Team 2` %in% teams())) %>% filter(as.integer(year(`Match Date`)) >= as.integer(y1())) %>% filter(as.integer(year(`Match Date`)) <= as.integer(y2()))
  #   }    
  #   scores <- NULL
  #   Win <- NULL
  #   dates <- NULL
  #   for (i in 1:length(t1$Winner_Score)) {
  #     scores <- append(scores, t1$Winner_Score[i])
  #     scores <- append(scores, t1$Loser_Score[i])
  #     Win <- append(Win, "Won")
  #     Win <- append(Win, "Lost")
  #     dates <- append(dates, t1$`Match Date`[i])
  #     dates <- append(dates, t1$`Match Date`[i])
  #   }
  #   t2 <- data.frame(dates, scores, Win)
  #   p1 <- ggplot(t2, aes(dates, scores, colour = Win)) + geom_point()
  #   t3 <- t1 %>% filter(Winner %in% teams())
  #   p2 <- ggplot(t3, aes(`Match Date`, Winner_Score, colour = Winner)) + geom_point()
  #   if (("Win_Loss" %in% code()) & length(code()) == 1) {
  #     ggplot(t2, aes(dates, scores, colour = Win)) + geom_point()
  #   }
  #   if (("Teams" %in% code()) & length(code()) == 1) {
  #     p2
  #     ggarrange(p2, ncol = 1, nrow = 1)
  #   }
  #   if (length(code()) == 2) {
  #     ggarrange(p1, p2, ncol = 1, nrow = 2, vjust = 20, align = "v")
  #   }
  # })
  output$plot <- renderPlot({
    t1 <- NULL
    if (exc() == "No") {
      t1 <- table %>% filter(`Team 1` %in% teams() | `Team 2` %in% teams()) %>% filter(as.integer(year(`Match Date`)) >= as.integer(y1())) %>% filter(as.integer(year(`Match Date`)) <= as.integer(y2()))
    }
    if (exc() == "Yes") {
      t1 <- table %>% filter((`Team 1` %in% teams()) & (`Team 2` %in% teams())) %>% filter(as.integer(year(`Match Date`)) >= as.integer(y1())) %>% filter(as.integer(year(`Match Date`)) <= as.integer(y2()))
    }
    Team <- NULL
    for (t in teams()) {
      tw <- t1 %>% filter(Winner == t)
      tl <- t1 %>% filter(Loser == t)
      Win <- NULL
      Win_Score <- tw$Winner_Score
      Loss_Score <- tl$Loser_Score
      Score <- append(Win_Score, Loss_Score)
      for (i in 1:length(Win_Score)) {
        Win <- append(Win, "Won")
      }
      for (i in 1:length(Loss_Score)) {
        Win <- append(Win, "Lost")
      }
      Date <- append(tw$`Match Date`, tl$`Match Date`)
      Name <- NULL
      for (i in 1:(length(Win_Score) + length(Loss_Score))) {
        Name <- append(Name, t)
      }
      dataset <- data.frame(Name, Score, Win, Date)
      Team <- rbind(Team, dataset)
    }
    Team <- Team %>% arrange(Date)
    p1 <- ggplot(Team, aes(Date, Score, colour = Win)) + geom_point()
    p2 <- ggplot(Team, aes(Date, Score, colour = Name)) + geom_point()
    if (("Win_Loss" %in% code()) & length(code()) == 1) {
      ggplot(Team, aes(Date, Score, colour = Win)) + geom_point()
    }
    if (("Teams" %in% code()) & length(code()) == 1) {
      ggplot(Team, aes(Date, Score, colour = Name)) + geom_point()
    }
    if (length(code()) == 2) {
      ggarrange(p1, p2, ncol = 1, nrow = 2, vjust = 20, align = "v")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
