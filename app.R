library(shiny)
library(ggplot2)
library(png)
library(magick)
library(shinyjs)
library(shinyalert)
library(dplyr)
library(shinythemes)
library(gt)

load("Cricket.RData")
load("Cricket_Match.RData")
load("Teams_Table.RData")
load("Cricket_Coord.RData")
happy <- read.csv("Happiness_Index.csv")
gdp <- read.csv("gdp_per_capita.csv")

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
                            plotOutput("world"),
                            tableOutput("world_table")
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
                            gt_output("h2h")
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
               ),
               tabPanel("Happiness Index",
                        tabsetPanel(
                          tabPanel("General",
                                   sidebarLayout(
                                     sidebarPanel(
                                       checkboxGroupInput(inputId = "teams_happy_general", label = "Countries",
                                                          choices = unique(table$Loser)[unique(table$Loser) %in% unique(happy$Country)],
                                                          selected = c("Australia", "Pakistan", "New Zealand", "India", "Sri Lanka", "Zimbabwe", "Bangladesh", "South Africa", "Netherlands", "Ireland", "Afghanistan"))
                                     ),
                                     mainPanel(
                                       plotOutput("bar_happy")
                                     )
                                   )
                          ),
                          tabPanel("Country",
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput("team_happy", "Select Team : ",
                                                   choices = unique(table$Loser)[unique(table$Loser) %in% unique(happy$Country)],
                                                   selected = "India")
                                     ),
                                     mainPanel(
                                       plotOutput("happy_plot")
                                     )
                                   ))
                        )
               ),
               tabPanel("GDP",
                        tabsetPanel(
                          tabPanel("General",
                                   sidebarLayout(
                                     sidebarPanel(
                                       checkboxGroupInput(inputId = "teams_gdp_general", label = "Countries",
                                                          choices = unique(table$Loser)[unique(table$Loser) %in% unique(gdp$Country.Name)],
                                                          selected = c("Australia", "Pakistan", "New Zealand", "India", "Sri Lanka", "Zimbabwe", "Bangladesh", "South Africa", "Netherlands", "Kenya", "Ireland", "Afghanistan"))
                                     ),
                                     mainPanel(
                                       plotOutput("bar_gdp")
                                     )
                                   )
                          ),
                          tabPanel("Country",
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput("team_gdp", "Select Team : ",
                                                   choices = unique(table$Loser)[unique(table$Loser) %in% unique(gdp$Country.Name)],
                                                   selected = "India")
                                     ),
                                     mainPanel(
                                       plotOutput("gdp_plot")
                                     )
                                   ))
                        )
               )
             )
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
  team <- reactive({
    input$team
  })
  
  team_1 <- reactive({
    input$team1
  })
  
  team_2 <- reactive({
    input$team2
  })
  
  team_happy <- reactive({
    input$team_happy
  })
  
  team_gdp <- reactive({
    input$team_gdp
  })
  
  team_table <- reactive({
    team_filter(team())
  })
  
  teams_gdp_general <- reactive({
    input$teams_gdp_general
  })
  
  teams_happy_general <- reactive({
    input$teams_happy_general
  })
  
  team_1_table <- reactive({
    team_filter(team_1())
  })
  
  match_table <- reactive({
    team_1_table() %>% filter(Opponent == team_2())
  })
  
  team_happy_table <- reactive({
    team_filter(team_happy())
  })
  
  y_world_1 <- reactive({
    t <- input$y_world
    t[1]
  })
  
  y_world_2 <- reactive({
    t <- input$y_world
    t[2]
  })
  
  output$world_table <- renderTable({
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
    colnames(WL)[1] <- "Teams"
    WL %>% arrange(desc(W_L)) %>% filter(!is.na(W_L))
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
  
  output$years_happy <- renderUI({
    sliderInput(
      inputId = "yh",
      label = "Years : ",
      min = max(min(year(team_happy_table()$`Match Date`)), min((happy %>% filter(Country == team_happy()))$Year)),
      max = min(max(year(team_happy_table()$`Match Date`)), max((happy %>% filter(Country == team_happy()))$Year)),
      value = c(max(min(year(team_happy_table()$`Match Date`)), min((happy %>% filter(Country == team_happy()))$Year)), min(max(year(team_happy_table()$`Match Date`)), max((happy %>% filter(Country == team_happy()))$Year))))
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
  
  years_happiness <- reactive({
    input$yh
  })
  
  output$team_summary <- renderPrint({
    t <- team_table()
    t1 <- (t %>% filter((as.integer(year(t$'Match Date')) >= year_team_1()) & (as.integer(year(t$'Match Date')) <= year_team_2())))
    colnames(t1)[3] <- paste0(team(), " Score")
    colnames(t1)[4] <- "Opponent Score"
    colnames(t1)[7] <- "Winning Score"
    colnames(t1)[8] <- "Losing Score"
    t1 <- t1[c(3, 4, 7, 8, 12)]
    summary(t1)
  })
  
  output$h2h <- render_gt({
    t2 <- team_table()
    t <- t2 %>% filter((as.integer(year(t2$'Match Date')) >= year_team_1()) & (as.integer(year(t2$'Match Date')) <= year_team_2()))
    Team <- unique(t$Opponent)
    other <- Team
    Won <- NULL
    Lost <- NULL
    Won_Bat <- NULL
    Won_Bowl <- NULL
    Lost_Bat <- NULL
    Lost_Bowl <- NULL
    for (i in 1:length(other)) {
      t1 <- t %>% filter(Opponent == other[i])
      Won <- append(Won, length((t1 %>% filter(Winner == team()))$Winner))
      Won_Bat <- append(Won_Bat, length((t1 %>% filter((Winner == team()) & (Winning_Innings == "Batting")))$Winner))
      Won_Bowl <- append(Won_Bowl, length((t1 %>% filter((Winner == team()) & (Winning_Innings == "Bowling")))$Winner))
      Lost_Bat <- append(Lost_Bat, length((t1 %>% filter((Winner == other[i]) & (Winning_Innings == "Bowling")))$Winner))
      Lost_Bowl <- append(Lost_Bowl, length((t1 %>% filter((Winner == other[i]) & (Winning_Innings == "Batting")))$Winner))
      Lost <- append(Lost, length((t1 %>% filter(Winner == other[i]))$Winner))
    }
    
    d <- data.frame(Team, Won_Bat, Lost_Bat, Won_Bowl, Lost_Bowl, Won/Lost)
    colnames(d)[6] <- "W_L Ratio"
    d$'W_L Ratio'[is.infinite(d$'W_L Ratio')] <- NA
    d$'W_L Ratio' <- as.double(d$'W_L Ratio')
    d <- d %>% arrange(desc(d$'W_L Ratio'))
    colnames(d)[6] <- "Win to Loss Ratio"
    d <- gt(d)
    tab <- tab_spanner(d, label = "Batting", columns = ends_with("Bat"))
    tab <- tab_spanner(tab, label = "Chasing", columns = ends_with("Bowl"))
    tab |> cols_label(starts_with("Won") ~ "Won", starts_with("Lost") ~ "Lost")
  })
  
  output$match_summary <- renderPrint({
    t <- match_table()
    t <- t %>% filter((year(`Match Date`) >= year_match_1()) & (year(`Match Date`) <= year_match_2()))
    colnames(t)[3] <- paste0(team_1(), " Score")
    colnames(t)[4] <- paste0(team_2(), " Score")
    colnames(t)[7] <- "Winning Score"
    colnames(t)[8] <- "Losing Score"
    t <- t[c(3, 4, 7, 8, 12)]
    summary(t)
  })
  
  output$happy_plot <- renderPlot({
    y1 <- happy %>% filter(Country == team_happy())
    y2 <- team_filter(team_happy())
    Year <- NULL
    for (i in unique(y1$Year)) {
      if (i %in% unique(year(y2$'Match Date'))) {
        Year <- append(Year, i)
      }
    }
    Win_To_Loss <- NULL
    Happiness <- NULL
    for (i in Year) {
      hap <- y1$Index[which(y1$Year == i, arr.ind = TRUE)]
      Happiness <- append(Happiness, as.double(hap))
      Win <- length((y2 %>% filter(Winner == team_happy()) %>% filter(year(`Match Date`) == i))$Winner)
      Loss <- length((y2 %>% filter(Loser == team_happy()) %>% filter(year(`Match Date`) == i))$Loser)
      if (Loss == 0) {
        Win_To_Loss <- append(Win_To_Loss, Win)
      }
      else {
        Win_To_Loss <- append(Win_To_Loss, Win/Loss)
      }
    }
    matplot(Year, cbind(Happiness, Win_To_Loss), col = c("red", "blue"), pch = c(13, 16), xlab = "Year", ylab = "Happiness Index + Win To Loss Ratio")
    legend("topright", legend = c("Happiness Index", "Win To Loss Ratio"), col = c("red", "blue"), pch = c(13, 16))
  })
  
  output$bar_happy <- renderPlot({
    #    Countries <- unique(table$Loser)[unique(table$Loser) %in% unique(gdp$Country.Name)]
    #    Countries <- c("Australia", "Pakistan", "New Zealand", "India", "Sri Lanka", "Zimbabwe", "Bangladesh", "South Africa", "Netherlands", "Kenya", "Ireland", "Afghanistan")
    Countries <- teams_happy_general()
    Correlation <- NULL
    for (c in Countries) {
      tab <- happy %>% filter(Country == c)
      Tf <- team_filter(c)
      y_team <- unique(year(Tf$'Match Date'))
      Year <- NULL
      for (i in unique(tab$Year)) {
        if (i %in% unique(year(Tf$'Match Date'))) {
          Year <- append(Year, i)
        }
      }
      Happiness <- NULL
      Win_To_Loss <- NULL
      for (i in Year) {
        hap <- tab$Index[which(tab$Year == i, arr.ind = TRUE)]
        Happiness <- append(Happiness, as.double(hap))
        Win <- length((Tf %>% filter(Winner == c) %>% filter(year(`Match Date`) == i))$Winner)
        Loss <- length((Tf %>% filter(Loser == c) %>% filter(year(`Match Date`) == i))$Loser)
        if (Loss == 0) {
          Win_To_Loss <- append(Win_To_Loss, Win)
        }
        else {
          Win_To_Loss <- append(Win_To_Loss, Win/Loss)
        }
      }
      Correlation <- append(Correlation, cor(Win_To_Loss, Happiness))
    }
    barplot(names = Countries, height = Correlation, xlab = "Countries", ylab = "Correlation")
  })
  
  output$gdp_plot <- renderPlot({
    t1 <- team_gdp()
    tab <- gdp %>% filter(Country.Name == t1)
    Tf <- team_filter(t1)
    y_team <- unique(year(Tf$'Match Date'))
    Years <- NULL
    for (i in 1960:2020) {
      if (!is.na(tab[i - 1957] == "NA") & (as.integer(i) %in% y_team)) {
        Years <- append(Years, as.integer(i))
      }
    }
    GDP <- NULL
    Win_To_Loss <- NULL
    for (i in Years) {
      GDP <- append(GDP, as.integer(tab[i - 1957]))
      Win <- length((Tf %>% filter(Winner == t1) %>% filter(year(`Match Date`) == i))$Winner)
      Loss <- length((Tf %>% filter(Loser == t1) %>% filter(year(`Match Date`) == i))$Loser)
      if (Loss == 0) {
        Win_To_Loss <- append(Win_To_Loss, Win)
      }
      else {
        Win_To_Loss <- append(Win_To_Loss, Win/Loss)
      }
    }
    # plot(Years, GDP, pch = 16, col = "red")
    # points(Years, GDP, pch = 12, col = "blue")
    # lines(Years, GDP, pch = 12, col = "blue")
    matplot(Years, cbind(GDP, 1000*Win_To_Loss), col = c("red", "blue"), pch = c(13, 16), xlab = "Years", ylab = "GDP + Win To Loss Ratio(x 1000)")
    legend("topright", legend = c("GDP per Capita", "Win To Loss Ratio(x 1000)"), col = c("red", "blue"), pch = c(13, 16))
  })
  
  output$bar_gdp <- renderPlot({
    #    Countries <- unique(table$Loser)[unique(table$Loser) %in% unique(gdp$Country.Name)]
    #    Countries <- c("Australia", "Pakistan", "New Zealand", "India", "Sri Lanka", "Zimbabwe", "Bangladesh", "South Africa", "Netherlands", "Kenya", "Ireland", "Afghanistan")
    Countries <- teams_gdp_general()
    Correlation <- NULL
    for (c in Countries) {
      tab <- gdp %>% filter(Country.Name == c)
      Tf <- team_filter(c)
      y_team <- unique(year(Tf$'Match Date'))
      Years <- NULL
      for (i in 1960:2020) {
        if (!is.na(tab[i - 1957] == "NA") & (as.integer(i) %in% y_team)) {
          Years <- append(Years, as.integer(i))
        }
      }
      GDP <- NULL
      Win_To_Loss <- NULL
      for (i in Years) {
        GDP <- append(GDP, as.integer(tab[i - 1957]))
        Win <- length((Tf %>% filter(Winner == c) %>% filter(year(`Match Date`) == i))$Winner)
        Loss <- length((Tf %>% filter(Loser == c) %>% filter(year(`Match Date`) == i))$Loser)
        if (Loss == 0) {
          Win_To_Loss <- append(Win_To_Loss, Win)
        }
        else {
          Win_To_Loss <- append(Win_To_Loss, Win/Loss)
        }
      }
      Correlation <- append(Correlation, cor(Win_To_Loss, GDP))
    }
    barplot(names = Countries, height = Correlation, xlab = "Countries", ylab = "Correlation")
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
