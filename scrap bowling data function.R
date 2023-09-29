library(rvest)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)

Scrap_Bowling_data <- function(link ,N){
  name <- read_html(link) %>% html_element(".panel-heading") %>% html_text2()
  data <- read_html(link) %>%
    html_table()
  Data <- data[N][[1]]
  row_number <- row_number <-dim(Data)[1]
  Data$Runs <-  gsub(pattern = ",",replacement = "",x = Data$Runs)
  Data$Overs <- gsub(pattern = ",",replacement = "",x = Data$Overs)
  Data$'4s' <- gsub(pattern = ",",replacement = "",x = Data$'4s')
  
  Data$Innings <- as.numeric(Data$Innings)
  Data$Runs <- as.numeric(Data$Runs)
  Data$Overs <- as.numeric(Data$Overs)
  Data$Wickets <- as.numeric(Data$Wickets)
  Data$Avg <- as.numeric(Data$Avg)
  Data$SR <- as.numeric(Data$SR)
  Data$'4s' <- as.numeric(Data$'4s')
  Data$'6s' <- as.numeric(Data$'6s')
  Data1 <- Data
  Data <- Data[-c(row_number),]
  Main <- Data %>%
    select(Year ,Runs ,Overs, Innings , Wickets) %>%
    summarise(Year = Year ,
              Runs_per_Inning = round({Runs/Innings},3),
              Wickets_per_Inning_X10 =Wickets/Innings *10 , 
              )
  xy <- Main[,c(1,2,3)]
  xy <- melt(xy,"Year")
  
  ggp <- ggplot(xy , aes(x = Year , y = value , fill = variable )) + 
    geom_bar(stat = "identity", width = 0.5,position = position_dodge(width = 0.7)) + 
    labs(x = "Years" , y = "Number of Wickets Or Runs Per Year ", title =paste0(name,"'s Performance") )
  
  plot(ggp)
  xz  <- Data1[c(row_number),]
  some <- as.numeric(xz[1,5]/xz[1,3])
  
  return(some/6)
}


bowler <- Scrap_Bowling_data("http://www.cricmetric.com/playerstats.py?player=HH+Pandya&role=all&format=all&groupby=year",4)




















# Shri lank bowlers 
Shrilanka_Bowlers <- list(length(5))
Shrilanka_Bowlers[[1]] <- Scrap_Bowling_data("http://www.cricmetric.com/playerstats.py?player=PWH+de+Silva&role=all&format=all&groupby=year",8,4,"Wanindu Hasaranga")
Shrilanka_Bowlers[[2]] <- Scrap_Bowling_data("http://www.cricmetric.com/playerstats.py?player=C+Karunaratne&role=all&format=all&groupby=year",4,4,"Chamika Karunaratne")
Shrilanka_Bowlers[[3]] <- Scrap_Bowling_data("http://www.cricmetric.com/playerstats.py?player=M+Theekshana&role=all&format=all&groupby=year",4,4,"Maheesh Theekshana")
Shrilanka_Bowlers[[4]] <- Scrap_Bowling_data("http://www.cricmetric.com/playerstats.py?player=DN+Wellalage&role=all&format=all&groupby=year",3,4,"Dunith Wellalage")
Shrilanka_Bowlers[[5]] <- Scrap_Bowling_data("http://www.cricmetric.com/playerstats.py?player=D+Madushanka&role=all&format=all&groupby=year",2,4,"Dilshan Madushanka")
























































ch <- rep(1:5,10)

# match start Now 
# tossing a coin
Coin_Toss <- sample(c("Head","Tail"), size = 1,replace = TRUE)
if(Coin_Toss == "Head"){
  # here coin toss is head so assume team one chosse to Bat
  Runs <- 0
  ind <- 1
  # frist over starts here 
  some <- ch[i]
   
    for(i in 1:50){
      while (ind != 11) {
        for (j in 1:6){
          #  Chosing a sample from out or not out  according to the bolwers previous performance 
          sam <- sample(c("Out", "Not_Out"), 1,replace = TRUE, prob =c(Shrilanka_Bowlers[some][[1]],1-Shrilanka_Bowlers[some][[1]]))
          
          if ( sam == "Not_Out"){
            # if batsman is not out then we take sample about the runs 
            run <- sample(c(0,1,2,3,4,6),1,replace = TRUE,prob = Bagladesh_batting[ind][[1]])
            print(run)
            Runs <- Runs + run
          } else {
            print("out")
            ind <- ind + 1
            
          }
        }
      }
    }
  
}

