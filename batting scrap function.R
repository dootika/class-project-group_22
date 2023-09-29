library(rvest)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)


Scrap_Bating_data <- function(link ,N){
  name <- read_html(link) %>% html_element(".panel-heading") %>% html_text2()
  data <- read_html(link) %>%
    html_table()
  Data <- data[N][[1]]
  row_number <-dim(Data)[1]
  Data$Runs <-  gsub(pattern = ",",replacement = "",x = Data$Runs)
  Data$Balls <- gsub(pattern = ",",replacement = "",x = Data$Balls)
  Data$'4s' <- gsub(pattern = ",",replacement = "",x = Data$'4s')
  
  
  
  Data$Innings <- as.numeric(Data$Innings)
  Data$Runs <- as.numeric(Data$Runs)
  Data$Balls <- as.numeric(Data$Balls)
  Data$Outs <- as.numeric(Data$Outs)
  Data$Avg <- as.numeric(Data$Avg)
  Data$SR <- as.numeric(Data$SR)
  Data$HS <- as.numeric(Data$HS)
  Data$'50' <- as.numeric(Data$'50')
  Data$'100' <- as.numeric(Data$'100')
  Data$'4s' <- as.numeric(Data$'4s')
  Data$'6s' <- as.numeric(Data$'6s')
  Data1 <- Data
  Data <- Data[-c(row_number),]
  
  Main <- Data %>%
    select(Year ,Runs ,Balls, Innings , Outs) %>%
    summarise(Year = Year ,
              Rate = round({Runs/Balls}*6, 2),
              Runs_per_Inning =Runs/Innings  , 
              Balls_per_Inning = Balls/Innings,
              Not_Out_Rate = {Innings-Outs}/Innings)
  
  xy <- Main[,c(1,3,4)]
  xy <- melt(xy,"Year")
  text <- Main$Rate
  text <- append(text,Main$Not_Out_Rate)
  text <- round(text,3)
  Data5 <- tibble(xy,text)
  ggp <- ggplot(Data5 , aes(x = Year , y = value , fill = variable )) + 
    geom_bar(stat = "identity", width = 0.5,position = position_dodge(width = 0.7)) + 
    geom_text(aes(label=text), vjust=1.5, colour="black", size=3) +
    labs(x = "Years" , y = "Number of Balls Or Runs ", title =paste0(name,"'s Performance") )
  
  plot(ggp)
  xz  <- Data1[c(row_number),]
  Fours <- as.numeric(xz[1,11]/xz[1,4])
  Sixs <- as.numeric(xz[1,12]/xz[1,4])
  
  Fo <- as.numeric(xz[1,11])*4
  Six <- as.numeric(xz[1,12])*6
  Tot <- as.numeric(xz[1,3])
  ToB <- as.numeric(xz[1,4])
  pro <- {Tot-(Fo+Six)}/6
  
  Ones <- pro*3/ToB
  Twos <-  pro*2/ToB
  Threes <-  pro/ToB
  zeros <- 1-Ones - Twos - Threes - Fours - Sixs
  x <- NULL
  x[1] <- zeros
  x[2] <- Ones
  x[3] <- Twos
  x[4] <- Threes
  x[5] <- Fours
  x[6] <-Sixs
  return(x)
}
x <- Scrap_Bating_data("http://www.cricmetric.com/playerstats.py?player=RG+Sharma&role=all&format=all&groupby=year",3)
































# bagladesh bating 
Bagladesh_batting <- list(length(11))
Bagladesh_batting[[1]] <-  Scrap_Bating_data(link = "http://www.cricmetric.com/playerstats.py?player=Shakib+Al+Hasan&role=all&format=all&groupby=year", 17 ,3,"Shakib Al Hasan")
Bagladesh_batting[[2]] <- Scrap_Bating_data(link = "http://www.cricmetric.com/playerstats.py?player=Mehidy+Hasan+Miraz&role=all&format=all&groupby=year",row_number = 8,3,name = "Mehidy Hasan")
Bagladesh_batting[[3]] <- Scrap_Bating_data(link = "http://www.cricmetric.com/playerstats.py?player=Nurul+Hasan&role=all&format=all&groupby=year",row_number = 5,3,name = "Nurul Hasan")
Bagladesh_batting[[4]] <- Scrap_Bating_data(link = "http://www.cricmetric.com/playerstats.py?player=Najmul+Hossain+Shant&role=all&format=all&groupby=year",6,3,"Najmul Hossain Shant")
Bagladesh_batting[[5]] <- Scrap_Bating_data(link = "http://www.cricmetric.com/playerstats.py?player=Mosaddek+Hossain&role=all&format=all&groupby=year",7,3,name = "Mosaddek Hossain")
Bagladesh_batting[[6]] <- Scrap_Bating_data(link = "http://www.cricmetric.com/playerstats.py?player=Yasir+Ali&role=all&format=all&groupby=year",3,3,name = "Yasir Ali")
Bagladesh_batting[[7]] <- Scrap_Bating_data(link = "http://www.cricmetric.com/playerstats.py?player=Sabbir+Rahman&role=all&format=all&groupby=year",7,3,"Sabbir Rahman")
Bagladesh_batting[[8]] <- Scrap_Bating_data(link = "http://www.cricmetric.com/playerstats.py?player=Mustafizur+Rahman&role=all&format=all&groupby=year",9,3,"Mustafizur+Rahman")
Bagladesh_batting[[9]] <- Scrap_Bating_data("http://www.cricmetric.com/playerstats.py?player=Taskin+Ahmed&role=all&format=all&groupby=year",8,3,"Taskin+Ahmed")
Bagladesh_batting[[10]] <- Scrap_Bating_data("http://www.cricmetric.com/playerstats.py?player=Afif+Hossain&role=all&format=all&groupby=year",5,1,"Afif Hossai")
Bagladesh_batting[[11]] <- Scrap_Bating_data("http://www.cricmetric.com/playerstats.py?player=Hasan+Mahmud&role=all&format=all&groupby=year",4,1,"Hasan Mahmud")
