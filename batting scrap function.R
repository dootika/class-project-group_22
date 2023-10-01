library(rvest)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)


Scrap_Bating_data <- function(name){
  name1 <- gsub(pattern = " ", replacement = "+", name )
  link <- paste0("http://www.cricmetric.com/playerstats.py?player=",name1,"&role=batsman&format=ODI&groupby=year")

  data <- read_html(link) %>%
    html_table()
  Data <- data[[1]]
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
  output <- list(length(3))
  output[[1]] <- x
  output[[2]] <- Data
  output[[3]] <- ggp
  return(output)
  
}
