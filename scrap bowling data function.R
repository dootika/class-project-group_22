Scrap_Bowling_data <- function(name){
  name <- gsub(pattern = " ", replacement = "+", name )
  link <- paste0("http://www.cricmetric.com/playerstats.py?player=",name,"&role=bowler&format=ODI&groupby=year")
  # name <- read_html(link) %>% html_element(".panel-heading") %>% html_text2()
  data <- read_html(link) %>%
    html_table()
  Data <- data[[1]]
  row_number <- dim(Data)[1]
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
  
 
  xz  <- Data1[c(row_number),]
  some <- as.numeric(xz[1,5]/xz[1,3])
  output <- list(length(3))
  output[[1]] <- some/6
  output[[2]] <- Data
  output[[3]] <- ggp
  return(output)
}
