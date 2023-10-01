Cricket_Match <- function(Batting_1 ,Bowling_1,Batting_2, Bowling_2 ){
  names <- c(deparse(substitute(Batting_1)), deparse(substitute(Batting_2)))
  x <- strsplit(x = names , split = "_", fixed = TRUE)
  x <- sapply(x,"[[",1)
  Team_1 <- x[1]
  Team_2 <- x[2]
  # There are 50 Overs and at first Runs are zero And I take Some variable as 
  # ind equal to one to define which batsman is playing and it will be used to stop the match
  Order <- rep(1:5,10)
  # Order is defined to deside which Bowler will Ball
  Runs <- 0
  Ind <- 1
  # Optional : i want to store data of every over so i create a list of length 50
  Over_Data <- list(length(50))
  for(i in 1:50){
    if(Ind == 11){
      break
    }
    Or <- Order[i]
    # i will define the balls in a over 
    # Optional: I will create a null variable to store over data 
    Over <- NULL
    for(j in 1:6){
      if(Ind == 11){
        break
      }
      # I decided that Out and Not Out is in Bowlers Hand and Runs are in Batsman Hand So 
      # I will take a sample to first deside that it is out or not and call is as a Sam
      Sam <- sample(c("Out","Not_Out"),1,replace = TRUE,prob = c(Bowling_2[Or][[1]],1-Bowling_2[Or][[1]]))
      # If Sam is Not out then we will decide about the runs
      if(Sam == "Not_Out"){
        Run <- sample(c(0,1,2,3,4,6),1,replace = TRUE,prob = Batting_1[Ind][[1]])
        Runs <- Runs + Run
        Over[j] <- Run
       
        if(Run == 1 || Run == 3){
          Runner <- Batting_1[Ind]
          Facer <- Batting_1[Ind + 1]
          Batting_1[Ind + 1] <- Runner
          Batting_1[Ind] <- Facer
        } 
      } else {
        # if player is out then next player will come to bat
        Ind <- Ind + 1
        Over[j] <- "Out"
      }
      # Now when over completes the Runner and Batter will interchange and save the over data 
      if (j == 6 ){
        Runner <- Batting_1[Ind]
        Facer <- Batting_1[Ind + 1]
        Batting_1[Ind + 1] <- Runner
        Batting_1[Ind] <- Facer
        Over_Data[[i]] <- Over
      }
    }
  }
  output_1 <- list(length(2))
  output_1[[1]] <- Runs
  output_1[[2]] <- Over_Data
  
  
  
  
  #########################
  {
    # There are 50 Overs and at first Runs are zero And I take Some variable as 
    # ind equal to one to define which batsman is playing and it will be used to stop the match
    Order <- rep(1:5,10)
    # Order is defined to deside which Bowler will Ball
    Runs <- 0
    Ind <- 1
    # Optional : i want to store data of every over so i create a list of length 50
    Over_Data <- list(length(50))
    for(i in 1:50){
      if(Ind == 11){
        break
      }
      Or <- Order[i]
      # i will define the balls in a over 
      # Optional: I will create a null variable to store over data 
      Over <- NULL
      for(j in 1:6){
        if(Ind == 11){
          break
        }
        # I decided that Out and Not Out is in Bowlers Hand and Runs are in Batsman Hand So 
        # I will take a sample to first deside that it is out or not and call is as a Sam
        Sam <- sample(c("Out","Not_Out"),1,replace = TRUE,prob = c(Bowling_1[Or][[1]],1-Bowling_1[Or][[1]]))
        # If Sam is Not out then we will decide about the runs
        if(Sam == "Not_Out"){
          Run <- sample(c(0,1,2,3,4,6),1,replace = TRUE,prob = Batting_2[Ind][[1]])
          Runs <- Runs + Run
          Over[j] <- Run
          
          if(Run == 1 || Run == 3){
            Runner <- Batting_2[Ind]
            Facer <- Batting_2[Ind + 1]
            Batting_2[Ind + 1] <- Runner
            Batting_2[Ind] <- Facer
          } 
        } else {
          # if player is out then next player will come to bat
          Ind <- Ind + 1
          Over[j] <- "Out"
        }
        # Now when over completes the Runner and Batter will interchange and save the over data 
        if (j == 6 ){
          Runner <- Batting_2[Ind]
          Facer <- Batting_2[Ind + 1]
          Batting_2[Ind + 1] <- Runner
          Batting_2[Ind] <- Facer
          Over_Data[[i]] <- Over
        }
      }
    }
    output_2 <- list(length(2))
    output_2[[1]] <- Runs
    output_2[[2]] <- Over_Data
    
    
    output <- list(length(3))
    output[[1]] <- output_1
    output[[2]] <- output_2
    if(output_1[[1]] > output_2[[1]]){
      output[[3]] <-  paste(Team_1,"Win") 
    } else {
      if(output_1[[1]] < output_2[[1]]){
        output[[3]] <- paste(Team_2,"Win")
      } else {
        output[[3]] <- "TIE"
      }
    } 
    
  }
  return(output)
}


## Team_1 is India And team_2 is Pakistan
Data <- Cricket_Match(India_batting,India_bowling,Pakistan_batting,Pakistan_bowling)

# Data[[1]] <- first team runs and over details      INIDA
Data[[1]][[1]]
Data[[1]][[2]][[25]]

# Data[[2]] <- second team runs and over details     PAKISTAN
Data[[2]][[1]]
Data[[2]][[2]][[25]]


# Data[[3]] contains who wins

Data[[3]]

Total <- NULL

for ( i in 1:1000){
  x <- Cricket_Match(India_batting,India_bowling,Pakistan_batting,Pakistan_bowling)
  Total[i] <-  x[[3]]
}

gp <- tibble(Match = 1:1000,Total)


gp <- gp %>%
  group_by(Total) %>% summarise(No_of_Match = n())

ggp <- ggplot(gp , aes(x = Total, y =No_of_Match)) + geom_bar(stat = "identity" , width = 0.5)  + 
  geom_text(aes(label=No_of_Match), vjust=1.5, colour="black", size=3)


ggp





















