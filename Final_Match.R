Cricket_Match <- function(Batting , Bowling){
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
      Sam <- sample(c("Out","Not_Out"),1,replace = TRUE,prob = c(Bowling[Or][[1]],1-Bowling[Or][[1]]))
      # If Sam is Not out then we will decide about the runs
      if(Sam == "Not_Out"){
        Run <- sample(c(0,1,2,3,4,6),1,replace = TRUE,prob = Batting[Ind][[1]])
        Runs <- Runs + Run
        Over[j] <- Run
       
        if(Run == 1 || Run == 3){
          Runner <- Batting[Ind]
          Facer <- Batting[Ind + 1]
          Batting[Ind + 1] <- Runner
          Batting[Ind] <- Facer
        } 
      } else {
        # if player is out then next player will come to bat
        Ind <- Ind + 1
        Over[j] <- "Out"
      }
      # Now when over completes the Runner and Batter will interchange and save the over data 
      if (j == 6 ){
        Runner <- Batting[Ind]
        Facer <- Batting[Ind + 1]
        Batting[Ind + 1] <- Runner
        Batting[Ind] <- Facer
        Over_Data[[i]] <- Over
      }
    }
  }
  output <- list(length(2))
  output[[1]] <- Runs
  output[[2]] <- Over_Data
  return(output)
}


Data <- Cricket_Match(Bagladesh_batting,Shrilanka_Bowlers)
Data[[1]]
Data[[2]][[25]]

Total <- NULL
for ( i in 1:1000){
  x <- Cricket_Match(Bagladesh_batting,Shrilanka_Bowlers)
  Total[i] <-  x[[1]]
}

gp <- tibble(Match = 1:1000,Total)

ggp <- ggplot(gp , aes(x = Match , y = Total)) + geom_point(stat = "identity") +
  stat_smooth(method = "loess",
              formula = y ~ x) + 
  geom_line(aes(y=mean(Total)), col = "red")

ggp






















