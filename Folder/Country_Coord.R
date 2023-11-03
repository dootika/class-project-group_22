library(ggplot2)
west_indies <- c("Saint Lucia", "Anguilla", "Dominican Republic", "Guadeloupe", "Antigua", "Barbados", "Jamaica", "Trinidad", "Cayman Islands", "Bahamas", "Haiti", "Cuba", "Virgin Islands", "Martinique", "Saint Kitts", "Bermuda", "	
Saint Barthelemy")
r <- map_data("world")
colnames(r)[5] <- "teams"
for (i in 1:length(r$teams)) {
  if (r$teams[i] %in% west_indies) {
    r$teams[i] <- "West Indies"
  }
  if (r$teams[i] == "UK") {
    r$teams[i] <- "England"
  }
  if (r$teams[i] == "United Arab Emirates") {
    r$teams[i] <- "U.A.E"
  }
  if (r$teams[i] == "USA") {
    r$teams[i] <- "U.S.A."
  }
  if (r$teams[i] == "Papua New Guinea") {
    r$teams[i] <- "P.N.G."
  }
}
coord <- r
save(coord, file = "Cricket_Coord.RData")
#data <- left_join(r, team_data, by = "teams")
#save(data, file = "World_Coord.Rdata")
#data <- data %>% filter(!is.na(win_loss))
#data
#ggplot(data, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = win_loss), color = "black")
teams <- unique(table$Loser)[c(1:6, 8:19, 24:28)]
W_L <- NULL
t1 <- 1980
t2 <- 2021
t <- table %>% filter((year(`Match Date`) >= t1) & (year(`Match Date`) <= t2))
for (i in 1:length(Teams)) {
  win <- sum(t$Winner == Teams[i])
  loss <- sum(t$Loser == Teams[i])
  if (loss == 0) {
    W_L <- append(W_L, NA)
  }
  else {
    W_L <- append(W_L, win/loss)
  }
}
WL <- data.frame(teams, W_L)
WL
data <- left_join(coord, WL, by = "teams")
ggplot(data, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = W_L), color = "black")