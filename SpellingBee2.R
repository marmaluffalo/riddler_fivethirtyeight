#This is my solution to fivethirtyeight's Riddler Puzzle from March 29, 2019
#https://fivethirtyeight.com/features/can-you-win-a-spelling-bee-if-you-know-99-percent-of-the-words/

#ascending
contestants_up <- c(90, 91, 92, 93, 94, 95, 96, 97, 98, 99)
#descending
contestants_down <- c(99, 98, 97, 96, 95, 94, 93, 92, 91, 90)

#This is each turn a contestant takes
#If the word is greater than their player number, they won't know it
turn <- function(x) {
  word <- sample(1:100, 1)
  if (word > x) {
    return(0)
  } else {
    return(1)
  }
}

#Call the number of games and which contestant list we're using
season <- function(games, contestants){
  #generate blank scoreboard to start
  scores <- c(rep(0, 10))
  scoreboard <- data.frame(player=contestants, score=scores)
  for (game in 1:games){
    #Players left on the stage Resets each game.
    left = contestants
    #Continue as long as there are 2 players left
    while (length(left) > 1){
      for (contestant in left){
        if (turn(contestant) == 0){
          #Remove that contestant
          left <- left[! left %in% contestant]
        }
      }
    }
    #Only one player (winner) is left, so left is now a single value
    #Add one to that players score by filtering for the row where player == left
    scoreboard <- within(scoreboard, score[player==left] <- (score[player==left] + 1))
  }
  print(scoreboard)
}

#Let's play!
season(1000000, contestants_up)
season(1000000, contestants_down)
