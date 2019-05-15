#this is my solution to the May 10th 2019 riddler about baseball hit-streaks
#https://fivethirtyeight.com/features/can-the-riddler-bros-beat-joe-dimaggios-hitting-streak/

#input averages
batters <- c(200, 250, 300, 350, 400, 500)
#5 guys get 3200 attempts, the PED guy gets half that
games_played <- c(rep(3200, 5), 1600)

careers <- data.frame(batter = batters, games_played = games_played)
careers

#If he logs a hit in a game, return 1
game <- function(x){
  hit = 0 
  for (i in 1:4){
    pitch <- sample(1:1000, 1)
    pitch
    if (pitch <= x){
      hit = hit + 1
    }
  }
  if (hit > 0){
    return(1)
  }
  else{
    return(0)
  }
}


for (a in 1:dim(careers)[1]){
  beats = 0
  #simulate 100000 careers for each player
  for (lifetimes in 1:100000){
    games <- careers$games_played[a]
    average <- careers$batter[a]
    current_streak = 0
    best_streak = 0
    #simulate all games in career
    for (k in 1:games){
      #if there's a hit add one to the streak
      if (game(average) == 1){
        current_streak = current_streak + 1
      } else{
        #when the streak ends, check if it's the biggest one yet
        #and save it under best_streak
          if (current_streak > best_streak){
            best_streak = current_streak
          }
        #reset streak to 0
          current_streak = 0
          }
    }
    #check if the record was broken during this career
    #even if the record was broken multiple times,
    #you only get one beat per career
    if (best_streak > 56){
      beats = beats + 1
    }
  }
  print(paste("Mr.", average, "beat Joe", beats, "times"))
}
