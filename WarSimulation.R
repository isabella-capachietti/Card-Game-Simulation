require(R.utils)

numberOfGamesToBePlayed <<- 100

setUpGame <- function() {

deck <<- list(1,2,3,4,5,6,7,8,9,10,11,12,13,1,2,3,4,5,6,7,8,9,10,11,12,13,1,2,3,4,5,6,7,8,9,10,11,12,13,1,2,3,4,5,6,7,8,9,10,11,12,13)
shuffleIndex <<- sample(1:52, 26, replace=F)

playerOneHand <<- list()
playerTwoHand <<- list()

playerOneTable <<- list()
playerTwoTable <<- list()

for (index in shuffleIndex){
  playerOneHand[[length(playerOneHand)+1]] <<- deck[[index]]
}


for (index in sort(shuffleIndex, decreasing = TRUE)){
  deck[[index]] <<- NULL
}

playerTwoHand <<- sample(deck)
}

runGame <- function() {
  gameFinished <<- FALSE
  firstWarWinner <<- 0
  gameWinner <<- 0
  comparisonCount <<- 0
  numberOfWars <<- 0
  while(length(playerOneHand) != 0 & length(playerTwoHand) != 0){
    cardsTheSame <<-FALSE
    
    playerOneTable[[length(playerOneTable)+1]] <<- playerOneHand[[1]]
    playerOneHand[[1]] <<- NULL
    playerTwoTable[[length(playerTwoTable)+1]] <<- playerTwoHand[[1]]
    playerTwoHand[[1]] <<- NULL
    
    
    
    
    #while the top cards are the same, remove 4 and check to see if the top cards are different
    while(playerOneTable[[length(playerOneTable)]] == playerTwoTable[[length(playerTwoTable)]]){
      cardsTheSame <<-TRUE
      numberOfWars <<- numberOfWars + 1
      comparisonCount <<- comparisonCount + 1
      #remove 4 cards
      for(x in c(1,2,3,4)){
        if(length(playerOneHand) == 0 | length(playerTwoHand) == 0){
          break
        }
        playerOneTable[[length(playerOneTable)+1]] <<- playerOneHand[[1]]
        playerOneHand[[1]] <<- NULL
        playerTwoTable[[length(playerTwoTable)+1]] <<- playerTwoHand[[1]]
        playerTwoHand[[1]] <<- NULL
      }
      if(playerOneTable[[length(playerOneTable)]] > playerTwoTable[[length(playerTwoTable)]]){
        if(numberOfWars == 1) {
          firstWarWinner <<- "P1"
          #print("P1")
        }
        comparisonCount <<- comparisonCount + 1
        for(card in playerOneTable){
          playerOneHand[[length(playerOneHand)+1]] <<- playerOneTable[[1]]
          playerOneTable[[1]] <<- NULL
        }
        for(card in playerTwoTable){
          playerOneHand[[length(playerOneHand)+1]] <<- playerTwoTable[[1]]
          playerTwoTable[[1]] <<- NULL
        }
        break
      } else if(playerOneTable[[length(playerOneTable)]] < playerTwoTable[[length(playerTwoTable)]]){
        if(numberOfWars == 1) {
          firstWarWinner <<- "P2"
          #print("P2")
        }
        comparisonCount <<- comparisonCount + 1
        for(card in playerTwoTable){
          playerTwoHand[[length(playerTwoHand)+1]] <<- playerTwoTable[[1]]
          playerTwoTable[[1]] <<- NULL
        }
        for(card in playerOneTable){
          playerTwoHand[[length(playerTwoHand)+1]] <<- playerOneTable[[1]]
          playerOneTable[[1]] <<- NULL
        }
        break
      }
    }
    if(cardsTheSame == FALSE){
      comparisonCount <<- comparisonCount + 1
    }
    
  }
  if(length(playerOneHand)==0) {
    gameWinner <<-"P2"
    #print("P2")
  } 
  if(length(playerTwoHand)==0) {
    gameWinner <<- "P1"
    #print("P1")
  }
    
  #print(comparisonCount)
  gameFinished <<- TRUE
}

runSimulation <- function() {
  numberOfGames <<- 0
  numberOfSames <<- 0
  totalComparisons <<- 0
  avgComparisons <<- 0
  
  firstRoundAndGameWinnerSame <<- list()
  gameWinners <<- list()
  firstWarWinners <<- list()
  ComparisonsResults <<- list()
  
  repeat {
    setUpGame()

    withTimeout({
      runGame()
    }, timeout = 2, onTimeout = "warning") 
    
    if(!firstWarWinner == 0 && gameFinished == TRUE){
      firstWarWinners[[length(firstWarWinners)+1]] <<- firstWarWinner
      gameWinners[[length(gameWinners)+1]] <<- gameWinner
      ComparisonsResults[[length(ComparisonsResults)+1]] <<- comparisonCount
      
      totalComparisons <<- totalComparisons + comparisonCount
      numberOfGames <<- numberOfGames + 1
      if(firstWarWinner == gameWinner) {
        firstRoundAndGameWinnerSame[[length(firstRoundAndGameWinnerSame)+1]] <<- 1
        numberOfSames <<- numberOfSames + 1
      } else {
        firstRoundAndGameWinnerSame[[length(firstRoundAndGameWinnerSame)+1]] <<- 0
      }
    }
    
    if(numberOfGames == numberOfGamesToBePlayed) {
      break
    }
    #Sys.sleep(1)
  }
  data <- data.frame(
    "id" = c (1:numberOfGames), 
    "comparison_counts" = unlist(ComparisonsResults, recursive = TRUE, use.names = TRUE),
    "first_war_winner" = unlist(firstWarWinners, recursive = TRUE, use.names = TRUE),
    "game_winner" = unlist(gameWinners, recursive = TRUE, use.names = TRUE),
    "same_winner" = unlist(firstRoundAndGameWinnerSame, recursive = TRUE, use.names = TRUE),
    
   
    stringsAsFactors = FALSE
  )
	
  avgComparisons <<- totalComparisons/numberOfGamesToBePlayed
  	
  print(data)
  print("number of games with same first war winner and game winner: ")
  print(numberOfSames)
  print("games with same first war winner and game winner per total: ")
  print(numberOfSames/numberOfGamesToBePlayed)
  print("average card comparisons per game: ")
  print(avgComparisons)
}

runSimulation()