#H-oPwR_Chapter7SlotMachineProgram
#Randomly Generate Three Symbols
#######################
#######################
#######################
#Conditions To Win Prizes
#***Three of a kind except zeroes
#***Three bars of mixed variety
#***One or more cherries
#######################
# Combination	Prizes
# DD DD DD	100
# 7 7 7	80
# BBB BBB BBB	40
# BB BB BB	25
# B B B	10
# C C C	10
# Any combination of bars	5
# C C *	5
# C * C	5
# * C C	5
# C * *	2
# * C *	2
# * * C	2
get_symbols <- function()
{
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE, 
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}
get_symbols()
symbols <- get_symbols()
symbols
print(symbols)
#The following takes the output from the get_symbols function and
#calculates the correct prize
#######################
#Assess The Symbols
score <- function (symbols)
{
  same <- symbols[1] == symbols [2] && symbols[2] == symbols[3] #SimilarCase1Three of a kind
  bars <- symbols %in% c("B", "BB", "BBB") #SimilarCase2Bars only
  if (same) {
    payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
                 "B" = 10, "C" = 10, "0" = 0) #SimilarCase1Payouts for three of a kind
    prize <- unname(payouts[symbols[1]]) #Dollar Amount Payout
  } else if (all(bars)) 
  {prize <- 5 #SimilarCase2Bars only Dollar Amount Payout
  } else {
    cherries <- sum(symbols == "C")
    prize <- c(0, 2, 5) [cherries + 1] #SimilarCase3 Cherries Dollar Amount Payout
    #[cherries + X] represents vector item #1 , #2, and #3
  }
  #Adjust for diamonds
  #Diamonds are considered wildcards
  #Every diamond that appears in a combination doubles the final prize amount
  diamonds <- sum(symbols =="DD")
  prize * 2 ^ diamonds #Diamonds Dollar Amount Payout
}
play <- function() {
  symbols <- get_symbols()
  print(symbols)
  score(symbols)
}
play()

