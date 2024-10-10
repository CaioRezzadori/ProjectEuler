### Sum of numbers multiples of 3 or 5 bellow some number "num" ###

#### SOLUTION 1 ####
multOfThreeFive <- function(num){
    multiples <- c()
    for(i in 1:(num - 1)){
        if(i %% 5 == 0| i %% 3 == 0){
            multiples <- c(multiples, i)
        }
    }
    return(sum(multiples))
}

#### SOLUTION 2 ####
multOfThreeFiveSol2 <- function(num){
    numbersBellow <- 1:(num - 1)
    multiplesOfFive <- (numbersBellow %% 5 == 0 | numbersBellow %% 3 == 0)
    return(sum(numbersBellow[multiplesOfFive]))
}

#### COMPARING SOLUTIONS ####
timeComparision(multOfThreeFive, multOfThreeFiveSol2, list(1000))