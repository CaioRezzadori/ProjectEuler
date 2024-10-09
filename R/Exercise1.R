### Sum of numbers multiples of 3 or 5 bellow some number "num" ###

multOfThreeFive__ <- function(num){
    multiples <- c()
    for(i in 1:(num - 1)){
        if(i %% 5 == 0| i %% 3 == 0){
            multiples <- c(multiples, i)
        }
    }
    return(sum(multiples))
}

multOfThreeFiveSol2__ <- function(num){
    numbersBellow <- 1:(num - 1)
    multiplesOfFive <- (numbersBellow %% 5 == 0 | numbersBellow %% 3 == 0)
    return(sum(numbersBellow[multiplesOfFive]))
}


timeComparision(multOfThreeFive__, multOfThreeFiveSol2__, list(1000))