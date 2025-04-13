getDivisors <- function(num){
    if(num == 1 | num == 0){
        return(0)
    }
    seq <- 1:(num %/% 2)
    divisors <- seq[(num %% seq) == 0]
    return(divisors)
}

sumCombination <- function(num, numList){
    
}

isPractical <- function(num){
    divisors <- getDivisors(num)
    if((num == 1) || num == 2){
        return(TRUE)
    }
    if((sum(divisors) < num - 1) || !(2 %in% divisors)){
        return(FALSE)
    }
    existCombination <- TRUE
    i <- 4
    while((i < num) & existCombination){
        existCombination <- is.null(sumCombination(i, divisors[divisors <= i]))
        i <- i + 1
    }
    return(existCombination)
}