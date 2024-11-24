sumOfDivisors__ <- function(num){
    if(num == 1 | num == 0){
        return(0)
    }
    seq <- 1:(num - 1)
    divisors <- seq[(num %% seq) == 0]

    return(sum(divisors))
}

isAmicable__ <- function(num_a){
    num_b <- sumOfDivisors__(num_a)
    return((num_a != num_b) & (sumOfDivisors__(num_b) == num_a))
}

sumOfAmicable <- function(limit){
    amicableNums <- c()
    i <- 1
    while(i < limit){
        if(isAmicable__(i)){
            amicableNums <- c(amicableNums, i)
        }
        i <- i + 1
    }
    return(sum(amicableNums))
}

sumOfAmicable(1e4)