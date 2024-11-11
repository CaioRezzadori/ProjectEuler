sumOfDivisors__ <- function(num){
    divisors <- c()
    i <- 1
    while(i < (num - 1)){
        if(num %% i == 0){
            divisors <- c(divisors, i)
        }
        i <- i + 1
    }
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