getDivisors <- function(num){
    if(num == 1 || num == 0){
        return(NULL)
    }
    seq <- 1:(num %/% 2)
    divisors <- seq[(num %% seq) == 0]
    return(c(divisors, num))
}

getPrimes <- function(numList){
    return(numList[sapply(numList, isPrime)])
}

practicalCheck <- function(num){
    if (num == 1 || num == 2) {
        return(TRUE)
    }
    primeDivisors <- rev(sort(getPrimes(getDivisors(num))))

    if(!(2 %in% primeDivisors)){
        return(FALSE)
    }

    aux <- num
    for(i in primeDivisors){
        while((aux %% i == 0) && aux != i){
            aux <- aux / i
        }
        if(i > 1 + sum(getDivisors(aux))){
            return(FALSE)
        }
    }
    return(TRUE)
}

practicalNumbersList <- function(limit){
    i <- 1
    numList <- integer(limit)
    while(i <= limit){
        if(practicalCheck(i)){
            numList[i] <- i
        }
        i <- i + 1
    }
    return(numList[numList > 0])
}

# 6804107
result <- practicalNumbersList(1e4)
sum(result)



divisors <- getDivisors(num)
practicalDivisors <- divisors[isPractical(divisors)]


# Old Solution
isPracticalOld <- function(num){
    divisors <- getDivisors(num)
    if((num == 1) || num == 2){
        return(TRUE)
    }
    if(!(2 %in% divisors) || (sum(divisors) < num - 1)){
        return(FALSE)
    }
    existCombination <- TRUE
    i <- 4
    while(existCombination && (i < num)){
        existCombination <- !is.null(sumCombination(i, divisors[divisors <= i]))
        i <- i + 1
    }
    return(existCombination)
}

sumCombination <- function(num, numList){
    if(sum(numList) == num){
        return(numList)
    }
    if(length(numList) == 0 || sum(numList) < num || all(numList > num)){
        return(NULL)
    }
    for(i in numList){
        subList <- numList[numList != i]
        result <- sumCombination(num, subList)
        if(!is.null(result)){
            return(result)
        }
    }
    return(NULL)
}

#################################################################
# Joao Vantol solution

properDivisors <- function(n) {
    if (n == 1) return(0)
    divisors <- c(1)

    if (n == 2) return(divisors)

    for (i in 2:sqrt(n)) {
        if (n %% i == 0) {
            divisors <- c(divisors, i)
            if (i != n / i) {
                divisors <- c(divisors, n / i)
            }
        }
    }

    return(divisors)
}

isPracticalNumber <- function(n) {
    if (n == 1 | n == 2) return (TRUE)

    divisors <- properDivisors(n)
    sums <- c()

    if (length(divisors) > 1) {
        for (divisor in divisors) {
            browser()
            sums <- union(sums, sums + divisor)
            sums <- union(sums, divisor)
        }
    }
    out <- sum(1:(n-1) %in% sums) == (n-1)

    return(out)
}

sumOfPracticals <- function(n = 10000) {
    result <- 0

    i <- 1
    while (i <= n) {
        if (isPracticalNumber(i)) {
            result <- result + i
        }
        i <- i + 1
    }

    return(result)
}