### Largest Prime Factor ###

largestPrimeFactor <- function(num){
    largestPrime = num
    i = 2
    repeat{
        if(largestPrime <= i) break

        if(largestPrime %% i == 0){
            largestPrime <- largestPrime/i
        }
        else{
            i <- i + 1
        }
    }
    return(largestPrime)
}

timeMeasure(largestPrimeFactor, list(600851475143), 100)