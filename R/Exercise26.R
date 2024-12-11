decimalDivision <- function(num, denom){
    remainder <- num %% denom
    quotients <- c(remainder)
    remainder <- remainder * 10
    while(!(remainder %% denom) %in% quotients){
        remainder <- remainder %% denom
        quotients <- c(quotients, remainder)
        remainder <- remainder * 10
    }
    quotients <- c(quotients, remainder %% denom)
    return(quotients)
}

getCycleLength <- function(num, denom){
    quotients <- decimalDivision(num, denom)
    cycleIndex <- which(quotients == tail(quotients, 1))
    return(cycleIndex[2] - cycleIndex[1])
}

longestCycle <- function(limit){
    d <- d_max <- 3
    while(d < limit){
        d <- d + 1
        if(getCycleLength(1, denom = d) > getCycleLength(1, denom = d_max)){
            d_max <- d
        }
    }
    return(d_max)
}

longestCycle(1e3)