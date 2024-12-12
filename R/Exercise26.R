decimalDivisionRemainders <- function(num, denom){
    r <- num %% denom
    remainders <- c(r)
    r <- r * 10
    while(!(r %% denom) %in% remainders){
        r <- r %% denom
        remainders <- c(remainders, r)
        r <- r * 10
    }
    remainders <- c(remainders, r %% denom)
    return(remainders)
}

getCycleLength <- function(num, denom){
    remainders <- decimalDivisionRemainders(num, denom)
    cycleIndex <- which(remainders == tail(remainders, 1))
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

longestCycle <- function(limit){
    d <- d_max <- limit
    maxCycle <- getCycleLength(1, denom = d_max)
    while(d >= 1){
        d <- d - 1
        cycle <- getCycleLength(1, denom = d)
        if(cycle > maxCycle){
            d_max <- d
            maxCycle <- getCycleLength(1, denom = d_max)
        }
        if(d < maxCycle){
            break
        }

    }
    return(d_max)
}

longestCycle(1e4)