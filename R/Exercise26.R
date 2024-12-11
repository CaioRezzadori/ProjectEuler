denominatorAdjust <- function(denom){
    while(denom %% 2 == 0){
        denom <- denom / 2
    }
    while(denom %% 5 == 0){
        denom <- denom / 5
    }
    return(denom)
}

decimalDivision <- function(num, denom, precision){
    remainder <- num %% denom
    digits <- integer(precision)
    index <- 0
    while(index < precision){
        index <- index + 1
        remainder <- remainder*10
        digits[index] <- remainder %/% denom
        remainder <- remainder %% denom
    }
    return(digits)
}


getCycleFraction <- function(num = 1, denom){
    denom <- denominatorAdjust(denom)

    remainder <- num %% denom
    cycle <- c((remainder * 10) %/% denom) # First digit
    counter <- 1
    while(TRUE){
        len <- length(cycle)
        if(len %% 2 == 0){
            subCycle <- cycle[1:(len/2)]
            if(identical(cycle, rep(subCycle, 2))){
                return(counter)
            }
            counter <- counter + 1
        }
        num <- num*10
        remainder <- num %% denom
        cycle <- c(cycle, (remainder * 10) %/% denom)
    }
    return(counter)
}

longestCycle <- function(limit){
    d <- d_max <- 3
    while(d < limit){
        d <- d + 1
        print(d)
        if(getCycleFraction(denom = d) > getCycleFraction(denom = d_max)){
            d_max <- d
        }
    }
    return(d_max)
}


#################################
findFirstDivisor <-function(num){
    d <- 2
    while(d < sqrt(num)){
        if(num %% d == 0) return(d)
        d <- d + 1
    }
    return("prime number")
}