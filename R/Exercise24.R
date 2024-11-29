# Swap positions from first element and the desired number
swapFirst__ <- function(vec, number){
    vec[which(vec == number)] <- vec[1]
    vec[1] <- number
    return(vec)
}

permuteDigits__ <- function(digitsVec, fixedDigits, counter, limit){
    if((length(counter) == 1) && (counter == limit)){
        return(c(fixedDigits, digitsVec))
    }
    if(length(digitsVec) == 1){
        return(counter + 1)
    }
    for(i in digitsVec){
        if(length(counter) > 1){
            break
        }
        digitsVec <- swapFirst__(digitsVec, i)
        counter <- permuteDigits__(digitsVec[-1],
                c(fixedDigits, digitsVec[1]), counter, limit)
    }
    return(counter)
}

getPermutation <- function(digitsVec, limit){
    return(permuteDigits__(digitsVec, fixedDigits = c(), counter = 1, limit))
}

digits <- getPermutation(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 1e6)