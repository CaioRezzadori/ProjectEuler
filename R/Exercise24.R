swap <- function(vec, number){
    vec[which(vec == number)] <- vec[1]
    vec[1] <- number
    return(vec)
}

permuteDigits <- function(digitsVec, limit){
    if(length(digits) == 1 || limit == 0){
        return(digitsVec)
    }
    for(i in sort(digitsVec)){
        permuteDigits(digits[2:length(digits)], limit - 1)
        newDigits <- swap(digits, i) ## Incompleto
    }
}