swapFirst__ <- function(vec, number){
    vec[which(vec == number)] <- vec[1]
    vec[1] <- number
    return(vec)
}

permuteDigits__ <- function(digitsVec, fixedDigits, counter, limit){
    if(length(digitsVec) == 1){
        if((counter > 0) && (counter == limit)){
            value <- c(fixedDigits, digitsVec)
            print(value)
            return(-1)
        }
        return(counter + 1)
    }
    for(i in digitsVec){
        digitsVec <- swapFirst__(digitsVec, i)
        if(counter > 0){
            counter <- permuteDigits__(digitsVec[-1],
                    c(fixedDigits, digitsVec[1]), counter, limit)
        }
    }
    return(counter)
}

permuteDigits <- function(digitsVec, limit){
    return(permuteDigits__(digitsVec, fixedDigits = c(), counter = 1, limit))
}

digits <- permuteDigits(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 1e6)

library(profvis)
library(htmlwidgets)
var <- profvis::profvis(permuteDigits(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 1e6))
saveWidget(var, "var.html")

permut <- c(0123,
            0132,
            0213,
            0231,
            0312,
            0321,
            1023,
            1032,
            1203,
            1230,
            1302,
            1320,
            2013,
            2031,
            2103,
            2130,
            2301,
            2310,
            3012,
            3021,
            3102,
            3120,
            3201,
            3210)
