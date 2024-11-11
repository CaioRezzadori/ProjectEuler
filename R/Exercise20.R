##### Using gmp library ###############
library(gmp)
digitSumsFactorialGMP <- function(num){
    fact <- gmp::factorialZ(num)
    digitSum <- 0
    while(fact > 0){
        digitSum <- digitSum + fact %% 10
        fact <- fact %/% 10
    }
    return(digitSum)
}

##########################################################

##### Modular arithmetic solution #####
# (A * B) mod C = (A mod C * B mod C) mod C
upperMagnitudeFactorial <- function(num){
    digits <- 1
    for(i in 1:num){
        while(num/10 >= 1){
            num <- num %/% 10 + 1
            digits <- digits + 1
        }
        num <- num*i
    }
    return(digits)
}


lowerMagnitudeFactorial <- function(num){
    digits <- 1
    for(i in 1:num){
        while(num/10 >= 1){
            num <- num %/% 10
            digits <- digits + 1
        }
        num <- num*i
    }
    return(digits)
}

