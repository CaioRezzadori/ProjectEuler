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

digitSumsFactorial <- function(num){
    maxDigits <- upperMagnitudeFactorial(num)
    digitsValue <- c()
    nDigit <- 1
    while(nDigit <= maxDigits){
        value <- 1
        previousValue <- digitsValue[nDigit]
        for(i in 1:num){
            value <- (value*i) %/% 10
            previousValue <- previousValue*i
        }
        nDigit <- nDigit + 1
    }
    return(aux)
}