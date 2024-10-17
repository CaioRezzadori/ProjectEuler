### Largest Palindrome Product ###

#### SOLUTION 1 ####

palindromeChecking <- function(num){
    nDigits <- 0
    while(num %/% 10**nDigits != 0){
        nDigits <- nDigits + 1
    }
    while(nDigits > 0){
        firstDigit <- num %/% 10**(nDigits - 1)
        lastDigit <- num %% 10
        if(firstDigit != lastDigit){
            return(FALSE)
        }
        num <- num %% 10**(nDigits - 1)
        num <- num %/% 10
        nDigits <- nDigits - 2
    }
    return(TRUE)
}

divisionByNDigitCheck <- function(num, nDigit){
    factor1 <- as.integer(paste(rep("9", nDigit), collapse = ""))
    while(factor1 >= 10**(nDigit - 1)){
        factor2 <- num %/% factor1
        if((num == factor1*factor2) & (factor2 >= (10**(nDigit - 1))) &
            (factor2 < 10**nDigit)){
                return(c(factor1, factor2))
        }
        factor1 <- factor1 - 1
    }
    return(NULL)
}

LargestPalindromeProduct <- function(nDigitNum){
    maxFactor <- as.integer(paste(rep("9", nDigitNum), collapse = ""))
    factor1 <- factor2 <- maxFactor
    num <- factor1*factor2
    repeat{
        if(palindromeChecking(num)){
            if(!is.null(divisionByNDigitCheck(num, nDigitNum))){
                break
            }
        }
        num <- num - 1
    }

    return(num)
}


#### SOLUTION 2 ####

LargestPalindromeProductBruteForce <- function(nDigitNum){
    maxFactor <- as.integer(paste(rep("9", nDigitNum), collapse = ""))
    maxNum <- 1
    for(i in 10**(nDigitNum - 1):maxFactor){
        for(j in 10**(nDigitNum - 1):maxFactor){
            if(palindromeChecking(i*j) & maxNum < i*j){
                maxNum <- i*j
            }
        }
    }
    return(maxNum)
}

#### COMPARING SOLUTIONS ####
timeComparision(LargestPalindromeProductBruteForce,
                LargestPalindromeProduct, list(3), 5)
