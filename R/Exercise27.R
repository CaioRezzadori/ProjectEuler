isPrime <- function(number){
    if(number <= 1){
        return(FALSE)
    }
    if(number == 2){
        return(TRUE)
    }
    if(number %% 2 == 0){
        return(FALSE)
    }
    i <- 3
    while(i < sqrt(number) + 1){
        if(number %% i == 0){
            return(FALSE)
        }
        i <- i + 1
    }
    return(TRUE)
}

getPrimes <- function(lowerBound, upperBound){
    i <- lowerBound
    primeList <- integer(upperBound - lowerBound)
    while(i <= upperBound){
        if(isPrime(i)){
            primeList[i - lowerBound + 1] <- i
        }
        i <- i + 1
    }
    return(primeList[primeList != 0])
}

# Since primes are positive, then the coefficient "b" is positive and should
# be prime, since for m = 0, m^2 + a*m + b = b

quadraticForm <- function(a, b, m){
    return(m^2 + a*m + b)
}

# a <= aMax, b <= bMax
maxPrimes <- function(aMax, bMax){
    bOptions <- getPrimes(2, bMax)
    aOptions <- -aMax:aMax
    mMax <- 0
    maxCoeff <- c(0, 0)
    for(a in aOptions){
        for(b in bOptions){
            m <- 0
            while(isPrime(quadraticForm(a, b, m))){
                m <- m + 1
            }
            if(m > mMax){
                mMax <- m
                maxCoeff[1] <- a
                maxCoeff[2] <- b
            }
        }
    }
    return(list(coeff = maxCoeff, m = mMax))
}

# |a| < 1000, |b| <= 1000
result <- maxPrimes(1000 - 1, 1000)
result

for(i in 0:result$m){
    num <- quadraticForm(result$coeff[1], result$coeff[2], i)
    cat(paste(num, ifelse(isPrime(num), "is prime", "not prime")), "\n")
}


# Curiosity: when a = -1 and b = 41, it generates
# more primes than a = 1 and b = 41 (Euler's quadratic formula)
for(i in 0:41){
    num <- quadraticForm(-1, 41, i)
    cat(paste0("iteration ", i, ": ",
              num, ifelse(isPrime(num),
              " is prime", " not prime")), "\n")
}