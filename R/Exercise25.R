# # Recursive Fibonacci Number #
# fibonacciNumberRecursive <- function(iter){
#     if(iter == 1 || iter == 2) return(1)
#     return(fibonacciNumberRecursive(iter - 1) + fibonacciNumberRecursive(iter - 2))
# }

# Using gmp
library(gmp)

# Iterative Fibonacci Number #
fibonacciNumberIterative <- function(iter){
    fib_1 <- fib_2 <- as.bigz(1)
    fib_3 <- fib_1 + fib_2
    index <- 2
    while(index < iter){
        index <- index + 1
        fib_1 <- fib_2
        fib_2 <- fib_3
        fib_3 <- fib_1 + fib_2
    }
    return(fib_2)
}

# Iterative solution

fibonacciIndex <- function(digitLimit){
    fib_1 <- fib_2 <- as.bigz(1)
    fib_3 <- fib_1 + fib_2
    index <- 3
    limit <- as.bigz(10)**(digitLimit - 1)
    while(fib_3 %/% limit == 0){
        fib_1 <- fib_2
        fib_2 <- fib_3
        fib_3 <- fib_1 + fib_2
        index <- index + 1
    }
    return(index)
}

library(Rmpfr)
# Using linear algebra
fibonacciIndexLA <- function(digitLimit){ # digitLimit = d
    # Sepctral decomposition of linear transformation
    T <- matrix(c(0, 1, 1, 1), 2, 2, byrow = TRUE)
    ev <- eigen(T)
    v_0 <- c(1, 1) # [f_0, f_1]
    V <- ev$vectors #Eigenvectors
    lambda <- ev$values #Eigenvalues

    # Calculating initial value to iterations
    limit <- as.bigz(10)**(digitLimit - 1)
    n <- ceiling((digitLimit - 1 - log10(1.5))/log10(lambda[1]) + 1)
    lambda <- as.bigq(lambda)

    # Power of n-1 of eigenvalue matrix
    L <- as.bigq(diag(c(1, 1)))
    L[1] <- lambda[1]** (n - 1)
    L[4] <- lambda[2]** (n - 1)

    # The inverse of V is equal to its transpose, since T is symmetric
    T <-  V %*% L %*% t(V)
    f <- T %*% v_0 # [f_(n-1), f_n]

    browser()
    # Finding result
    while(f[2]/limit < 1){
        temp <- f[1]
        f[1] <- f[2]
        f[2] <- f[1] + temp
        n <- n + 1
    }
    return(n)
}

# source("R/Exercise25.R")
fibonacciIndexLA(1e3)


fibonacciNumber <- function(iter){
    T <- matrix(c(0, 1, 1, 1), 2, 2, byrow = TRUE)
    ev <- eigen(T)
    vec <- c(1, 1)
    V <- ev$vectors

    ev$values <- as.bigq(ev$values)
    L <- as.bigq(diag(c(1, 1)))
    L[1] <- ev$values[1]** (iter - 1)
    L[4] <- ev$values[2]** (iter - 1)
    T <-  V %*% L %*% t(V)
    values <- T %*% vec

    return(as.numeric(values[1]))
}
