# Recursive Fibonacci Number #
fibonacciNumberRecursive <- function(iter){
    if(iter == 1 || iter == 2) return(1)
    return(fibonacciNumberRecursive(iter - 1) + fibonacciNumberRecursive(iter - 2))
}

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
    index <- 2
    limit <- as.bigz(10)**(digitLimit - 1)
    while(fib_3 %/% limit == 0){
        index <- index + 1
        fib_1 <- fib_2
        fib_2 <- fib_3
        fib_3 <- fib_1 + fib_2
    }
    return(index + 1)
}

# Using linear algebra
fibonacciIndexLA <- function(digitLimit){
    T <- matrix(c(0, 1, 1, 1), 2, 2, byrow = TRUE)
    ev <- eigen(T)
    ev$values <- as.bigq(ev$values)
    vec <- c(1, 1)
    V <- ev$vectors

    limit <- as.bigz(10)**(digitLimit - 1)
    index <- floor((digitLimit - 1)/log10(max(ev$values)))
    L <- as.bigq(diag(c(1, 1)))
    L[1] <- ev$values[1] ** index
    L[4] <- ev$values[2] ** index
    T <-  V %*% L %*% t(V)
    values <- T %*% vec
    while(values[2] < limit){
        T <-  V %*% L %*% t(V)
        values <- T %*% vec
        L[1] <- L[1] * ev$values[1]
        L[4] <- L[4] * ev$values[2]
        index <- index + 1
    }
    return(index + 1)
}