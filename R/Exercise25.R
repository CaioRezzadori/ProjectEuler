# Recursive solution #
fibonacciNumberRecursive <- function(iter){
    if(iter == 1 || iter == 2) return(1)
    return(fibonacciNumberRecursive(iter - 1) + fibonacciNumberRecursive(iter - 2))
}

# Using gmp
library(gmp)

# Iterative solution #
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
fibonacciLA <- function(digitLimit){
    T <- matrix(c(0, 1, 1, 1), 2, 2, byrow = TRUE)
    ev <- eigen(T)
    ev$values <- as.bigq(ev$values)
    index <- 2
    vec <- c(1, 1)
    V <- ev$vectors
    values <- T %*% vec
    limit <- as.bigz(10)**(digitLimit - 1)
    while(any(values < limit)){
        browser()
        L <- identity(2, 2)%*%(ev$values ** index)
        T <-  V %*% L %*% t(V)
        values <- T %*% vec
        index <- index + 1
    }
    return(index)
}
fibonacciLA(10)