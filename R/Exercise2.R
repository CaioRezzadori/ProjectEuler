### Sum of Even Fibonacci Numbers ###

#### RECURSIVE SOLUTION ####
fibonacciSeqRec__ <- function(n){
    if(n == 1){
        return(c(1))
    }
    if(n == 2){
        return(c(1, 1))
    }
    prevArray <- sumFibonacciNumber__(n - 1)
    aux <- length(prevArray)
    return(c(prevArray, prevArray[aux] + prevArray[aux - 1]))
}

fibonacciSeqRec <- function(max){
    n <- 1
    while(tail(fibonacciSeqRec__(n), 1) < max){
        n <- n + 1
    }
    return(fibonacciSeqRec__(n - 1))
}

seqEvenSumRec <- function(max){
    seq <-fibonacciSeqRec(max)
    mask <- seq %% 2 == 0
    return(sum(seq[mask]))
}

#### ITERATIVE SOLUTION ####
fibonacciSeq <- function(max){
    seq <- c(1, 1)
    repeat{
        i <- length(seq)
        if(seq[i] + seq[i - 1] >= max){
            break
        }
        seq <- c(seq, seq[i] + seq[i - 1])
    }
    return(seq)
}

seqEvenSum <- function(max){
    seq <- fibonacciSeq(max)
    mask <- seq %% 2 == 0
    return(sum(seq[mask]))
}

#### COMPARING SOLUTIONS ####
timeComparision(seqEvenSumRec, seqEvenSum, list(1e16))
