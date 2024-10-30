collatzIter <- function(num){
    if(num %% 2 == 0){
        return(num %/% 2)
    }
    return(3*num + 1)
}

collatzSeq <- function(num){
    seq <- c(num)
    while(num > 1){
        num <- collatzIter(num)
        seq <- c(seq, num)
    }
    return(seq)
}

############################################################
largestCollatzSeq <- function(limit){
    seqNumbers <- list()
    seqNumbers["1"] = 1
    # seqLengths <- list(1)
    for(num in 2:limit){
        if(num == 6171){
            browser()
        }
        auxNum <- num
        i <- 0
        # browser()
        while(auxNum >= 1){
            auxNum <- as.character(auxNum)
            if(!is.null(seqNumbers[[auxNum]])){
                newLength <- seqNumbers[[auxNum]]
                seqNumbers[[as.character(num)]] <- newLength + i
                break
            }
            auxNum <- as.integer(auxNum)
            auxNum <- collatzIter(auxNum)
            i <- i + 1
        }
    }
    seqNumbers <- unlist(seqNumbers)
    lenMax <- max(seqNumbers)
    maxSeq <- names(seqNumbers[seqNumbers == lenMax])
    maxSeq <- as.integer(maxSeq)
    return(maxSeq)
}

######################################################
largestCollatzSeqNoOptimized <- function(limit){
    maxSeq <- 1
    lenMax <- 1
    for(num in 1:limit){
        seq <- collatzSeq(num)
        if(length(seq) > lenMax){
            maxSeq <- num
            lenMax <- length(seq)
        }
    }
    return(list(number = maxSeq, length = lenMax))
}


###### Using memoise ########
library(memoise)

collatzSeqRec <- function(num){
    if(num == 1){
        return(1)
    }
    if(num %% 2 == 0){
        return(c(num, collatzSeqRec(num %/% 2)))
    }
    return(c(num, collatzSeqRec(3*num + 1)))
}

collatzSeqMemoise <- memoise::memoise(collatzSeqRec)

largestCollatzSeqMemoise <- function(limit){
    maxSeq <- 1
    lenMax <- 1
    for(num in 1:limit){
        seq <- collatzSeqMemoise(num)
        if(length(seq) > lenMax){
            maxSeq <- num
            lenMax <- length(seq)
        }
    }
    return(list(number = maxSeq, length = lenMax))
}


largestCollatzSeqMemoise(5e5)

#### COMPARING SOLUTIONS ####

timeComparision(largestCollatzSeqNoOptimized,
                largestCollatzSeqMemoise, list(1e5), 1)



timeComparision(largestCollatzSeqNoOptimized,
                largestCollatzSeq, list(2000), 1)


largestCollatzSeq(10000)
largestCollatzSeqNoOptimized(100000)




l <- largestCollatzSeq(1e6)

l$number[(l$length == max(l$length))]
teste$number[(teste$length == max(teste$length))]
