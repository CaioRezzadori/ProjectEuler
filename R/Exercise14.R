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

### Trying to optmize (save sequences) ####
############################################################
largestCollatzSeq <- function(limit){
    seqNumbers <- list()
    seqNumbers["1"] = 1
    for(num in 2:limit){

        auxNum <- num
        i <- 0
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
} #(Was worse than brute force)

#### Brute force ####
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
######################################################
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