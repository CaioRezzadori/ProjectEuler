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

### Saving sequences ####
############################################################
largestCollatzSeq <- function(limit){
    seqLengths <- integer(limit)
    seqLengths[1] = 1
    num <- 2
    while(num <= limit){
        auxNum <- num
        count <- 0
        while(auxNum >= 1){
            if((auxNum < limit) && (seqLengths[auxNum] > 0)){
                seqLengths[num] <- seqLengths[auxNum] + count
                break
            }
            auxNum <- collatzIter(auxNum)
            count <- count + 1
        }
        num <- num + 1
    }
    return(list(number = which(seqLengths == max(seqLengths)),
                length = max(seqLengths)))
}

#### Brute force ####
######################################################
largestCollatzSeqNoOptimized <- function(limit){
    maxSeq <- 1
    lenMax <- 1
    num <- 1
    while(num <= limit){
        seq <- collatzSeq(num)
        if(length(seq) > lenMax){
            maxSeq <- num
            lenMax <- length(seq)
        }
        num <- num + 1
    }
    return(list(number = maxSeq, length = lenMax))
}