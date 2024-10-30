
smallestMultiple <- function(limit){
    leastCommonMultiplesFactors <- c(1)
    for(multiple in 2:limit){
        multipleReduced <- multiple
        for(j in leastCommonMultiplesFactors){
            if(multipleReduced %% j == 0){
                multipleReduced <- multipleReduced / j
            }
        }
        leastCommonMultiplesFactors <- c(leastCommonMultiplesFactors,
                                         multipleReduced)
    }
    return(prod(leastCommonMultiplesFactors))
}

smallestMultiple(20)