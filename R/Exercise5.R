# Using Euclidian Algorithm for Greatest Common Divisor (GCM)
GCM <- function(num1, num2){
    if(num1 == 0){
        return(num2)
    }
    return(GCM(num2 %% num1, num1))
}

# Least Common Multiple (LCM)
LCM <- function(num1, num2){
    return(num1 * num2 / GCM(num1, num2))
}

smallestMultiple <- function(limit){
    return(Reduce(LCM, 1:limit))
}

smallestMultiple(20)


# Old solution #
# smallestMultiple <- function(limit){
#     leastCommonMultiplesFactors <- c(1)
#     for(multiple in 2:limit){
#         multipleReduced <- multiple
#         for(j in leastCommonMultiplesFactors){
#             if(multipleReduced %% j == 0){
#                 multipleReduced <- multipleReduced / j
#             }
#         }
#         leastCommonMultiplesFactors <- c(leastCommonMultiplesFactors,
#                                          multipleReduced)
#     }
#     return(prod(leastCommonMultiplesFactors))
# }

# smallestMultiple(20)