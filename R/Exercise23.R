# Uses function sumOfDivisors__, defined in exercise 21
getAbundantNumbers__ <- function(limit){
    abundantNumbers <- 12:limit # First abundant number is 12
    mask <- (abundantNumbers < (Map(sumOfDivisors__, abundantNumbers)))
    return(abundantNumbers[mask])
}

numbersNotSumOfAbundant <- function(limit){
    mask <- logical(limit)
    abundantNumbers <- getAbundantNumbers__(limit)
    for(i in abundantNumbers){
        mask[abundantNumbers + i] <- TRUE
    }
    numbers <- which(mask == FALSE)
    return(numbers)
}

sum(numbersNotSumOfAbundant(28123))

# Old Solution (not using mask) #
###################################
# numbersNotSumOfAbundant <- function(limit){
#     numbers <- 1:limit
#     abundantNumbers <- getAbundantNumbers__(limit)
#     for(i in abundantNumbers){
#         numbers <- numbers[!(numbers %in% (abundantNumbers + i))]
#     }
#     return(numbers)
# }