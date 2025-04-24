isArmstrongNumber <- function(n){
    digits <- unlist(strsplit(as.character(n), ""))
    digits <- as.numeric(digits)
    return(sum(digits**length(digits)) == n)
}

isArmstrongNumber(9)
isArmstrongNumber(10)
isArmstrongNumber(153)
isArmstrongNumber(154)