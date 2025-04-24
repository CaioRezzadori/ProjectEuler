luhnCheck <- function(n){
    if(nchar(n) <= 1) return(FALSE)

    digits <- unlist(strsplit(gsub(" ", "", n), ""))
    digits <- as.numeric(digits)
    indexSelect <- (0:(floor(length(digits) / 2)))*2 + 1
    indexSelect <- indexSelect[indexSelect < length(digits)]

    digits[indexSelect] <- digits[indexSelect]*2
    digits[digits > 9] <- digits[digits > 9] - 9
    return(sum(digits))
    # return(sum(digits[-indexSelect]) + sum(secondDigits))
}

luhnCheck("4539 3195 0343 6467") %% 10 == 0

luhnCheck("8273 1232 7352 0569") %% 10 == 0
