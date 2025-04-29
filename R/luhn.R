luhnCheck <- function(number){
    if(nchar(number) <= 1) return(FALSE)

    digits <- unlist(strsplit(gsub(" ", "", number), ""))

    digits <- suppressWarnings(as.numeric(digits))
    if(any(is.na(digits))) return(FALSE)

    indexSelect <- seq(length(digits) - 1, 1, -2)

    digits[indexSelect] <- digits[indexSelect]*2
    digits[digits > 9] <- digits[digits > 9] - 9
    return(sum(digits))
}

luhnCheck("4539 3195 0343 6467") # %% 10 == 0

luhnCheck("8273 1232 7352 0569") # %% 10 == 0

luhnCheck("A4230B")