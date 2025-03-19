squareExtremeSums <- function(size){
    if(size == 1){
        return(1)
    }
    sum <- 0
    i <- 0
    while(i < 4){
        sum <- sum + (size**2 - (size - 1)*i)
        i <- i + 1
    }
    return(sum)
}

diagonalSum <- function(greatestSquareSize){
    diagSum <- 0
    size <- 1
    while(size <= greatestSquareSize){
        diagSum <- diagSum + squareExtremeSums(size)
        size <- size + 2
    }
    return(diagSum)
}

diagonalSum(1001)