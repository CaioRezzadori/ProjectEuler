swap <- function(vec, a, b){
    aux <- vec[a]
    vec[a] <- vec[b]
    vec[b] <- aux
    return(vec)
}

moveNumber <- function(chart, direction){
    emptySpaceIndex <- which(chart == 0) # Representation in matrix
    nRow <- dim(chart)[1]
    moveList <- list(u = -1, d = 1, l = -nRow, r = nRow)
    numberSwap <- emptySpaceIndex + moveList[[direction]]
    if(!is.na(chart[numberSwap])){
        return(swap(chart,
                    emptySpaceIndex,
                    numberSwap))
    }
    return(chart)
}

manhattanDistance <- function(chart, solvedChart){
    if(any(dim(chart) != dim(solvedChart))) {
        return(-1)
    } # Dimension error or distinct elements
    nRows <- dim(chart)[1]
    nCol <- dim(chart)[2]
    lengthChart <- nRows*nCol

    values <- 1:(lengthChart - 1)
    chartRows <- chartCols <- integer(lengthChart - 1)
    solvedChartRows <- solvedChartCols <- integer(lengthChart - 1)
    for(i in values){
        chartRows[i] <- (which(chart == i) - 1) %% nRows
        chartCols[i] <- (which(chart == i) - 1) %/% nCol
        solvedChartRows[i] <- (which(solvedChart == i) - 1) %% nRows
        solvedChartCols[i] <- (which(solvedChart == i) - 1) %/% nCol
    }
    rowsDistance <- abs(chartRows - solvedChartRows)
    colsDistance <-  abs(chartCols - solvedChartCols)
    return(sum(rowsDistance + colsDistance))
    # return(list(chartRows = chartRows,
    #             chartCols = chartCols,
    #             solvedChartRows = solvedChartRows,
    #             solvedChartCols = solvedChartCols))
}

puzzleSolver <- function(chart, solvedChart){
    possibleMoves <- lapply(c("l", "r", "u", "d"), moveNumber, chart = input)
    moveTrack <- list()
    while(manhattanDistance(chart, solvedChart) > 0){
        possibleMoves <- lapply(c("l", "r", "u", "d"),
                                moveNumber,
                                chart = input)

        # for(i in c("r", "l", "u", "d")){
        #     possibleMoves[[i]] <- moveNumber(chart, i)
        # }
        
    }
}


lapply(c("l", "r", "u", "d"), moveNumber, chart = input)
input <- matrix(rev(c(0, 1, 2, 3, 4, 5, 6, 7, 8)), nrow = 3, ncol = 3)
output <- matrix(rev(c(3, 0, 2, 1, 4, 5, 6, 7, 8)), nrow = 3, ncol = 3)
output <- matrix(c(0, 1, 2, 3, 4, 5, 6, 7, 8), nrow = 3, ncol = 3)

manhattanDistance(input, output)

which(input == 3)

which(input ==1)

input
moveNumber(input, "u")

dim(matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3))


which(input == 10)