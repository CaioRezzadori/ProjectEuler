swap <- function(vec, a, b){
    aux <- vec[a]
    vec[a] <- vec[b]
    vec[b] <- aux
    return(vec)
}

moveNumber <- function(chart, direction){
    emptySpaceIndex <- which(chart == 0) # Representation of empty space in matrix
    nRow <- nrow(chart)
    moveList <- c(l = -nRow, r = nRow, u = -1, d = 1)
    numberSwapIndex <- emptySpaceIndex + moveList[direction]

    edgeCaseDimension <- (numberSwapIndex > 0 && {
                          numberSwapIndex <= length(chart)})
    edgeCaseDown <- !((direction == "d") && (emptySpaceIndex %% nRow == 0))
    edgeCaseUp <- !((direction == "u") && ((emptySpaceIndex - 1) %% nRow == 0))

    if(edgeCaseDimension && edgeCaseDown && edgeCaseUp){
        return(swap(chart,
                    emptySpaceIndex,
                    numberSwapIndex))
    }
    return(chart)
}

possibleMoves <- function(chartList, moveIndex){
    for(i in c("u", "d", "l", "r")){
        newChart <- moveNumber(chartList[[moveIndex]], i)
        if(!(list(newChart) %in% chartList)){
            chartList[[paste0(moveIndex, i)]] <- newChart
        }
    }
    return(chartList)
}

manhattanDistance <- function(chart, solvedChart){
    if(any(dim(chart) != dim(solvedChart))) {
        return(-1)
    } # Dimension error
    nRows <- nrow(chart)
    nCol <- ncol(chart)
    values <- 0:(length(chart) - 1)

    chartPos <- match(values, chart) - 1
    solvedChartPos <- match(values, solvedChart) - 1

    chartRows <- chartPos %% nRows
    chartCols <- chartPos %/% nCol
    solvedChartRows <- solvedChartPos %% nRows
    solvedChartCols <- solvedChartPos %/% nCol
    rowsDistance <- abs(chartRows - solvedChartRows)
    colsDistance <-  abs(chartCols - solvedChartCols)
    return(sum(rowsDistance[-1] + colsDistance[-1]))
}

puzzleSolver <- function(chart, solvedChart){
    moveTrack <- possibleMoves(list(x = chart), "x")
    distances <- unlist(lapply(moveTrack[names(moveTrack) != "x"],
                               manhattanDistance,
                               solvedChart))
    visitedTracks <- c("x")
    while(!any(0 %in% distances)){
        chartIndex <- names(which(distances == min(distances))[1])
        visitedTracks <- c(visitedTracks, chartIndex)
        tracksNames <- names(moveTrack)
        moveTrack <- possibleMoves(moveTrack, chartIndex)

        distances <- lapply(moveTrack,
                            manhattanDistance,
                            solvedChart)
        distances <- distances[!(names(distances) %in% visitedTracks)]
        distances <- unlist(distances)

        print(chartIndex)
        print(moveTrack[[chartIndex]])
    }

    print(which(distances == 0))

    # return(which(distances == 0))
    return(moveTrack)
}


puzzleChart <- matrix(c(15, 14, 1, 6,
                        9, 11, 4, 12,
                        0, 10, 7, 3,
                        13, 8, 5, 2), nrow = 4, ncol = 4, byrow = TRUE)

solvedChart <- matrix(1:16, nrow = 4, ncol = 4, byrow = TRUE)
solvedChart[16] <- 0

result <- puzzleSolver(puzzleChart, solvedChart)


moves<- unlist(strsplit("uurrddruuldddluulurrddlurrddluldrurdllurdruuulldrurdlldluurddlurrrddlluluurddlurulddrrrulddruulldrrdlluurdldrr",
        ""))

testPuzzle <- puzzleChart
for(i in moves){
    print(testPuzzle)
    print(i)
    testPuzzle <- moveNumber(testPuzzle, i)
}

####################################################################

puzzleChart <- matrix(c(3, 1, 2, 0), nrow = 2, ncol = 2, byrow = TRUE)

solvedChart <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE)
solvedChart[4] <- 0
result <- puzzleSolver(puzzleChart, solvedChart, stopParam = 100)


####################################################################



puzzleChart <- matrix(c(4, 1, 3,
                        2, 0, 6,
                        7, 5, 8), nrow = 3, ncol = 3, byrow = TRUE)

solvedChart <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
solvedChart[9] <- 0
result <- puzzleSolver(puzzleChart, solvedChart, stopParam = 100)



####################################################################




lapply(possibleMoves(list(input) , 1), manhattanDistance, solvedChart)
input <- matrix(c(7, 0, 2, 3, 4, 5, 6, 1, 8), nrow = 3, ncol = 3)



lapply(c("l", "r", "u", "d"), moveNumber, chart = input)
input <- matrix(rev(c(0, 1, 2, 3, 4, 5, 6, 7, 8)), nrow = 3, ncol = 3)
output <- matrix(rev(c(3, 0, 2, 1, 4, 5, 6, 7, 8)), nrow = 3, ncol = 3)
output <- matrix(c(0, 1, 2, 3, 4, 5, 6, 7, 8), nrow = 3, ncol = 3)

manhattanDistance(input, output)

which(input == 3)

which(input ==1)

input <- matrix(c(7, 0, 2, 3, 4, 5, 6, 1, 8), nrow = 3, ncol = 3)

possibleMoves(list(input) , )
moveNumber(input, "u")
input
dim(matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3))


which(input == 10)



inputL <- matrix(c(0, 1, 6, 3, 4, 5, 2, 7, 8), nrow = 3, ncol = 3)
inputR <- matrix(c(8, 1, 6, 3, 4, 5, 2, 7, 0), nrow = 3, ncol = 3)
inputU <- matrix(c(3, 1, 6, 0, 4, 5, 2, 7, 8), nrow = 3, ncol = 3)
inputD <- matrix(c(6, 1, 0, 3, 4, 5, 2, 7, 8), nrow = 3, ncol = 3)

inputU
moveNumber(inputU, "u")


input <- matrix(c( 1, 0, 2, 3, 4, 5, 6, 8, 7), nrow = 3, ncol = 3)
# output <- matrix(rev(c(3, 0, 2, 1, 4, 5, 6, 7, 8)), nrow = 3, ncol = 3)
output <- matrix(c(0, 1, 2, 3, 4, 5, 6, 7, 8), nrow = 3, ncol = 3)

manhattanDistance(input, output)

