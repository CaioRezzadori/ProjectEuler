def swap(vec, a, b):
    swappedVec = vec.copy()
    swappedVec[a] = vec[b]
    swappedVec[b] = vec[a]
    return(swappedVec)

import numpy as np
def moveNumber(chart, direction):
    emptySpaceIndex = np.nonzero(chart == 0)
    numberSwapIndex = list(np.copy(emptySpaceIndex))
    match direction:
        case "u":
            numberSwapIndex[0] += - 1
        case "d":
            numberSwapIndex[0] += 1
        case "l":
            numberSwapIndex[1] += - 1
        case "r":
            numberSwapIndex[1] += 1
    if(any(np.array(numberSwapIndex) < 0)):
        return(chart)
    try:
        return(swap(chart,
                    emptySpaceIndex,
                    tuple(numberSwapIndex)))
    except IndexError:
        return(chart)

def possibleMoves(chartDict, moveIndex):
    for direction in ["u", "d", "l", "r"]:
        newChart = moveNumber(chartDict[moveIndex], direction)

        alreadyExists = [np.array_equal(x, newChart) for x in chartDict.values()]
        if(not any(alreadyExists)):
            chartDict[moveIndex + direction] = newChart
    return(chartDict)

def manhattanDistance(chart, solvedChart):
    if(chart.shape != solvedChart.shape):
        return(-1) # Dimension error

    distances = [0]*chart.size
    for i in solvedChart.flatten():
        indexChart = np.argwhere(chart == i)[0]
        indexSolvedChart = np.argwhere(solvedChart == i)[0]
        distances[i] = sum(abs(indexChart - indexSolvedChart))
    return(sum(distances[1:]))

def puzzleSolver(chart, solvedChart):
    moveTrack = possibleMoves({"x": chart},"x")
    visitedPaths = ["x"]
    distances = {x: manhattanDistance(y, solvedChart) for x, y in moveTrack.items()}
    while(not any(0 == distances)):
        nextPath = names(which(distances == min(distances))[1]) ###

        visitedPaths = visitedPaths.append(nextPath)
        moveTrack = possibleMoves(moveTrack, nextPath)

        newPaths <- moveTrack[setdiff(names(moveTrack), visitedPaths)]
        # newPaths <- newPaths[nchar(names(newPaths)) < 111]
        distances = {x: manhattanDistance(y, solvedChart) for x, y in newPaths.items()} #

        print(nextPath)
        print(moveTrack[nextPath])
    return(distances.values() == 0) ####

puzzleChart2 = np.array([[15, 11, 1, 6],
                        [0, 14, 4, 12],
                        [9, 10, 7, 3],
                        [13, 8, 5, 2]])

manhattanDistance(puzzleChart, puzzleChart2)

np.argwhere(puzzleChart == 0)

solvedChart = np.array(range(15, -1, -1)).reshape(4, 4)


puzzleChart = np.array([[14, 6, 1, 2],
                        [9, 11, 4, 12],
                        [15, 0, 7, 3],
                        [13, 8, 5, 10]])

np.argwhere(puzzleChart == 0)


teste = {"a": 0, "b": 1, "c": 2}

[sum(np.where(puzzleChart == x)) for x in solvedChart.flatten()]
np.where(puzzleChart == list())

np.argwhere(puzzleChart == solvedChart)
moveNumber(puzzleChart, "u")
teste = swap(puzzleChart, (0, 0), (1, 1))

chartD = {"x": puzzleChart}
[np.array_equal(teste, x) for x in chartD.values()]
print(possibleMoves({"x": puzzleChart}, "x"))

nRow = puzzleChart.shape[0]
moveList = {"l": -nRow, "r": nRow, "u": -1, "d" :-1}
teste = (1,1)
teste[1]

any(np.array(list(np.where(puzzleChart == 0))) < 0)


# np.array([0, 3])  list({1: np.array([1, 2]), 2: np.array([0, 3])}.values())