def swap(vec, a, b):
    swappedVec = vec.copy()
    swappedVec[a], swappedVec[b] = vec[b], vec[a]
    return(swappedVec)

import numpy as np
def moveNumber(chart, direction):
    emptySpaceIndex = np.nonzero(chart == 0)
    numberSwapIndex = np.array(emptySpaceIndex).ravel()
    directionDict = {'u': [-1, 0], 'd':[1, 0], 'l':[0, -1], 'r':[0, 1]}
    numberSwapIndex += np.array(directionDict[direction])

    if(any(np.array(numberSwapIndex) < 0) |
       any(numberSwapIndex >= chart.shape)):
        return(chart)

    return(swap(chart,
                emptySpaceIndex,
                tuple(numberSwapIndex)))

def possibleMoves(chartDict, moveIndex):
    for direction in ["u", "d", "l", "r"]:
        newChart = moveNumber(chartDict[moveIndex], direction)
        alreadyExists = np.all(newChart == np.array([*chartDict.values()]), axis = (1, 2))
        if(not any(alreadyExists)):
            chartDict[moveIndex + direction] = newChart
    return(chartDict)

def manhattanDistance(chart):
    nRow, nCol = chart.shape

    distance = 0
    for chartPos, solvedPos in enumerate(chart.ravel()):
        if(solvedPos == 0):
            continue
        rowDist = abs((solvedPos - 1) % nRow - chartPos % nRow)
        colDist = abs((solvedPos - 1) // nCol - chartPos // nCol)
        distance += rowDist + colDist
    return distance

def puzzleSolver(chart):
    moveTrack = possibleMoves({"x": chart},"x")
    moveTrack.pop("x")
    visitedPaths = ["x"]
    distances = {k: manhattanDistance(v) for \
                    k, v in moveTrack.items()}
    while(0 not in distances.values()):
        nextPath = min(distances, key = distances.get)
        visitedPaths.append(nextPath)
        possibleMoves(moveTrack, nextPath)
        distances = {x: manhattanDistance(moveTrack[x]) \
                     for x in moveTrack.keys() if x not in visitedPaths} #

        print(nextPath)
        print(moveTrack[nextPath])
    return(min(distances, key = distances.get)) ####

puzzleChart = np.array([[15, 14, 1, 6],
                        [9, 11, 4, 12],
                        [0, 10, 7, 3],
                        [13, 8, 5, 2]])

solvedPuzzle = np.array(range(1, 17)).reshape(4, 4)
solvedPuzzle[-1,-1] = 0
puzzleSolver(puzzleChart)







for i in solvedPuzzle[:]:
    print(i)


# "xuurrddruuldddluulurrddlurrddluldrurdllurdruuulldrurdlldluurddlurrrddlluluurddlurulddrrrulddruulldrrdlluurdldr"
# "xuurrddruuldddluulurrddlurrddluldrurdllurdruuulldrurdlldluurddlurrrddlluluurddlurulddrrrulddruulldrrdlluurdldr"
"xuurrddruuldddluulurrddlurrddluldrurdllurdruuulldrurdlldluurddlurrrddlluluurddlurulddrrrulddruulldrrdlluurdldr"
# "xuurrddruuldddlurdrullulurrdrddlulurdldruuuldldruurddldruuldlurrrddllurruldluurdlurdlddrruldlurrdllurdruulurdddluldrrulldrurdllurdrulldrruuuldddruluurdddluruuldddruluurdldrulurdldruldruulddrulurddd"
# possibleMoves({"x": puzzleChart})

# sum(abs(np.array(aux)))


# manhattanDistance(solvedChart=solvedPuzzle, chart=puzzleChart)

# np.where


# manhattanDistance(puzzleChart, solvedPuzzle)

# # np.argwhere(puzzleChart == 0)

# # solvedChart = np.array(range(15, -1, -1)).reshape(4, 4)


# # puzzleChart = np.array([[14, 6, 1, 2],
# #                         [9, 11, 4, 12],
# #                         [15, 0, 7, 3],
# #                         [13, 8, 5, 10]])

# # np.argwhere(puzzleChart == 0)


# # teste = {"a": 0, "b": 1, "c": 2}

# # [sum(np.where(puzzleChart == x)) for x in solvedChart.flatten()]
# # np.where(puzzleChart == list())

# # np.argwhere(puzzleChart == solvedChart)
# # moveNumber(puzzleChart, "u")
# # teste = swap(puzzleChart, (0, 0), (1, 1))

# # chartD = {"x": puzzleChart}
# # [np.array_equal(teste, x) for x in chartD.values()]
# # print(possibleMoves({"x": puzzleChart}, "x"))

# # nRow = puzzleChart.shape[0]
# # moveList = {"l": -nRow, "r": nRow, "u": -1, "d" :-1}
# # teste = (1,1)
# # teste[1]

# # any(np.array(list(np.where(puzzleChart == 0))) < 0)


# # np.array([0, 3])  list({1: np.array([1, 2]), 2: np.array([0, 3])}.values())