def sumCombinations(num: int, nFactors: int, maxValueFactors: int = 9) -> set[tuple[int]]:
    '''
    Find tuples of length "nFactors" made of different integers with max values
    to "maxValueFactors" which sums to "num". Returns set of tuples
    Examples:
    >>> sumCombinations(45, 9)
    {(1, 2, 3, 4, 5, 6, 7, 8, 9)}
    >>> sumCombination(46, 9, 10)
    {(1, 2, 3, 4, 5, 6, 7, 8, 10)}
    >>> sumCombinations(7, 3)
    {(1, 2, 4)}
    >>> sumCombinations(10, 2)
    {(3, 7), (4, 6), (1, 9), (2, 8)}
    '''
    if(sum(nFactors*(1,)) > num or sum(nFactors*(maxValueFactors,)) < num):
        raise ValueError("Impossible combination")

    combList = {()}
    validComb = set()
    for i in range(1, maxValueFactors + 1):
        for comb in combList:
            if(sum(comb) < num and len(comb) < nFactors):
                newComb = tuple(sorted(comb + (i,)))
                combList = combList.union({newComb})
                if(sum(newComb) == num and len(newComb) == nFactors):
                    validComb = validComb.union({newComb})

    if(validComb == set()):
        raise ValueError("Impossible combination")
    return(validComb)

def sudokuCellValues(sudokuTable: list[list[int]], cellIndex: tuple[int, int]):
    '''
    Given a sudoku table, returns integer numbers between 1 and 9 that
    cells marked with 'x' cannot assume
    Examples:
    >>> sudokuCellValues(sudokuTable = [[0, 0, 0,    1,  0, 0,     0, 0, 0],
                                        [0, 0, 0,    0,  0, 0,     0, 0, 0],
                                        [0, 0, 0,    0,  0, 0,     0, 0, 0],

                                        [0, 0, 0,    0,  9, 0,     0, 0, 0],
                                        [0, 0, 0,    'x',0, 0,     0, 2, 0], # <
                                        [0, 0, 0,    0,  8, 7,     0, 0, 0],

                                        [0, 0, 0,    0,  0, 0,     0, 0, 0],
                                        [0, 0, 0,    0,  3, 0,     0, 0, 0],
                                        [0, 0, 0,    0,  0, 0,     0, 0, 0]],
                                                   # ^
                        cellIndex = (4, 3)
    (0, 1, 2, 7, 8, 9)
    (3, 4, 5, 6)
    '''
    # cellValues = ()
    cellValues = list(range(1, 10))
    squareCenterCell = ((1 + cellIndex[0] // 3)*3 - 1,
                        (1 + cellIndex[1] // 3)*3 - 1)

    for row in range(0, len(sudokuTable)):
        for col in range(0, len(sudokuTable[row])):
            squareCenterIter = ((1 + row // 3)*3 - 1,
                        (1 + col // 3)*3 - 1)
            valueIter = sudokuTable[row][col] #
            if(valueIter == 0 or valueIter == 'x'): #
                continue #
            if(row == cellIndex[0] or col == cellIndex[1] \
                or squareCenterCell == squareCenterIter):
                cellValues.remove(valueIter)
                # cellValues = cellValues + (valueIter,) if \
                #     sudokuTable[row][col] else cellValues #

    return(tuple(cellValues)) # Removing duplicates

def killerSudoku(sudokuTable: list[list[int]], sumValue: int):
    cageIndexes = set()
    for row in range(0, len(sudokuTable)):
        if('x' in sudokuTable[row]):
            cageIndexes = cageIndexes.union({(row,
                                              sudokuTable[row].index('x'))})
    cageValues = {sudokuCellValues(sudokuTable, x) for x in cageIndexes}
    nDigitCage = len(cageValues)

    possibleCombs = sumCombinations(sumValue, nDigitCage)
    # return(cageValues, possibleCombs)
    for i in possibleCombs:
        cageIndexesAux = {tuple(set(x).intersection(set(i))) for x in cageValues}
        # if(...):
        #     possibleCombs = possibleCombs - {i}
    return(cageIndexesAux)

sudokuTable = [[0, 0, 0,    1, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 0, 0,     0, 0, 0],
               [0, 0, 5,    0, 0, 0,     0, 0, 0],

               [0, 0, 0,    'x', 9, 0,   0, 0, 0],
               [0, 0, 0,    'x','x',0,   0, 2, 0],
               [0, 0, 6,    'x', 8, 7,   0, 0, 0],

               [0, 0, 0,    0, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 3, 0,     0, 0, 0],
               [0, 0, 0,    0, 0, 0,     0, 0, 0]]

sudokuCellValues(sudokuTable, (4, 3))

killerSudoku(sudokuTable, 10)