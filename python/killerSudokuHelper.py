# https://exercism.org/tracks/r/exercises/killer-sudoku-helper

def sumCombinations(num: int, nFactors: int, maxValueFactors: int = 9) -> set[tuple[int]]:
    '''
    Find tuples of length "nFactors" made of different integers with max values
    to "maxValueFactors" which sums to "num". Returns set of tuples.
    Examples:
    >>> sumCombinations(7, 3)
    {(1, 2, 4)}
    >>> sumCombinations(10, 2)
    {(3, 7), (4, 6), (1, 9), (2, 8)}
    >>> sumCombinations(45, 9)
    {(1, 2, 3, 4, 5, 6, 7, 8, 9)}
    >>> sumCombination(46, 9, 10)
    {(1, 2, 3, 4, 5, 6, 7, 8, 10)}
    '''
    if(sum(nFactors*(1,)) > num or sum(nFactors*(maxValueFactors,)) < num):
        raise ValueError("impossible combination")

    combList = {()}
    validComb = set(())
    for i in range(1, maxValueFactors + 1):
        for comb in combList:
            if(sum(comb) < num and len(comb) < nFactors):
                newComb = tuple(sorted(comb + (i,)))
                combList = combList.union({newComb})
                if(sum(newComb) == num and len(newComb) == nFactors):
                    validComb = validComb.union({newComb})

    if(validComb == set(())):
        raise ValueError("impossible combination")
    return(validComb)

def sudokuCellValues(sudokuTable: list[list[int | str]],
                     cellIndex: tuple[int, int]) -> tuple[int]:
    '''
    Given a sudoku table 9x9, returns integer numbers between 1 and 9 that
    cell of index "cellIndex" can assume.
    Examples:
    >>> sudokuCellValues(sudokuTable = [[0, 0, 0,    1,  0, 0,     0, 0, 0],
                                        [0, 0, 0,    0,  0, 0,     0, 0, 0],
                                        [0, 0, 0,    0,  0, 0,     0, 0, 0],

                                        [0, 0, 0,    0,  9, 0,     0, 0, 0],
                                        [0, 0, 0,   'x', 0, 0,     0, 2, 0], # <
                                        [0, 0, 0,    0,  8, 7,     0, 0, 0],

                                        [0, 0, 0,    0,  0, 0,     0, 0, 0],
                                        [0, 0, 0,    0,  3, 0,     0, 0, 0],
                                        [0, 0, 0,    0,  0, 0,     0, 0, 0]],
                                                   # ^
                        cellIndex = (4, 3)
    (3, 4, 5, 6)
    '''
    # Initial possible values
    cellValues = list(range(1, 10))
    squareCenterCell = ((1 + cellIndex[0] // 3)*3 - 1,
                        (1 + cellIndex[1] // 3)*3 - 1)

    for row in range(0, 9):
        for col in range(0, 9):
            squareCenterIter = ((1 + row // 3)*3 - 1,
                                (1 + col // 3)*3 - 1)
            valueIter = sudokuTable[row][col]
            if(valueIter == 0 or valueIter == 'x'):
                continue
            # Applying sudoku rules
            if(row == cellIndex[0] or col == cellIndex[1] \
                or squareCenterCell == squareCenterIter):
                try:
                    cellValues.remove(valueIter)
                except ValueError: # Value was already removed
                    continue

    return(tuple(cellValues))

from itertools import permutations

def killerSudoku(sudokuTable: list[list[int | str]], sumValue: int):
    '''
    Given a sudoku table 9x9, return's possible sums combinations of "sumValue"
    where the cages are represented by adjacent cells with 'x', respecting
    the rules of sudoku.
    Examples:
    >>> killerSudoku(sudokuTable = [[0, 0, 0,    0, 0, 0,     0, 0, 0],
                                    [0, 0, 0,    0, 0, 0,     0, 0, 0],
                                    [0, 0, 0,    0, 0, 0,     0, 0, 0],

                                    [0, 0, 0,    'x',0, 0,   0, 0, 0],
                                    [0, 0, 0,    'x',0 ,0,   0, 0, 0],
                                    [0, 0, 0,    'x',0, 0,   0, 0, 0],

                                    [0, 0, 0,    0, 0, 0,     0, 0, 0],
                                    [0, 0, 0,    0, 0, 0,     0, 0, 0],
                                    [0, 0, 0,    0, 0, 0,     0, 0, 0]],
                    sumValue = 7)
    {124}
    >>> killerSudoku(sudokuTable = [[0, 0, 0,    1, 0, 0,     0, 0, 0],
                                    [0, 0, 0,    0, 4, 0,     0, 0, 0],
                                    [0, 0, 5,    0, 0, 0,     0, 0, 0],

                                    [0, 0, 0,    'x', 9, 0,  0, 8, 0],
                                    [0, 0, 0,    'x',0 ,0,   0, 2, 0],
                                    [0, 0, 6,    0 , 0, 7,   4, 0, 0],

                                    [0, 0, 0,    0, 0, 0,     0, 0, 0],
                                    [0, 0, 0,    0, 3, 0,     0, 0, 0],
                                    [0, 0, 0,    0, 0, 0,     0, 0, 0]],
                     sumValue = 10)
    {28, 46}
    '''
    cageIndexes = []
    for row in range(0, 9):
        for col in range(0, 9):
            if('x' == sudokuTable[row][col]):
                cageIndexes.append((row, col))

    cageValues = [sudokuCellValues(sudokuTable, id) for id in cageIndexes]
    possibleCombs = sumCombinations(sumValue, len(cageValues))
    for comb in possibleCombs:
        perm = permutations(comb)
        removeComb = True
        # Checking if exists arrange respecting sudoku rules
        for p in perm:
            if(all(x in y for x,y in zip(p, cageValues))):
                removeComb = False
                break
        possibleCombs = possibleCombs - {comb} if removeComb else possibleCombs
    # Adjusting output

    return({int(''.join(tuple(str(x) for x in comb))) for comb in possibleCombs})


sudokuTable = [[0, 0, 0,    0, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 0, 0,     0, 0, 0],

               [0, 0, 0,    'x',0, 0,   0, 0, 0],
               [0, 0, 0,    'x',0 ,0,   0, 0, 0],
               [0, 0, 0,    'x',0, 0,   0, 0, 0],

               [0, 0, 0,    0, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 0, 0,     0, 0, 0]]

killerSudoku(sudokuTable, 7)

sudokuTable = [[0, 0, 0,    0, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 0, 0,     0, 0, 0],

               [0, 0, 0,    0, 'x',0,   0, 0, 0],
               [0, 0, 0,    0, 'x',0,   0, 0, 0],
               [0, 0, 0,    0,  0, 0,   0, 0, 0],

               [0, 0, 0,    0, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 0, 0,     0, 0, 0]]

killerSudoku(sudokuTable, 10)

sudokuTable = [[0, 0, 0,    0, 4, 0,     0, 0, 0],
               [0, 0, 0,    0, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 1, 0,     0, 0, 0],

               [0, 0, 0,    0, 'x',0,   0, 0, 0],
               [0, 0, 0,    0, 'x',0,   0, 0, 0],
               [0, 0, 0,    0,  0, 0,   0, 0, 0],

               [0, 0, 0,    0, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 0, 0,     0, 0, 0]]

killerSudoku(sudokuTable, 10)

sudokuTable = [[0, 0, 0,    1, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 4, 0,     0, 0, 0],
               [0, 0, 5,    0, 0, 0,     0, 0, 0],

               [0, 0, 0,    'x', 9, 0,  0, 8, 0],
               [0, 0, 0,    'x',0 ,0,   0, 2, 0],
               [0, 0, 6,    0 , 0, 7,   4, 0, 0],

               [0, 0, 0,    0, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 3, 0,     0, 0, 0],
               [0, 0, 0,    0, 0, 0,     0, 0, 0]]

killerSudoku(sudokuTable, 10)

sudokuTable = [[0, 0, 0,    1, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 4, 0,     0, 0, 0],
               [0, 0, 5,    0, 0, 0,     0, 0, 0],

               [0, 0, 0,   'x', 9, 0,  0, 0, 0],
               [0, 0, 0,   'x',0 ,0,   8, 2, 0],
               [0, 0, 6,    0 , 0, 7,   4, 0, 0],

               [0, 0, 0,    0, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 3, 0,     0, 0, 0],
               [0, 0, 0,    0, 0, 0,     0, 0, 0]]

killerSudoku(sudokuTable, 10)

sudokuTable = [[0, 0, 0,    1, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 4, 0,     0, 0, 0],
               ['x','x', 5,    0, 0, 0,     0, 0, 0],

               ['x', 0, 0,  0, 9, 0,  0, 0, 0],
               [0, 0, 0,    0, 0 ,0,   8, 2, 0],
               [0, 0, 6,    0 , 0, 7,   4, 0, 0],

               [0, 0, 0,    0, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 3, 0,     0, 0, 0],
               [0, 0, 0,    0, 0, 0,     0, 0, 0]]

killerSudoku(sudokuTable, 10)


sudokuTable = [[0, 0, 0,    1, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 4, 0,     0, 0, 0],
               [0, 0, 5,    0, 0, 0,     0, 0, 0],

               [0, 0, 0,   'x', 9, 0,  0, 4, 6],
               [0, 0, 0,   'x',0 ,0,   8, 2, 0],
               [0, 0, 6,    0 , 0, 7,   4, 0, 0],

               [0, 0, 0,    0, 0, 0,     0, 0, 0],
               [0, 0, 0,    0, 3, 0,     0, 0, 0],
               [0, 0, 0,    0, 0, 0,     0, 0, 0]]


killerSudoku(sudokuTable, 19)