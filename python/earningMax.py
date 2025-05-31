def subLists(l, id):
    '''
    Splits list "l" by removing elements from indexes in "id"
    Examples:
    subLists(l = [1, 2, 3, 4, 5, 6],
             id = [2, 4])
    >>> [[1, 2], [4], [6]]
    '''
    id.sort()
    output = [l[id[i] + 1:id[i + 1]] for i in range(0, len(id) - 1)]

    return([l[:id[0]]] + output + [l[id[-1] + 1:]])

def minSumIndex(l, minSize, idRestrictions):
    '''
    Find list of indexes from sorted list "l", with length
    greater than minSize - 1, which elements from "l" have
    smallest sum and indexes not contained in "idRestriction"
    '''

    # counter = minSize
    id = [] #list(range(0, minSize))
    previousIds = []
    while(id in idRestrictions and len(id) < minSize):
        for i in range(0, len(l)):
            if(i in id):
                continue
            l[i]
            if(id not in previousIds):
                break
        previousIds.append(id)
    return id

def maxEarnings(earnings, k):
    sortedEarnings, sortedIdEarnings = zip(*sorted(enumerate(earnings),
                                                   key = lambda x:x[1]))
    minRestDays = len(earnings) // (k + 1)
    restDays = sortedIdEarnings[:minRestDays]
    testedCombinations = [[]]
    while(any([len(x) > k for x in subLists(earnings, restDays)])):
        # Update restDays
        comb = minSumIndex(sortedEarnings,
                           minSize = minRestDays,
                           idRestrictions = testedCombinations)
        testedCombinations.append(comb)
        restDays = [sortedIdEarnings[i] for i in comb]
    return [earnings i for i in restDays]