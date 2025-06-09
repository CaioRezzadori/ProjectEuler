#https://www.codewars.com/kata/66e03a09eeaad7e94d9f40a9
def subLists(l, id):
    '''
    Splits list "l" by removing elements from indexes in "id"
    Examples:
    subLists(l = [1, 2, 3, 4, 5, 6],
             id = [2, 4])
    >>> [[1, 2], [4], [6]]
    '''
    id = sorted(list(id))

    output = [l[id[i] + 1:id[i + 1]] for i in range(0, len(id) - 1)]

    return([l[:id[0]]] + output + [l[id[-1] + 1:]])

def minSumIndex(l, idRestrictions = {()}):
    '''
    Find list of indexes from sorted list "l" which elements from "l" have
    smallest sum and indexes not contained in "idRestriction"
    Examples:
    minSumIndex(l = [22, 22, 30, 50],
                idRestrictions = {(), (0,), (1,)})
    >>> (2,) # Min sum is 30
    minSumIndex(l = [22, 22, 30, 50],
                idRestrictions = {(), (0,), (1,), (2,)})
    >>> (0, 1) # Min sum is 22 + 22 = 40
    minSumIndex(l = [22, 22, 30, 50],
                idRestrictions = {(), (0,), (1,), (2,), (0, 1)})
    >>> (3,) # Min sum is 50
    minSumIndex(l = [22, 22, 30, 50],
                idRestrictions = {(), (0,), (1,), (2,), (3,) (0, 1)})
    >>> (1, 2) # Min sum is 22 + 30 = 52
    '''
    # Finding possible list of indexes, assuming "l" is sorted
    idSet = idRestrictions
    for i in range(0, len(l)):
        idSet = idSet.union({x + (i,) if (x and (i,)) in idRestrictions \
                             else x for x in idSet})
    idSet = {tuple(set(x)) for x in idSet} # Remove duplicates inside tuples
    idSet = tuple(idSet - idRestrictions)

    minComb = 0
    minSum = sum([l[x] for x in idSet[0]])
    for i, j in enumerate(idSet):
        newSum = sum([l[x] for x in j])
        if(newSum < minSum):
            minComb = i
            minSum = newSum
    return idSet[minComb]

def maxEarnings(earnings, k):
    '''
    Remove elements from list "earnings" which has minimum sum splits earnings
    in sublists with length lesser than k - 1
    Examples
    maxEarnings(earnings = [60, 70, 80, 40, 80, 90, 100, 20],
                k = 3)
    >>> [20, 40]
    maxEarnings(earnings = [45, 12, 78, 34, 56, 89, 23, 67, 91],
                            k = 4)
    >>> [12, 23]
    '''
    sortedIdEarnings, sortedEarnings = zip(*sorted(enumerate(earnings),
                                                   key = lambda x:x[1]))
    minRestDays = len(earnings) // (k + 1)
    restDays = sortedIdEarnings[:minRestDays]
    testedCombinations = {(), restDays}
    while(any([len(x) > k for x in subLists(earnings, restDays)])):
        # Update restDays
        comb = minSumIndex(sortedEarnings,
                           idRestrictions = testedCombinations)
        testedCombinations.update({comb})
        restDays = tuple([sortedIdEarnings[i] for i in comb])
    return tuple([earnings[i] for i in restDays])

maxEarnings(earnings = [100, 1, 90, 2, 80, 3, 70],
            k = 2)

maxEarnings(earnings = [100, 1, 90, 2, 80, 3, 70],
            k = 3)

maxEarnings(earnings = [60, 70, 80, 40, 80, 90, 100, 20],
            k = 3)

maxEarnings(earnings = [45, 12, 78, 34, 56, 89, 23, 67, 91],
            k = 4)


from random import shuffle
earnings = list(range(40))
shuffle(earnings)

maxEarnings(earnings = earnings,
            k = 10)