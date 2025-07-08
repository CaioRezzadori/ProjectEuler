# Fazer com memoization com decorator TODO
def knapsackRec__(W: int, param: list[dict[int, int]], n: int) -> int:
    '''
    Solves knapsack with recursion. Aux function
    '''
    if W == 0 or n == 0:
        return 0

    pick = 0
    if W - param[n - 1]['weight'] >= 0:
        pick = param[n - 1]['value'] + knapsackRec__(W - param[n - 1]['weight'], param, n - 1)

    notPick = knapsackRec__(W, param, n - 1)

    return max([pick, notPick])

def knapsackRec(W: int, param: list[dict[int, int]]) -> int:
    '''
    Call recursive solution to knapsack
    '''
    return knapsackRec__(W, param, len(param))

# def knapsackDp(W, param):

def knapsackComb(W: int, param: list[dict[int, int]]) -> int:
    '''
    Solves knapsack with combinatorics
    '''
    combinations = [set()]
    n = len(param)
    maxValue = 0
    for i in range(n):
        for comb in combinations:
            newComb = comb.union({i})
            sumWeights, valSum = (0, 0)
            for j in newComb:
                sumWeights += param[j]['weight']
                valSum += param[j]['value']
            if sumWeights <= W and newComb not in combinations:
                combinations.append(newComb)
                if maxValue < valSum:
                    maxValue = valSum
    return maxValue

def knapsackComb2(W: int, param: list[dict[int, int]]) -> int:
    '''
    Solves knapsack with combinatorics
    '''
    combinations = [[0, 0]]
    for i in param:
        combinations += [[comb[0] + i['weight'], comb[1] + i['value']] if \
                          comb[0] + i['weight'] <= W else [0, 0] \
                          for comb in combinations]
    values = [val[1] for val in combinations]
    return max(values)

def maximum_value(W: int, param: list[dict[int, int]]) -> int:
    '''
    Call knasack function to tests
    '''
    return knapsackComb2(W, param)
