# def knapsackRec(W, param, n):
def knapsackComb(W: int, param: list[dict[int, int]]):
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

def knapsackComb2(W: int, param: list[dict[int, int]]):
    combinations = [[0, 0]]
    for i in param:
        combinations += [[comb[0] + i['weight'], comb[1] + i['value']] if \
                          comb[0] + i['weight'] <= W else [0, 0] \
                          for comb in combinations]
    values = [val[1] for val in combinations]
    return max(values)

def maximum_value(W: int, param: list[dict[int, int]]):
    return knapsackComb(W, param)
