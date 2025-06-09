# https://projecteuler.net/problem=31
def coinSums(coinTypes: tuple[int] = (1, 2, 5, 10, 20, 50, 100, 200),
             totalSum: int = 200):
  maxNumber = tuple(totalSum // x for x in coinTypes)
  nCoins = len(coinTypes)
  combList = [()]
  validCombs = []
  for comb in combList:
    combSize = len(comb)
    moneyCoins = tuple(coinTypes[i]*comb[i] for i in range(combSize))
    if(combSize == nCoins and sum(moneyCoins) == totalSum):
      validCombs.append(comb)
    if(sum(moneyCoins) > totalSum or combSize >= nCoins):
      continue # Pruning combinations
    for i in range(maxNumber[combSize] + 1):
      combList.append(comb + (i,))
  return(validCombs)

result = coinSums()

len(result)