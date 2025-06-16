# https://projecteuler.net/problem=31

def coinSums(coinTypes: tuple[int] = (1, 2, 5, 10, 20, 50, 100, 200),
             totalSum: int = 200):
  '''
  Finds all combinations of coins from "coinTypes" which sums to "totalSum"
  Example
  >>> coinSums()
  73682
  >>> coinSums(coinTypes = (1, 2, 5, 10), totalSum = 10)
  11
  '''
  maxNumber = tuple(totalSum // x for x in coinTypes)
  nCoins = len(coinTypes)
  combList = [()]
  nValidCombs = 0 #[]
  for comb in combList:
    combSize = len(comb)
    totalMoney = sum(tuple(coinTypes[i]*comb[i] for i in range(combSize)))
    if(combSize == nCoins and totalMoney == totalSum):
      nValidCombs += 1 # validCombs.append(comb) # n
    if(totalMoney > totalSum or combSize >= nCoins):
      continue # Pruning combinations
    for i in range(maxNumber[combSize] + 1):
      combList.append(comb + (i,))
  return(nValidCombs) # validCombs

# result = coinSums()

# coinSums((1, 2, 5, 10), totalSum=10)