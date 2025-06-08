def totalSumCoins(coinTypes: tuple[int], nCoins: tuple[int]):
  if(len(coinTypes) != len(nCoins)):
    raise ValueError("coinTypes and nCoins must have same length")
  totalSum = 0
  for i in range(0, len(coinTypes)):
    totalSum += coinTypes[i]*nCoins[i]
  return(totalSum)

def coinSums(coinTypes = (1, 2, 5, 10, 20, 50, 100, 200), totalSum = 200):
  nCoins = len(coinTypes)
  combList = {()}
  validComb = set(())
  for i in range(0, (totalSum + 1) // coinTypes[0]):
    for comb in combList:
      newComb = comb + (i, )
      if(len(comb) < nCoins):
        combList = combList.union({newComb})
      if(len(validComb) == nCoins):
        validComb = validComb.union({newComb})
  return(combList)

coinSums()