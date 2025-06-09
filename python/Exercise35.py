def isPrime(num: int) -> bool:
  if(num == 2):
    return(True)
  if(num % 2 == 0):
    return(False)

  for i in range(3, int(num**(1/2)) + 2, 2):
    if(num % i == 0):
      return(False)
  return(True)

def isCircularPrime(num: int) -> tuple[bool, list[int]]:
  strNum = str(num)
  nRot = len(strNum)
  digitRotations = [int(strNum[i:] + strNum[:i]) for i in range(nRot)]
  return(all([isPrime(x) for x in digitRotations]), digitRotations)

def circularPrimes(limit: float = 1e6) -> set[int]:
  num = 2
  testedNums = set()
  circularPrimesList = []
  while(num < limit):
    if(num not in testedNums):
      condition, numRotations = isCircularPrime(num)
      if(condition):
        circularPrimesList.extend(numRotations)
      testedNums.update(set(numRotations))
    num += 1
  return(set(circularPrimesList))

result = circularPrimes()