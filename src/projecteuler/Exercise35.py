# https://projecteuler.net/problem=35

def isPrime(num: int) -> bool:
  '''
  Checks if number is prime or not
  Examples:
  >>> isPrime(2)
  True
  >>> isPrime(10)
  False
  >>> isPrime(13)
  True
  '''
  if(num == 2):
    return(True)
  if(num % 2 == 0):
    return(False)

  for i in range(3, int(num**(1/2)) + 2, 2):
    if(num % i == 0):
      return(False)
  return(True)

def isCircularPrime(num: int) -> tuple[bool, list[int]]:
  '''
  Gives all rotations of number and if they are all prime
  Examples:
  >>> isCircularPrime(197)
  (True, [197, 971, 719])
  >>> isCircularPrime(13)
  (True, [13, 31])
  >>> isCircularPrime(19)
  (False, [19, 91])
  '''
  strNum = str(num)
  nRot = len(strNum)
  digitRotations = [int(strNum[i:] + strNum[:i]) for i in range(nRot)]
  return(all([isPrime(x) for x in digitRotations]), digitRotations)

def circularPrimes(limit: float = 1e6) -> set[int]:
  '''
  Calculates all circular primes bellow "limit".
  Examples:
  >>> ciruclarPrimes(1e2)
  {97, 2, 3, 5, 37, 7, 71, 73, 11, 13, 79, 17, 31}
  '''
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

result = circularPrimes(1e2)