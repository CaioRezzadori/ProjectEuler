from projecteuler.Exercise31 import coinSums

def teste_exercise31():
  assert 73682 == coinSums()

def teste_exercise31_other():
  assert 11 == coinSums(coinTypes = (1, 2, 5, 10), totalSum = 10)