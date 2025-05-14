def powerSum(num, power):
    sum = 0
    for digitIndex in range(1, len(str(num)) + 1):
        sum += ((num % 10**digitIndex) // 10**(digitIndex - 1))**power
    return(sum)

from math import log
def upperBound(power):
    # We want to calculate when (9**power)*y <= 10**y - 1 (9999... 9 y times)
    x = power*log(9, 10) - log(log(10), 10)

def findNumbers(power):
    num = 2
    numList = []

    while(num <= (10**(power + 1) - 1)):
        if(powerSum(num, power) == num):
            numList.append(num)
        num += 1
    return(numList)

findNumbers(5)
# findNumbers(6)



# Imporved version
def f(x, power):
    return((9**power)*x - 10**x + 1)
def df(x, power):
    return(9**power - (10**x)*log(10))

def newtonMethod(fun, dfun, power, x0 = 1, n = 1000):
    x = x0
    i = 0
    while(i < n):
        x = x - fun(*[x, power])/dfun(*[x, power])
        i += 1
    return(x)

def findNumbersImproved(power):
    num = 2
    numList = []

    powerLimit = newtonMethod(f, df,
                              power,
                              x0 = power*log(9, 10) - log(log(10), 10) + 1)
    while(num <= (10**(powerLimit) - 1)):
        if(powerSum(num, power) == num):
            numList.append(num)
        num += 1
    return(numList)

findNumbersImproved(5)

findNumbersImproved(6)