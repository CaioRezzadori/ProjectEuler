def isArmstrong(number):
    numberList = list(str(number))
    numberList = [int(x)**len(numberList) for x in numberList]
    return(sum(numberList) == number)

print([isArmstrong(x) for x in [9, 10, 153, 154]])