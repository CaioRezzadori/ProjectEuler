def luhnCheck(number):
    if(len(number) <= 1): return(False)

    number = number.replace(" ", "") # Removing spaces
    try:
        digits = [int(x) for x in number]
    except ValueError:
        return(False)

    for i in range(len(digits) - 2, -1, -2):
        digits[i] = digits[i]*2
        if(digits[i] > 9):
            digits[i] += -9
    return(sum(digits))


import numpy as np
def luhnCheckNp(number):
    if(len(number) <= 1): return(False)

    number = number.replace(" ", "") # Removing spaces
    try:
        digits = np.array([int(x) for x in number])
    except ValueError:
        return(False)

    digits[0:(len(digits) + 1):2] *= 2
    digits[digits > 9] += - 9

    return(sum(digits).tolist())

print([luhnCheck(x) for x in ["4539 3195 0343 6467",
                                        "8273 1232 7352 0569"]])

print("With numpy:")
print([luhnCheckNp(x) % 10 == 0  for x in ["4539 3195 0343 6467",
                                           "8273 1232 7352 0569"]])

print(luhnCheck("a4302"))