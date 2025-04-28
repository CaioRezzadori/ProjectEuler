def luhnCheck(number):
    if(len(number) <= 1): return(False)

    number = number.replace(" ", "") # Removing spaces
    digits = [int(x) for x in number]

    secondDigits = digits[0:(len(digits) + 1):2]
    otherDigits = digits[1:len(digits):2]

    secondDigits = [x*2 - 9 if x > 4 else x*2 for x in secondDigits]
    return(sum(otherDigits) + sum(secondDigits))

import numpy as np
def luhnCheckNp(number):
    if(len(number) <= 1): return(False)

    number = number.replace(" ", "") # Removing spaces
    digits = np.array([int(x) for x in number])

    digits[0:(len(digits) + 1):2] *= 2
    digits[digits > 9] += - 9

    return(sum(digits).tolist())

print([luhnCheck(x) for x in ["4539 3195 0343 6467",
                                        "8273 1232 7352 0569"]])

print("With numpy:")
print([luhnCheckNp(x) % 10 == 0  for x in ["4539 3195 0343 6467",
                                           "8273 1232 7352 0569"]])

