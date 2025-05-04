def maximumThrill(atms):
    distances = range(0, len(atms))
    maxValue = - 1
    for i in distances:
        value = [abs(x - i) + atms[i] + atms[x] for x in distances]
        if (max(value) > maxValue):
            maxValue = max(value)
    return maxValue

import numpy as np
def maximumThrillNp(atms):
    distances = range(0, len(atms))
    maxValue = - 1
    for i in distances:
        value = abs(np.array(distances) - i) + atms[i] + np.array(atms)
        if (max(value) > maxValue):
            maxValue = max(value)
    return int(maxValue)


atms =  [3,1,3]
maximumThrill(atms)

atms = [2,3,4,5]
maximumThrill(atms)

atms = [10, 10, 11, 13, 7, 8, 9]
maximumThrill(atms)

atms = [2, 3, 4, 5, 10, 6, 7, 8, 9, 10, 11, 12, 4, 4, 2, 2, 12, 8]
maximumThrill(atms)



l = len(atms)
aux = list(range(0, l))
for i in range(0, l):
    print([abs(x - i) for x in aux])