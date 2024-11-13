
import numpy as np

def collatzSeq(num):
    seq = [num]

    while(num > 1):
        num = collatzIter(num)
        seq.append(num)
    return(seq)

def collatzIter(num):
    if(num % 2 == 0):
        return(num // 2)
    return(3*num + 1)


def largestCollatzSeqNoOptimized(limit):
    maxSeq = 1
    lenMax = 1

    for num in range(1, limit):
        seq = collatzSeq(num)
        if(len(seq) > lenMax):
            maxSeq = num
            lenMax = len(seq)
    return(dict({"maxNumber": maxSeq, "maxLength": lenMax}))

def largestCollatzSeq(limit):
    seqNumbers = dict({"1": 1})
    num = 2
    while(num <= limit):
        auxNum = num
        i = 0
        while(auxNum >= 1):
            auxNum = str(auxNum)
            if auxNum in seqNumbers.keys():
                newLength = seqNumbers[auxNum]
                seqNumbers[str(num)] = newLength + i
                break
            auxNum = collatzIter(int(auxNum))
            i = i + 1
        num = num + 1
    maxNumber =  max(seqNumbers, key = seqNumbers.get)
    maxLength = seqNumbers[maxNumber]
    return({"maxNumber": maxNumber, "maxLength": maxLength})
