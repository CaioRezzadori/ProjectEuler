def runLengthEncoding(data):
    numberCheck = [str(x) in data for x in range(0, 10)]
    if(any(numberCheck)): return -1 # Error
    counter = 1
    encoding = ""
    for i in range(0, len(data) - 1):
        if(data[i] == data[i + 1]):
            counter += 1
        else:
            encoding += data[i] if counter == 1 else str(counter) + data[i]
            counter = 1
    encoding += data[len(data) - 1] if counter == 1 else \
                str(counter) + data[len(data) - 1] # Edge case
    return(encoding)

runLengthEncoding("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB")

runLengthEncoding("AABCCCDEEEE")

runLengthEncoding("AABCCCD1EEEE")

def runLengthDecoding(data):
    decoding = ""
    counter = 0
    for i, j in enumerate(data):
        try:
            int(j)
            counter += 1
        except ValueError:
            decoding += j if counter == 0 else int(data[i - counter: i])*j
            counter = 0

    return(decoding)

runLengthDecoding("2AB3CD4E") # == "AABCCCDEEEE"

runLengthDecoding("12WB12W3B24WB") # == "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"