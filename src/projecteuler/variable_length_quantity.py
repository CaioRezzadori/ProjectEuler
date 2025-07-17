def encode(numList: list[int]) -> list[int]:
    """
    Apply encode__ function to list of numbers and concatenate results
    """

    def encode__(num: int) -> list[int]:
        """
        Creates VLQ encoding of number 'num'
        """

        binNum = bin(num)[2:]
        while len(binNum) % 7 != 0:
            binNum = "0" + binNum

        encodedNum: list[str] = []
        for i in range(0, len(binNum), 7):
            encodedNum.append(binNum[i : i + 7])

        encodedNum = [
            "1" + l if id < len(encodedNum) - 1 else "0" + l
            for id, l in enumerate(encodedNum)
        ]

        return [int(val, 2) for val in encodedNum]

    return sum([encode__(x) for x in numList], [])


def decode(bytes: list[int]) -> list[int]:
    """
    Decodes VLQ bytes to numbers
    """
    decodedBytes: list[int] = []
    num = ""
    for bt in bytes:
        # if byteLen > 7:
        #     raise ValueError("incomplete sequence")
        binByte = bin(bt)[2:]
        if len(binByte) == 8:
            num += bin(bt)[3:]
            continue
        num += "0" * (7 - len(binByte)) + binByte
        decodedBytes.append(int(num, 2))
        num = ""
    if decodedBytes == []:
        raise ValueError("incomplete sequence")
    return decodedBytes
