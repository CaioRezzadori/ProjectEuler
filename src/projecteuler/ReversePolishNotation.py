from typing import Any

operators = {"+": "__add__", "-": "__sub__", "*": "__mul__", "/": "__floordiv__"}


def applyOperator(num1: str, num2: str, op: str):
    if op in operators.keys():
        if op == "/" and (abs(int(num1)) < abs(int(num2))):
            return 0
        return getattr(int(num1), operators[op])(int(num2))


def solveRec(
    numList: list[Any], currentNumber: Any, symbolList: list[Any]
) -> str | int | Any:
    if not (symbolList or numList):
        return currentNumber
    if symbolList[0] in operators.keys():
        return solveRec(
            numList[:-1],
            applyOperator(numList[-1], currentNumber, symbolList[0]),
            symbolList[1:],
        )
    if currentNumber is not None:
        return solveRec(numList + [currentNumber], symbolList[0], symbolList[1:])
    return solveRec(numList, symbolList[0], symbolList[1:])


def solve(symbolList: list[Any]):
    return solveRec([], None, symbolList)


def solveDP(symbolList: list[str]):
    numList: list[Any] = []
    currentNumber = None
    while len(symbolList) > 0 or len(numList) > 0:
        if symbolList[0] in operators.keys():
            currentNumber = applyOperator(numList[-1], currentNumber, symbolList[0])
            numList = numList[:-1]
            symbolList = symbolList[1:]
        else:
            if currentNumber is not None:
                numList += [currentNumber]
            currentNumber = symbolList[0]
            symbolList = symbolList[1:]
    return currentNumber
