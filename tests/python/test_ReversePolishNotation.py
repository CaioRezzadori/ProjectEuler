from projecteuler.ReversePolishNotation import solve, solveDP


def test_case1():
    input = ["2", "1", "+", "3", "*"]
    assert 9 == solve(input)


def test_case2():
    input = ["4", "13", "5", "/", "+"]
    assert 6 == solve(input)


def test_case3():
    input = ["10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+"]
    assert 22 == solve(input)


def test_case1DP():
    input = ["2", "1", "+", "3", "*"]
    assert 9 == solveDP(input)


def test_case2DP():
    input = ["4", "13", "5", "/", "+"]
    assert 6 == solveDP(input)


def test_case3DP():
    input = ["10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+"]
    assert 22 == solveDP(input)
