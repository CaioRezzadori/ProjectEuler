from projecteuler.maximizeSegments import solve, solveMemoization


def test_1():
    assert 4 == solve(4, (2, 1, 1))


def test_2():
    assert 2 == solve(5, (5, 3, 2))


def test_3():
    assert 0 == solve(7, (8, 9, 10))


def test_4():
    assert 20 == solve(40, (3, 3, 2))


def test_5():
    assert 5 == solve(11, (2, 3, 5))


def test_1_memo():
    assert 4 == solveMemoization(4, (2, 1, 1))


def test_2_memo():
    assert 2 == solveMemoization(5, (5, 3, 2))


def test_3_memo():
    assert 0 == solveMemoization(7, (8, 9, 10))


def test_4_memo():
    assert 20 == solveMemoization(40, (3, 3, 2))


def test_5_memo():
    assert 5 == solveMemoization(11, (2, 3, 5))
