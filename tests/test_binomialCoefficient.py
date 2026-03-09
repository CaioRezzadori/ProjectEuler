from projecteuler.binomialCoefficient import binomialCoeff, binomialCoeffDP


def test_1():
    assert 6 == binomialCoeff(4, 2)


def test_2():
    assert 10 == binomialCoeff(5, 2)


def test_3():
    assert 20 == binomialCoeff(6, 3)


def test_1_dp():
    assert 6 == binomialCoeffDP(4, 2)


def test_2_dp():
    assert 10 == binomialCoeffDP(5, 2)


def test_3_dp():
    assert 20 == binomialCoeffDP(6, 3)
