from projecteuler.pascalTriangle import nRowPascalTriangle, nRowPascalTriangleTabulation


def test_base_case():
    assert nRowPascalTriangle(1) == [1]
    assert nRowPascalTriangle(2) == [1, 1]


def test_n_equals_3():
    assert nRowPascalTriangle(3) == [1, 2, 1]


def test_n_equals_4():
    assert nRowPascalTriangle(4) == [1, 3, 3, 1]


def test_n_equals_5():
    assert nRowPascalTriangle(5) == [1, 4, 6, 4, 1]


def test_n_equals_6():
    assert nRowPascalTriangle(6) == [1, 5, 10, 10, 5, 1]


def test_base_case_DP():
    assert nRowPascalTriangleTabulation(1) == [1]
    assert nRowPascalTriangleTabulation(2) == [1, 1]


def test_n_equals_3_DP():
    assert nRowPascalTriangleTabulation(3) == [1, 2, 1]


def test_n_equals_4_DP():
    assert nRowPascalTriangleTabulation(4) == [1, 3, 3, 1]


def test_n_equals_5_DP():
    assert nRowPascalTriangleTabulation(5) == [1, 4, 6, 4, 1]


def test_n_equals_6_DP():
    assert nRowPascalTriangleTabulation(6) == [1, 5, 10, 10, 5, 1]
