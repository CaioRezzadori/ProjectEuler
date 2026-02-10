from projecteuler.catalanNumber import catalanNumRec, catalanNumDp


def test_1():
    assert 1 == catalanNumRec(1)

def test_2():
    assert 2 == catalanNumRec(2)

def test_3():
    assert 5 == catalanNumRec(3)

def test_4():
    assert 14 == catalanNumRec(4)

def test_5():
    assert 42 == catalanNumRec(5)

def test_6():
    assert 132 == catalanNumRec(6)

def test_7():
    assert 429 == catalanNumRec(7)

def test_8():
    assert 1430 == catalanNumRec(8)


def test_16():
    assert 35357670 == catalanNumRec(16)



def test_dp_1():
    assert 1 == catalanNumDp(1)

def test_dp_2():
    assert 2 == catalanNumDp(2)

def test_dp_3():
    assert 5 == catalanNumDp(3)

def test_dp_4():
    assert 14 == catalanNumDp(4)

def test_dp_5():
    assert 42 == catalanNumDp(5)

def test_dp_6():
    assert 132 == catalanNumDp(6)

def test_dp_7():
    assert 429 == catalanNumDp(7)

def test_dp_8():
    assert 1430 == catalanNumDp(8)


def test_dp_16():
    assert 35357670 == catalanNumDp(16)