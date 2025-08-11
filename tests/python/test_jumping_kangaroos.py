from projecteuler.jumping_kangaroos import kangaroo


def test_true():
    assert kangaroo(kanga1=0, speed1=3, kanga2=4, speed2=2)


def test_true_2():
    assert kangaroo(kanga1=4, speed1=2, kanga2=0, speed2=3)


def test_false():
    assert not kangaroo(kanga1=0, speed1=2, kanga2=5, speed2=3)
