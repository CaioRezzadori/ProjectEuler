from projecteuler.redundantSignalTowers import Towers


def test_codeWarsData():
    towers = Towers(
        N=3,
        x_i=[1, 2, 4],
        k_i=[1, 1, 3],
    )
    assert towers.shutDownTowers() == [2]


def test_2():
    towers = Towers(
        N=2,
        x_i=[1, 3],
        k_i=[1, 2],
    )
    assert towers.shutDownTowers() == []


def test_two_towers_no_redundant() -> None:
    towers = Towers(
        N=2,
        x_i=[0, 10],
        k_i=[5, 5],
    )
    assert len(towers.shutDownTowers()) == 0


def test_three_towers_middle_redundant() -> None:
    towers = Towers(
        N=2,
        x_i=[0, 5, 10],
        k_i=[10, 1, 10],
    )
    assert len(towers.shutDownTowers()) == 1


def test_all_increasing_ki() -> None:
    towers = Towers(
        N=2,
        x_i=[0, 1, 2, 3],
        k_i=[1, 2, 3, 4],
    )
    assert len(towers.shutDownTowers()) == 1


def test_large_gap() -> None:
    towers = Towers(
        N=2,
        x_i=[0, 1000, 2000],
        k_i=[100, 1, 100],
    )
    assert len(towers.shutDownTowers()) == 1
