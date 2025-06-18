from projecteuler.spiralMatrix import spiralMatrix

def test_spiralMatrix3x3() -> None:
  assert[[1, 2, 3],
         [8, 9, 4],
         [7, 6, 5]] == spiralMatrix(3)

def test_spiralMatrix4x4() -> None:
  assert[[1,  2,  3,  4],
         [12, 13, 14, 5],
         [11, 16, 15, 6],
         [10, 9,  8,  7]] == spiralMatrix(4)