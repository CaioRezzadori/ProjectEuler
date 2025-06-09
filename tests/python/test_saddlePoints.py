from projecteuler.saddlePoints import saddlePoints

def test_example():
  assert [(2, 1)] == saddlePoints([[9, 8, 7, 8],
                                   [5, 3, 2, 4],
                                   [6, 6, 7, 1]])


# saddlePoints([[9, 8, 7, 8],
#               [5, 3, 2, 4],
#               [6, 6, 7, 1]])


# saddlePoints([[10, 8, 7, 8],
#               [5, 3, 2,  5],
#               [11, 6, 1, 11]])

# saddlePoints([[9, 8, 7, 8],
#               [2, 3, 5, 4],
#               [6, 6, 7, 1]])

# saddlePoints([[9, 8, 7, 8], # 8 is max in row and max in col
#               [2, 3, 1, 4], # 4 is max in row and 1 < 4 (not min in col)
#               [6, 6, 7, 1]]) # 7 is max in row and 1 < 7 (not min in col)

# # Exception tests
# saddlePoints([[9, 8, 7, 8],
#               [2, 3, 1, 4],
#               [6, 6, -1, 1]])

# saddlePoints([[9, 8, '7', 8],
#               [2, 3, 1, 4],
#               [6, 6, 8, 1]])

# saddlePoints([[9, 8, 7, 8],
#               ['2', 3, 1, 4],
#               [6, 6, 8, 1]])

# saddlePoints([[9, 8, 7],
#               [2, 3, 5, 4],
#               [6, 6, 1]])
