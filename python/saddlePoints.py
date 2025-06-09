# https://exercism.org/tracks/python/exercises/saddle-points

def saddlePoints(grid: list[list[int]]) -> list[tuple[int, int]]:
  '''
  Given a square matrix of numeric values, returns indexes + (1, 1) of
  elements which are the maximum of it's rows and the minimum of it's columns.
  Solution may not exist, or if so, may have more than one.
  Examples:
  >>> saddlePoints([[9, 8, 7, 8],
                    [5, 3, 2, 4],
                    [6, 6, 7, 1]])
  [(2, 1)]
  >>> saddlePoints([[10, 8, 7, 8],
                    [5, 3, 2,  5],
                    [11, 6, 1, 11]])
  [(2, 1), (2, 4)]
  '''
  nCols = len(grid[0])

  if(any([len(x) != nCols for x in grid])):
    raise ValueError("irregular matrix")

  solutions = []
  for row in range(len(grid)):
    try:
      maxValueRow = max(grid[row])
    except TypeError:
      raise ValueError("invalid heigh")

    for col in range(len(grid[row])):
      try:
        minValueCol = min([grid[x][col] for x in range(len(grid))])
      except TypeError:
        raise ValueError("invalid heigh")

      if(grid[row][col] <= 0):
        raise ValueError("invalid height")

      if(grid[row][col] == maxValueRow and \
          grid[row][col] == minValueCol):
            solutions.append((row + 1, col + 1))

  return(solutions)


saddlePoints([[9, 8, 7, 8],
              [5, 3, 2, 4],
              [6, 6, 7, 1]])


saddlePoints([[10, 8, 7, 8],
              [5, 3, 2,  5],
              [11, 6, 1, 11]])

saddlePoints([[9, 8, 7, 8],
              [2, 3, 5, 4],
              [6, 6, 7, 1]])

saddlePoints([[9, 8, 7, 8], # 8 is max in row and max in col
              [2, 3, 1, 4], # 4 is max in row and 1 < 4 (not min in col)
              [6, 6, 7, 1]]) # 7 is max in row and 1 < 7 (not min in col)

# Exception tests
saddlePoints([[9, 8, 7, 8],
              [2, 3, 1, 4],
              [6, 6, -1, 1]])

saddlePoints([[9, 8, '7', 8],
              [2, 3, 1, 4],
              [6, 6, 8, 1]])

saddlePoints([[9, 8, 7, 8],
              ['2', 3, 1, 4],
              [6, 6, 8, 1]])

saddlePoints([[9, 8, 7],
              [2, 3, 5, 4],
              [6, 6, 1]])
