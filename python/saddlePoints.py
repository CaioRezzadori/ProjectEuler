# https://exercism.org/tracks/python/exercises/saddle-points
def saddlePoints(grid: list[list[int]]):
  nCols = len(grid[0])

  if(any([len(x) != nCols for x in grid])):
    raise ValueError("irregular matrix")

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
            return(row + 1, col + 1)

  return(None)


saddlePoints([[9, 8, 7, 8],
              [5, 3, 2, 4],
              [6, 6, 7, 1]])

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
