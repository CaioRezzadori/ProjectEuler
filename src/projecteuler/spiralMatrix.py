# https://exercism.org/tracks/python/exercises/spiral-matrix

def spiralMatrix(size: int):
  '''
  Create spiral matrix of size "size".
  Examples:
  >>> spiralMatrix(3):
  [[1, 2, 3],
   [8, 9, 4],
   [7, 6, 5]],
  '''
  visitedIds = [(0, 0)]
  directions = {'right': (0, 1),
               'down': (1, 0),
               'left': (0, -1),
               'up': (-1, 0)}
  directionKey = 'right'
  while(len(visitedIds) < size**2):
    newId = tuple(x + y for x, y in zip(visitedIds[-1],
                                        directions[directionKey]))
    if(0 <= newId[0] < size and 0 <= newId[1] < size and\
       newId not in visitedIds):
      visitedIds.append(newId)
    else:
      match directionKey:
        case 'right':
          directionKey = 'down'
        case 'down':
          directionKey = 'left'
        case 'left':
          directionKey = 'up'
        case 'up':
          directionKey = 'right'

  valueMapping = dict(zip(visitedIds, list(range(1, size**2 + 1))))
  output = [[0]*size for _ in range(size)]

  for row in range(len(output)):
    for col in range(len(output[row])):
      output[row][col] = valueMapping[(row, col)]

  return(output)

for row in spiralMatrix(3):
  print(row)

for row in spiralMatrix(4):
  print(row)

for row in spiralMatrix(5):
  print(row)

