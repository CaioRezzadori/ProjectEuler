# Check if row is filled
def isRowWinner(field):
    fieldStr = ''.join(field)
    return any([fieldStr[x: x + 3] in ["OOO", "XXX"] \
           for x in range(0, len(fieldStr), 3)])

isRowWinner(['X', 'X', 'X',
             'O', 'O', '.',
             '.', '.', '.'])
isRowWinner(['X', 'O', 'X',
             'O', 'O', 'O',
             'X', '.', 'X'])
isRowWinner(['X', 'O', '.',
             '.', 'X', 'O',
             '.', '.', 'X'])
isRowWinner(['X', 'O', 'X',
             'X', 'X', 'O',
             '.', '.', 'O'])
isRowWinner(['X', 'O', 'X',
             'X', 'X', 'O',
             'O', 'O', 'O'])

