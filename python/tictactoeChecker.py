# Check if row is filled
def isRowWinner(field):
    fieldStr = ''.join(field)
    if("XXX" in fieldStr or "OOO" in fieldStr):
        return(True)
    return(False)

isRowWinner(['X', 'X', 'X', 'O', 'O', '.', '.', '.', '.'])
isRowWinner(['X', 'O', 'X', 'O', 'O', 'O', 'X', '.', 'X'])
isRowWinner(['X', 'O', '.', '.', 'X', 'O', '.', '.', 'X'])