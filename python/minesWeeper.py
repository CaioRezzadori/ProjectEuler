def minesWeeper(board):
    boardLength = len(board[0])
    if(any([len(x) != boardLength for x in board])):
        return(-1) # Dimension error

    boardStr = "".join(board)
    mineCounter = 0
    directions = [-1, 1, # Left, Right
                  boardLength, -boardLength, # Down, Up
                  boardLength + 1, boardLength - 1, # Diag down right, Diag down left
                  -boardLength + 1, -boardLength - 1] # Diag up right, Diag up left
    
    for i in range(0, len(boardStr)):
        if(boardStr[i] == "*"):
            continue
        for dir in directions:
            try:
                mineCounter = mineCounter + 1 if (boardStr[i + dir] == "*" and \
                                                  i + dir >= 0)\
                              else mineCounter
            except IndexError:
                continue

        if(mineCounter > 0):
            boardStr = boardStr[:i] + str(mineCounter) + boardStr[i + 1:]
            mineCounter = 0
    return([boardStr[x:x + boardLength] for x in range(0, len(boardStr), boardLength)])


board = ["·*·*·",
         "··*··",
         "··*··",
         "·····"]

minesWeeper(board) == ["1*3*1",
                       "13*31",
                       "·2*2·",
                       "·111·"]

minesWeeper(["·*·*·",
             "··*··",
             "··*··",
             "··*··"]) == ["1*3*1",
                           "13*31",
                           "·3*3·",
                           "·2*2·"]