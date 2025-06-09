def minesWeeper(board):
    boardLength = len(board[0])
    if(any([len(x) != boardLength for x in board])):
        return(-1) # Dimension error

    mineCounter = 0
    directions = [[0, -1], [0, 1], # Left, Right
                  [1, 0], [-1, 0], # Down, Up
                  [1, 1], [1, -1], # Diag down right, Diag down left
                  [-1, 1], [-1, - 1]] # Diag up right, Diag up left
    
    for i in range(0, len(board)):
        for j in range(0, boardLength):
            for dir in directions:
                if(board[i][j] == "*" or i + dir[0] < 0 or j + dir[1] < 0):
                    continue
                try:
                    mineCounter = mineCounter + 1 if (board[i + dir[0]][j + dir[1]] == "*")\
                                  else mineCounter
                except IndexError:
                    continue

            if(mineCounter > 0):
                board[i] = board[i][:j] + str(mineCounter) + board[i][j + 1:]
                mineCounter = 0
    return(board)

board = ["·*·*·",
         "··*··",
         "··*··",
         "·····"]

assert minesWeeper(board) == ["1*3*1",
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

minesWeeper(["·*·*·",
             "*·*··",
             "··*··",
             "··*··"]) == ["2*3*1",
                           "*4*31",
                           "14*3·",
                           "·2*2·"]



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
                mineCounter = mineCounter + 1 if (boardStr[i + dir] == "*")\
                              else mineCounter
            except IndexError:
                continue

        if(mineCounter > 0):
            boardStr = boardStr[:i] + str(mineCounter) + boardStr[i + 1:]
            mineCounter = 0
    return([boardStr[x:x + boardLength] for x in range(0, len(boardStr), boardLength)])

