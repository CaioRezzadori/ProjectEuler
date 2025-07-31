def neighbor_count(grid: list[list[int]], row: int, col: int) -> int:
    """
    Count alive cells in the neighbor of element grid[row][col]
    """
    counter = 0
    for i in (-1, 0, 1):
        for j in (-1, 0, 1):
            if i == j == 0:
                continue
            if row + i < 0 or col + j < 0:
                continue
            if row + i >= len(grid) or col + j >= len(grid[0]):
                continue
            counter += grid[row + i][col + j]
    return counter


def tick(grid: list[list[int]]) -> list[list[int]]:
    """
    Iterate game of life given a grid of alive (represented by 1) and dead
    (represented by 0) cells
    """
    nrows = len(grid)
    ncols = len(grid[0])
    new_grid = [x[:] for x in grid]
    for row in range(nrows):
        for col in range(ncols):
            counter = neighbor_count(grid, row, col)
            if counter == 3:
                new_grid[row][col] = 1
                continue
            if new_grid[row][col] == 1 and counter == 2:
                continue
            else:
                new_grid[row][col] = 0

    return new_grid
