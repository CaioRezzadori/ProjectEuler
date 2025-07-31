def tick(grid: list[list[int]]):
    nrows = len(grid)
    ncols = len(grid[0])
    neighboor_counter = [[0] * nrows for _ in range(ncols)]
    new_grid = grid.copy()
    for row in range(nrows):
        for col in range(ncols):
            counter = 0
            if row - 1 >= 0:
                counter += neighboor_counter[row - 1][col]
            if row + 1 < nrows:
                counter += neighboor_counter[row + 1][col]
            if col - 1 >= 0:
                counter += neighboor_counter[row][col - 1]
            if col + 1 < ncols:
                counter += neighboor_counter[row][col + 1]

            if counter == 3:
                new_grid[row][col] = 1
            if new_grid[row][col] == 1 and counter == 2:
                continue
            else:
                new_grid[row][col] = 0
    return new_grid
