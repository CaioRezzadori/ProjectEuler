from projecteuler.game_of_life import tick


def live_cells_with_zero_live_neighbors_die():
    assert tick([[0, 0, 0], [0, 1, 0], [0, 0, 0]]) == [[0, 0, 0], [0, 0, 0], [0, 0, 0]]


def live_cells_with_only_one_live_neighbor_die():
    assert tick([[0, 0, 0], [0, 1, 0], [0, 1, 0]]) == [[0, 0, 0], [0, 0, 0], [0, 0, 0]]


def live_cells_with_two_live_neighbors_stay_alive():
    assert tick([[1, 0, 1], [1, 0, 1], [1, 0, 1]]) == [[0, 0, 0], [1, 0, 1], [0, 0, 0]]


def live_cells_with_three_live_neighbors_stay_alive():
    assert tick([[0, 1, 0], [1, 0, 0], [1, 1, 0]]) == [[0, 0, 0], [1, 0, 0], [1, 1, 0]]


def dead_cells_with_three_live_neighbors_become_alive():
    assert tick([[1, 1, 0], [0, 0, 0], [1, 0, 0]]) == [[0, 0, 0], [1, 1, 0], [0, 0, 0]]


def live_cells_with_four_or_more_neighbors_die():
    assert tick([[1, 1, 1], [1, 1, 1], [1, 1, 1]]) == [[1, 0, 1], [0, 0, 0], [1, 0, 1]]


def bigger_matrix():
    assert tick(
        [
            [1, 1, 0, 1, 1, 0, 0, 0],
            [1, 0, 1, 1, 0, 0, 0, 0],
            [1, 1, 1, 0, 0, 1, 1, 1],
            [0, 0, 0, 0, 0, 1, 1, 0],
            [1, 0, 0, 0, 1, 1, 0, 0],
            [1, 1, 0, 0, 0, 1, 1, 1],
            [0, 0, 1, 0, 1, 0, 0, 1],
            [1, 0, 0, 0, 0, 0, 1, 1],
        ]
    ) == [
        [1, 1, 0, 1, 1, 0, 0, 0],
        [0, 0, 0, 0, 0, 1, 1, 0],
        [1, 0, 1, 1, 1, 1, 0, 1],
        [1, 0, 0, 0, 0, 0, 0, 1],
        [1, 1, 0, 0, 1, 0, 0, 1],
        [1, 1, 0, 1, 0, 0, 0, 1],
        [1, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 1, 1],
    ]
