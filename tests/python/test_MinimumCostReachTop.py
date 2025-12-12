from projecteuler.MinimumCostReachTop import solve_minimum_cost, solve_minimum_cost_DP


def test_reach_top_1():
    assert solve_minimum_cost([10, 15, 20]) == 15


def test_reach_top_2():
    assert solve_minimum_cost([1, 100, 1, 1, 1, 100, 1, 1, 100, 1]) == 6


def test_reach_top_DP_1():
    assert solve_minimum_cost_DP([10, 15, 20]) == 15


def test_reach_top_DP_2():
    assert solve_minimum_cost_DP([1, 100, 1, 1, 1, 100, 1, 1, 100, 1]) == 6
