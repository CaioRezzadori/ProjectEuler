def solve_minimum_cost(cost_list: list[int]) -> int:
    def solve_minimum_cost_rec(cost_list: list[int], current_cost: int) -> int:
        if not cost_list[0]:
            return current_cost

        cost_1 = solve_minimum_cost_rec(
            cost_list[1:],
            current_cost + cost_list[0],
        )
        cost_2 = solve_minimum_cost_rec(
            cost_list[2:],
            current_cost + cost_list[1],
        )

        return min(cost_1, cost_2)

    return solve_minimum_cost_rec(cost_list + [0, 0], 0)


def solve_minimum_cost_DP(cost_list: list[int]) -> int:
    size = len(cost_list)
    dp = [0 for _ in range(size)]
    dp[0] = cost_list[0]
    dp[1] = cost_list[1]

    for i in range(2, size):
        dp[i] = cost_list[i] + min([dp[i - 1], dp[i - 2]])

    return min([dp[-1], dp[-2]])
