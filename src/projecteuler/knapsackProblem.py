def knapsack(W, values, weights):
    n = len(values)
    dp = [[0] * (W + 1) for _ in range(1, n + 1)]
    for i in range(n):
        for j in range(W):
            if i > 0 and j > 0:
                pick = 0
                if weights[i - 1] <= j:
                    pick = values[i] + dp[i - 1][j - weights[i]]
                notPick = dp[i - 1][j]
                dp[i][j] = max(pick, notPick)
    return dp[n - 1][W - 1]


knapsack(W=10, values=[10, 40, 30, 50], weights=[5, 4, 6, 4])
knapsack(W=4, values=[1, 2, 3], weights=[4, 5, 1])
