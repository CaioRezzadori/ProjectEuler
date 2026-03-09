from functools import lru_cache


@lru_cache(maxsize=None)
def binomialCoeff(n: int, k: int) -> int:
    if k == 0 or k == n:
        return 1

    return binomialCoeff(n - 1, k) + binomialCoeff(n - 1, k - 1)


def binomialCoeffDP(n: int, k: int) -> int:
    memo = [[0 for _ in range(0, k + 1)] for _ in range(0, n + 1)]

    for i in range(1, n + 1):
        for j in range(0, min(i, k) + 1):

            if j == 0 or j == i:
                memo[i][j] = 1

            else:
                memo[i][j] = memo[i - 1][j] + memo[i - 1][j - 1]

    return memo[n][k]
