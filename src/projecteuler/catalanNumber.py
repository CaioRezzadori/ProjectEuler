from functools import lru_cache

@lru_cache(maxsize=None)
def catalanNumRec(n:int) -> int:
    if n == 0:
        return 1
    val = 0
    for i in range(0, n):
        val += catalanNumRec(i)*catalanNumRec(n - i - 1)
    return val


def catalanNumDp(n: int) -> int:
    memo = [0]*(n + 1)
    memo[0] = 1
    for i in range(0, n + 1):
        for j in range(0, i):
            memo[i] += memo[j]*memo[i - j - 1]

    return memo[n]
