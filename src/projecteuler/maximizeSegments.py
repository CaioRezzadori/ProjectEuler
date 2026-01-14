def solve(n: int, cuts: tuple[int, int, int]) -> int:
    if n == 0:
        return 0
    counters = [0]
    for seg in cuts:
        if n - seg >= 0:
            counters.append(solve(n - seg, cuts) + 1)
    return max(counters)


def solveMemoization(n: int, cuts: tuple[int, int, int]):
    cache = {0: 0}

    def solve_rec(n: int, cuts: tuple[int, int, int]) -> int:
        if n == 0:
            return 0
        if n in cache.keys():
            return cache[n]
        counters = [0]
        for seg in cuts:
            if n - seg >= 0:
                counters.append(solve_rec(n - seg, cuts) + 1)
        cache[n] = max(counters)
        return cache[n]

    return solve_rec(n, cuts)


# def solve(params: tuple[int, int, int, int]) -> int:
#     def solve_rec(
#         params: tuple[int, int, int, int],
#         cost: int,
#         counter: int,
#     ) -> int:
#         n, x, y, z = params
#         if cost == n:
#             return counter
#         counters = [0]
#         for seg in (x, y, z):
#             if cost + seg <= n:
#                 counters.append(solve_rec(params, cost + seg, counter + 1))
#         return max(counters)

#     return solve_rec(params, 0, 0)
