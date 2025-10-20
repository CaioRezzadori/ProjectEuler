def nRowPascalTriangle(n: int):
    if n == 1:
        return [1]
    if n == 2:
        return [1, 1]
    row = [1 for _ in range(0, n)]
    last_row = nRowPascalTriangle(n - 1)
    for i in range(1, n - 1):
        row[i] = last_row[i - 1] + last_row[i]

    return row


def nRowPascalTriangleTabulation(n: int):
    rows = [[1], [1, 1]] + [[1 for _ in range(0, i)] for i in range(3, n + 1)]
    # Bottom-up approach
    for idx in range(2, n):
        last_row = rows[idx - 1]
        for j in range(1, idx):
            rows[idx][j] = last_row[j - 1] + last_row[j]

    return rows[n - 1]
