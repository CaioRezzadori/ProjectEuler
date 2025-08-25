class Towers:
    def __init__(self, N: int, x_i: list[int], k_i: list[int]):
        self.N = N
        self.x_i = x_i
        self.k_i = k_i

    def activeTowers(self) -> list[int]:
        maxSignalTowerId = self.k_i.index(min(self.k_i))
        active = [maxSignalTowerId]
        # Right
        currentId = maxSignalTowerId
        for towerIdR in range(maxSignalTowerId + 1, self.N):
            if self.k_i[towerIdR] < self.k_i[currentId] + abs(
                self.x_i[currentId] - self.x_i[towerIdR]
            ):
                active.append(towerIdR)
                currentId = towerIdR
        # Left
        currentId = maxSignalTowerId
        for towerIdL in range(maxSignalTowerId - 1, 0, -1):
            if self.k_i[towerIdL] < self.k_i[currentId] + abs(
                self.x_i[currentId] - self.x_i[towerIdL]
            ):
                active.append(towerIdL)
                currentId = towerIdL
        return active

    def shutDownTowers(self) -> list[int]:
        active = self.activeTowers()
        return list(set(range(0, self.N)) - set(active))
