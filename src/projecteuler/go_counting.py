BLACK = "B"
WHITE = "W"
NONE = " "


class Board:
    """Count territories of each player in a Go game

    Args:
        board (list[str]): A two-dimensional Go board
    """

    def __init__(
        self,
        board: list[str],
    ):
        self.board = board

    def coordinate_is_valid(
        self,
        x: int,
        y: int,
    ) -> bool:
        return (0 <= x < len(self.board[0])) and (0 <= y < len(self.board))

    def get_element(self, x: int, y: int):
        return self.board[y][x] if self.coordinate_is_valid(x, y) else None

    def __walk__(
        self,
        x: int,
        y: int,
        visited_points: set[tuple[int, int]],
        stones: set[str],
    ) -> tuple[set[tuple[int, int]], set[str]]:
        stone = self.get_element(x, y)
        if (
            not self.coordinate_is_valid(x, y)
            or stone != NONE
            or (x, y) in visited_points
        ):
            if stone and stone != NONE:
                stones.update(stone)

            return visited_points, stones

        visited_points.update([(x, y)])

        for direction in ((0, 1), (0, -1), (1, 0), (-1, 0)):
            new_coord = (x + direction[0], y + direction[1])
            visited_points, stones = self.__walk__(*new_coord, visited_points, stones)

        return visited_points, stones

    def territory(self, x: int, y: int) -> tuple[str, set[tuple[int, int]]]:
        """Find the owner and the territories given a coordinate on
           the board

        Args:
            x (int): Column on the board
            y (int): Row on the board

        Returns:
            (str, set): A tuple, the first element being the owner
                        of that area.  One of "W", "B", "".  The
                        second being a set of coordinates, representing
                        the owner's territories.
        """
        if not self.coordinate_is_valid(x, y):
            raise ValueError("Invalid coordinate")

        if self.get_element(x, y) != NONE:
            return NONE, set()

        territory_point, stones = self.__walk__(x, y, set(), set())

        if len(stones) != 1:
            stone = NONE
        else:
            stone = list(stones)[0]

        return (stone, territory_point)

    def territories(self):
        """Find the owners and the territories of the whole board

        Args:
            none

        Returns:
            dict(str, set): A dictionary whose key being the owner
                        , i.e. "W", "B", "".  The value being a set
                        of coordinates owned by the owner.
        """
        territories_points: dict[str, set[tuple[int, int] | None]] = {
            BLACK: set(),
            WHITE: set(),
            NONE: set(),
        }
        for x in range(len(self.board[0])):
            for y in range(len(self.board)):
                if (x, y) in set().union(*territories_points.values()):
                    continue
                stone, terr = self.territory(x, y)
                territories_points[stone].update(terr)

        return territories_points
