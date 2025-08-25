# https://exercism.org/tracks/python/exercises/largest-series-product


class largest_series_product:
    def __init__(self, series: str, span: int) -> None:
        """
        Construct problem inputs (span and series)
        """
        self.series = series
        self.span = span

    def largest_product(self) -> int:
        """
        Calculates largest product with size of span of adjacent digits of series
        """
        if self.series and not self.series.isdigit():
            raise ValueError("digits input must only contain digits")

        if len(str(self.series)) < self.span:
            raise ValueError("span must not exceed string length")

        if self.span < 0:
            raise ValueError("span must not be negative")

        if not self.span:
            return 0

        products: list[int] = []
        for idx, digit in enumerate(self.series):
            if idx + self.span <= len(self.series):
                for i in range(self.span):
                    if not i:
                        products.append(int(digit))
                    else:
                        products[idx] *= int(self.series[idx + i])
        return max(products)
