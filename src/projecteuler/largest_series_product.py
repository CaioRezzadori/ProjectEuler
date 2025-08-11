# https://exercism.org/tracks/python/exercises/largest-series-product


class largest_series_product:
    def __init__(self, series: str | int, span: int):
        """
        Construct problem inputs (span and series)
        """
        self.series = series
        self.span = span

    def largest_product(self):
        """
        Calculates largest product with size of span of adjacent digits of series
        """
        if (
            isinstance(self.series, str)
            and self.series != ""
            and not self.series.isdigit()
        ):
            raise ValueError("digits input must only contain digits")

        if len(str(self.series)) < self.span:
            raise ValueError("span must not exceed string length")

        if self.span < 0:
            raise ValueError("span must not be negative")

        if self.span == 0:
            return 0

        series = str(self.series)
        products: list[int] = []
        for idx, digit in enumerate(series):
            if idx + self.span <= len(series):
                for i in range(self.span):
                    if i == 0:
                        products.append(int(digit))
                    else:
                        products[idx] *= int(series[idx + i])
        return max(products)
