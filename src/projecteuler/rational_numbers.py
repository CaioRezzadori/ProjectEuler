class Rational:
    def __init__(self, numerator: int | float, denominator: int | float):
        self.numerator = numerator
        self.denominator = denominator

    def __add__(self, other: "Rational | int | float"):
        if isinstance(other, Rational):
            return Rational(
                self.numerator * other.denominator + other.numerator * self.denominator,
                self.denominator * other.denominator,
            )
        return Rational(self.numerator + other * self.denominator, self.denominator)

    def __radd__(self, other: int | float):
        return Rational(self.numerator + other * self.denominator, self.denominator)

    def __sub__(self, other: "Rational | int | float"):
        if isinstance(other, Rational):
            return Rational(
                self.numerator * other.denominator - other.numerator * self.denominator,
                self.denominator * other.denominator,
            )
        return Rational(-self.numerator + other * self.denominator, self.denominator)

    def __rsub__(self, other: int | float):
        return Rational(-self.numerator + other * self.denominator, self.denominator)

    def __mul__(self, other: "Rational | int | float"):
        if isinstance(other, Rational):
            return Rational(
                self.numerator * other.numerator, self.denominator * other.denominator
            )
        return Rational(other * self.numerator, self.denominator)

    def __eq__(self, other: "Rational | float"):
        return (self - other).numerator == 0
