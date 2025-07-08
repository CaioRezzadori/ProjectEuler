from typing import Any

class Rational:
    """
    Define rational number and it's operations
    """

    def __init__(self, numerator: int | float, denominator: int | float) -> None:
        if denominator == 0:
            raise ValueError("denominator must be different from 0")
        self.numerator = numerator
        self.denominator = denominator

    def __add__(self, other: "Rational | int | float") -> "Rational":
        if isinstance(other, Rational):
            return Rational(
                self.numerator * other.denominator + other.numerator * self.denominator,
                self.denominator * other.denominator,
            )
        return Rational(self.numerator + other * self.denominator, self.denominator)

    def __radd__(self, other: int | float) -> "Rational":
        return Rational(self.numerator + other * self.denominator, self.denominator)

    def __sub__(self, other: "Rational | int | float") -> "Rational":
        if isinstance(other, Rational):
            return Rational(
                self.numerator * other.denominator - other.numerator * self.denominator,
                self.denominator * other.denominator,
            )
        return Rational(-self.numerator + other * self.denominator, self.denominator)

    def __rsub__(self, other: int | float) -> "Rational":
        return Rational(-self.numerator + other * self.denominator, self.denominator)

    def __eq__(self, other: "Rational | Any") -> bool:
        if isinstance(other, Rational):
            return (self - other).numerator == 0
        if isinstance(other, float):
            return (self.numerator - other) == 0
        raise ValueError("error")

    def __mul__(self, other: "Rational | int | float") -> "Rational":
        if isinstance(other, Rational):
            return Rational(
                self.numerator * other.numerator, self.denominator * other.denominator
            )
        return Rational(other * self.numerator, self.denominator)

    def __truediv__(self, other: "Rational | int | float") -> "Rational":
        if isinstance(other, Rational):
            return Rational(
                self.numerator * other.denominator, self.denominator * other.numerator
            )
        return Rational(self.numerator, self.denominator * other)

    def __rtruediv__(self, other: int | float) -> "Rational":
        return Rational(other * self.denominator, self.numerator)

    def __abs__(self) -> "Rational":
        return Rational(abs(self.numerator), abs(self.denominator))

    def __pow__(self, other: "Rational | int | float") -> "Rational":
        if isinstance(other, Rational):
            if other.numerator * other.denominator > 0:
                return Rational(
                    self.numerator ** (other.numerator / other.denominator),
                    self.denominator ** (other.numerator / other.denominator),
                )
            elif self.numerator != 0 and other.numerator == 0:
                return Rational(1, 1)
            return Rational(
                self.denominator ** (other.numerator / other.denominator),
                self.numerator ** (other.numerator / other.denominator),
            )
        return Rational(self.numerator**other, self.denominator**other)

    def __rpow__(self, other: int | float) -> float:
        return other ** (self.numerator / self.denominator)

    def __round__(self) -> "Rational":
        return Rational(round(self.numerator, 8), round(self.denominator, 2))
