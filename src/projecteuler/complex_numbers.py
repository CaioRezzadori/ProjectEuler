# https://exercism.org/tracks/python/exercises/complex-numbers

import math


class ComplexNumber:
    """
    Define complex numbers and it's operations
    """

    def __init__(self, real: int | float, imaginary: int | float) -> None:
        self.real = real
        self.imaginary = imaginary

    def __eq__(self, other: "ComplexNumber | int | float"):
        if not isinstance(other, ComplexNumber):
            return NotImplemented

        return self.real == other.real and self.imaginary == other.imaginary

    def __round__(self):
        return ComplexNumber(round(self.real, 2), round(self.imaginary, 2))

    def __add__(self, other: "ComplexNumber | int | float") -> "ComplexNumber":
        if isinstance(other, ComplexNumber):
            return ComplexNumber(
                self.real + other.real, self.imaginary + other.imaginary
            )
        return ComplexNumber(self.real + other, self.imaginary)

    def __radd__(self, other: int | float) -> "ComplexNumber":
        return ComplexNumber(self.real + other, self.imaginary)

    def __sub__(self, other: "ComplexNumber | int | float") -> "ComplexNumber":
        if isinstance(other, ComplexNumber):
            return ComplexNumber(
                self.real - other.real, self.imaginary - other.imaginary
            )
        return ComplexNumber(self.real - other, self.imaginary)

    def __rsub__(self, other: int | float) -> "ComplexNumber":
        return ComplexNumber(other - self.real, -self.imaginary)

    def __mul__(self, other: "ComplexNumber | int | float") -> "ComplexNumber":
        if isinstance(other, ComplexNumber):
            return ComplexNumber(
                self.real * other.real - self.imaginary * other.imaginary,
                self.real * other.imaginary + self.imaginary * other.real,
            )
        return ComplexNumber(self.real * other, self.imaginary * other)

    def __rmul__(self, other: int | float) -> "ComplexNumber":
        return ComplexNumber(self.real * other, self.imaginary * other)

    def __abs__(self) -> float:
        return (self.real**2 + self.imaginary**2) ** (1 / 2)

    def __truediv__(self, other: "ComplexNumber | int | float") -> "ComplexNumber":
        if isinstance(other, ComplexNumber):
            return ComplexNumber(
                (self.real * other.real + self.imaginary * other.imaginary)
                / (abs(other) ** 2),
                (self.imaginary * other.real - self.real * other.imaginary)
                / (abs(other) ** 2),
            )
        return ComplexNumber(self.real / other, self.imaginary / other)

    def __rtruediv__(self, other: int | float) -> "ComplexNumber":
        return ComplexNumber(other, 0) / ComplexNumber(self.real, self.imaginary)

    def conjugate(self) -> "ComplexNumber":
        return ComplexNumber(self.real, -self.imaginary)

    def exp(self):
        return ComplexNumber(math.exp(self.real), 0) * ComplexNumber(
            math.cos(self.imaginary), math.sin(self.imaginary)
        )
