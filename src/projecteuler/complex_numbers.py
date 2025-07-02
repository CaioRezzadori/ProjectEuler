class ComplexNumber:
    def __init__(self, real: float, imaginary: float) -> None:
        self.real = real
        self.imaginary = imaginary

    def __add__(self, other: "ComplexNumber | int | float") -> "ComplexNumber":
        # if isinstance(other, ComplexNumber):
        return ComplexNumber(self.real + other.real, self.imaginary + self.imaginary)

    # else:
    # return ComplexNumber(self.real + other, self.imaginary)

    def __sub__(self, other: "ComplexNumber | int | float") -> "ComplexNumber":
        if isinstance(other, ComplexNumber):
            return ComplexNumber(
                self.real - other.real, self.imaginary + self.imaginary
            )
        else:
            return ComplexNumber(self.real - other, self.imaginary)

    def __mul__(self, other: "ComplexNumber | int | float") -> "ComplexNumber":
        if isinstance(other, ComplexNumber):
            return ComplexNumber(
                self.real * other.real - self.imaginary * other.imaginary,
                self.real * other.imaginary + self.imaginary * other.real,
            )
        else:
            return ComplexNumber(self.real * other, self.imaginary * other)

    def __abs__(self) -> float:
        return (self.real**2 + self.imaginary**2) ** (1 / 2)

    def __div__(self, other: "ComplexNumber | int | float") -> "ComplexNumber":
        if isinstance(other, ComplexNumber):
            return ComplexNumber(
                (self.real * other.real + self.imaginary * other.imaginary)
                / (abs(other) ** 2),
                (self.real * other.imaginary + self.imaginary * other.real)
                / (abs(other) ** 2),
            )
        else:
            return ComplexNumber(self.real * other, self.imaginary * other)
