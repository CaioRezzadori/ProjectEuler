import unittest

from projecteuler.zebra_puzzle import (
    drinks_water,
    owns_zebra,
)


class ZebraPuzzleTest(unittest.TestCase):
    def test_resident_who_drinks_water(self):
        self.assertEqual(drinks_water(), "Norwegian")

    def test_resident_who_owns_zebra(self):
        self.assertEqual(owns_zebra(), "Japanese")
