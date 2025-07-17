# https://exercism.org/tracks/r/exercises/zebra-puzzle

from itertools import permutations


def solve_puzzle() -> dict[str, list[str]]:
    """
    Solves Zebra Puzzle and returns dict with sorted categories by the rules
    """

    houses_possibilities = ["red", "green", "ivory", "yellow", "blue"]
    nationalities_possibilities = [
        "Englishman",
        "Spaniard",
        "Ukrainian",
        "Norwegian",
        "Japanese",
    ]

    drinks_possibilities = ["coffee", "tea", "milk", "orange juice", "water"]
    pets_possibilities = ["dog", "snail", "fox", "horse", "zebra"]
    hobbies_possibilities = ["chess", "football", "dancing", "reading", "painting"]

    for house_id in permutations(range(5)):
        houses = [houses_possibilities[i] for i in house_id]

        # The green house is immediately to the right of the ivory house.
        if (
            houses.index("green") - 1 < 0
            or houses[houses.index("green") - 1] != "ivory"
        ):
            continue
        for nationality_id in permutations(range(5)):
            nationalities = [nationalities_possibilities[i] for i in nationality_id]

            # The Norwegian lives in the first house.
            if nationalities[0] != "Norwegian":
                continue
            # The Englishman lives in the red house.
            if houses[nationalities.index("Englishman")] != "red":
                continue
            # The Norwegian lives next to the blue house.
            if abs(nationalities.index("Norwegian") - houses.index("blue")) != 1:
                continue

            for drink_id in permutations(range(5)):
                drinks = [drinks_possibilities[i] for i in drink_id]

                # The person in the middle house drinks milk.
                if drinks[2] != "milk":
                    continue
                # The Ukrainian drinks tea.
                if drinks[nationalities.index("Ukrainian")] != "tea":
                    continue
                # The person in the green house drinks coffee.
                if drinks[houses.index("green")] != "coffee":
                    continue

                for hobby_id in permutations(range(5)):
                    hobbies = [hobbies_possibilities[i] for i in hobby_id]

                    # The person who plays football drinks orange juice.
                    if drinks[hobbies.index("football")] != "orange juice":
                        continue
                    # The person in the yellow house is a painter.
                    if hobbies[houses.index("yellow")] != "painting":
                        continue
                    # The Japanese person plays chess.
                    if hobbies[nationalities.index("Japanese")] != "chess":
                        continue

                    for pet_id in permutations(range(5)):
                        pets = [pets_possibilities[i] for i in pet_id]

                        # The Spaniard owns the dog.
                        if pets[nationalities.index("Spaniard")] != "dog":
                            continue
                        # The snail owner likes to go dancing.
                        if hobbies[pets.index("snail")] != "dancing":
                            continue
                        # The person who enjoys reading lives in the house next to the person with the fox.
                        if abs(hobbies.index("reading") - pets.index("fox")) != 1:
                            continue
                        # The painter's house is next to the house with the horse.
                        if abs(hobbies.index("painting") - pets.index("horse")) != 1:
                            continue

                        return {
                            "houses": houses,
                            "nationalities": nationalities,
                            "drinks": drinks,
                            "pets": pets,
                            "hobbies": hobbies,
                        }
    return {}


def drinks_water() -> str | None:
    """
    Solves puzzle and gives nationality of who drinks water
    """
    result = solvePuzzle()
    id__ = result["drinks"].index("water")
    return result["nationalities"][id__]


def owns_zebra():
    """
    Solves puzzle and gives nationality of who owns the zebra
    """
    result = solvePuzzle()
    id__ = result["pets"].index("zebra")
    return result["nationalities"][id__]
