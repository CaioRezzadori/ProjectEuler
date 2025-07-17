# Your task is to solve the Zebra Puzzle to find the answer to these two questions:

# Which of the residents drinks water?
# Who owns the zebra?


# Additionally, each of the five houses is painted a different color, and their
# inhabitants are of different national extractions, own different pets, drink
# different beverages and engage in different hobbies.


from itertools import permutations


def solvePuzzle():
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
        if (
            houses.index("green") - 1 < 0
            or houses[houses.index("green") - 1] != "ivory"
        ):
            continue
        for nationality_id in permutations(range(5)):
            nationalities = [nationalities_possibilities[i] for i in nationality_id]
            if nationalities[0] != "Norwegian":
                continue
            if houses[nationalities.index("Englishman")] != "red":
                continue
            if abs(nationalities.index("Norwegian") - houses.index("blue")) != 1:
                continue
            for drink_id in permutations(range(5)):
                drinks = [drinks_possibilities[i] for i in drink_id]
                if drinks[2] != "milk":
                    continue
                if drinks[nationalities.index("Ukrainian")] != "tea":
                    continue
                if drinks[houses.index("green")] != "coffee":
                    continue
                for hobby_id in permutations(range(5)):
                    hobbies = [hobbies_possibilities[i] for i in hobby_id]
                    if drinks[hobbies.index("football")] != "orange juice":
                        continue
                    if hobbies[houses.index("yellow")] != "painting":
                        continue
                    if hobbies[nationalities.index("Japanese")] != "chess":
                        continue
                    for pet_id in permutations(range(5)):
                        pets = [pets_possibilities[i] for i in pet_id]
                        if pets[nationalities.index("Spaniard")] != "dog":
                            continue
                        if hobbies[pets.index("snail")] != "dancing":
                            continue
                        if abs(hobbies.index("reading") - pets.index("fox")) != 1:
                            continue
                        if abs(hobbies.index("painting") - pets.index("horse")) != 1:
                            continue

                        return {
                            "houses": houses,
                            "nationalities": nationalities,
                            "drinks": drinks,
                            "pets": pets,
                            "hobbies": hobbies,
                        }


def drinks_water():
    result = solvePuzzle()
    id = result["drinks"].index("water")
    return result["nationalities"][id]


def owns_zebra():
    result = solvePuzzle()
    id = result["pets"].index("zebra")
    return result["nationalities"][id]
