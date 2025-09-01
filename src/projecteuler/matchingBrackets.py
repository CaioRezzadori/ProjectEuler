class MatchingBrackets:
    def __init__(self, input_string: str) -> None:
        self.input_string = input_string

    def __stack__(self):
        bracks: list[str] = []
        for s in self.input_string:
            if s in ("(", "[", "{"):
                bracks.append(s)
            elif s in (")", "]", "}"):
                if len(bracks) > 0 and (bracks[-1], s) in (
                    ("(", ")"),
                    ("[", "]"),
                    ("{", "}"),
                ):
                    del bracks[-1]
                else:
                    return False
        return len(bracks) == 0

    def __recursive__(self) -> bool:
        def __recursive_aux__(input_string: str, brack: str = "") -> bool:
            if len(input_string) == 0:
                return len(brack) == 0
            if input_string[0] in ("(", "[", "{"):
                return __recursive_aux__(input_string[1:], brack + input_string[0])
            elif input_string[0] in (")", "]", "}"):
                if len(brack) > 0 and (brack[-1], input_string[0]) in (
                    ("(", ")"),
                    ("[", "]"),
                    ("{", "}"),
                ):
                    return __recursive_aux__(input_string[1:], brack[:-1])
                else:
                    return False
            return __recursive_aux__(input_string[1:], brack)

        return __recursive_aux__(self.input_string)

    def __state__(self) -> bool:
        state = 0

        for char in self.input_string:
            if char in "([{":
                state += 1
            if char in ")]}":
                state -= 1

            if state < 0:
                return False

        return 0 == state


def is_paired(input_string: str, engine: str = "stack"):
    match engine:
        case "stack":
            return MatchingBrackets(input_string).__stack__()
        case "recursive":
            return MatchingBrackets(input_string).__recursive__()
        case "state":
            return MatchingBrackets(input_string).__state__()
        case _:
            return False


# def is_paired(input_string: str):
#     return is_paired_stack(input_string)
