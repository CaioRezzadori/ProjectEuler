# These tests are auto-generated with test data from:
# https://github.com/exercism/problem-specifications/tree/main/exercises/matching-brackets/canonical-data.json
# File last updated on 2023-07-19


from projecteuler.matchingBrackets import is_paired


def test_paired_square_brackets():
    assert is_paired("[]")


def test_empty_string():
    assert is_paired("")


def test_unpaired_brackets():
    assert not is_paired("[[")


def test_wrong_ordered_brackets():
    assert not is_paired("}{")


def test_wrong_closing_bracket():
    assert not is_paired("{]")


def test_paired_with_whitespace():
    assert is_paired("{ }")


def test_partially_paired_brackets():
    assert not is_paired("{[])")


def test_simple_nested_brackets():
    assert is_paired("{[]}")


def test_several_paired_brackets():
    assert is_paired("{}[]")


def test_paired_and_nested_brackets():
    assert is_paired("([{}({}[])])")


def test_unopened_closing_brackets():
    assert not is_paired("{[)][]}")


def test_unpaired_and_nested_brackets():
    assert not is_paired("([{])")


def test_paired_and_wrong_nested_brackets():
    assert not is_paired("[({]})")


def test_paired_and_wrong_nested_brackets_but_innermost_are_correct():
    assert not is_paired("[({}])")


def test_paired_and_incomplete_brackets():
    assert not is_paired("{}[")


def test_too_many_closing_brackets():
    assert not is_paired("[]]")


def test_early_unexpected_brackets():
    assert not is_paired(")()")


def test_early_mismatched_brackets():
    assert not is_paired("{)()")


def test_math_expression():
    assert is_paired("(((185 + 223.85) * 15) - 543)/2")


def test_complex_latex_expression():
    assert is_paired(
        "\\left(\\begin{array}{cc} \\frac{1}{3} & x\\\\ \\mathrm{e}^{x} &... x^2 \\end{array}\\right)"
    )
