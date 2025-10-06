import pytest

from projecteuler.satellite import tree_from_traversals


def test_empty_tree():
    preorder = []
    inorder = []

    expected = {}
    assert tree_from_traversals(preorder, inorder) == expected


def test_tree_with_one_item():
    preorder = ["a"]
    inorder = ["a"]

    expected = {"v": "a", "l": {}, "r": {}}
    assert tree_from_traversals(preorder, inorder) == expected


def test_tree_with_many_items():
    preorder = ["a", "i", "x", "f", "r"]
    inorder = ["i", "a", "f", "x", "r"]

    expected = {
        "v": "a",
        "l": {"v": "i", "l": {}, "r": {}},
        "r": {
            "v": "x",
            "l": {"v": "f", "l": {}, "r": {}},
            "r": {"v": "r", "l": {}, "r": {}},
        },
    }
    assert tree_from_traversals(preorder, inorder) == expected


def test_tree_with_many_items_2():
    preorder = ["a", "i", "x", "f", "r"]
    inorder = ["a", "x", "i", "f", "r"]

    expected = {
        "v": "a",
        "l": {},
        "r": {
            "v": "i",
            "l": {
                "v": "x",
                "l": {},
                "r": {},
            },
            "r": {
                "v": "f",
                "l": {},
                "r": {
                    "v": "r",
                    "l": {},
                    "r": {},
                },
            },
        },
    }
    assert tree_from_traversals(preorder, inorder) == expected


def test_tree_with_many_items_3():
    preorder = ["a", "i", "x", "f", "r"]
    inorder = ["a", "f", "x", "r", "i"]

    expected = {
        "v": "a",
        "l": {},
        "r": {
            "v": "i",
            "l": {
                "v": "x",
                "l": {
                    "v": "f",
                    "l": {},
                    "r": {},
                },
                "r": {
                    "v": "r",
                    "l": {},
                    "r": {},
                },
            },
            "r": {},
        },
    }
    assert tree_from_traversals(preorder, inorder) == expected


def test_reject_traversals_of_different_length():
    preorder = ["a", "b"]
    inorder = ["b", "a", "r"]

    with pytest.raises(ValueError) as exc_info:
        tree_from_traversals(preorder, inorder)
    assert exc_info.type == ValueError
    assert str(exc_info.value) == "traversals must have the same length"


def test_reject_inconsistent_traversals_of_same_length():
    preorder = ["x", "y", "z"]
    inorder = ["a", "b", "c"]

    with pytest.raises(ValueError) as exc_info:
        tree_from_traversals(preorder, inorder)
    assert exc_info.type == ValueError
    assert str(exc_info.value) == "traversals must have the same elements"


def test_reject_traversals_with_repeated_items():
    preorder = ["a", "b", "a"]
    inorder = ["b", "a", "a"]

    with pytest.raises(ValueError) as exc_info:
        tree_from_traversals(preorder, inorder)
    assert exc_info.type == ValueError
    assert str(exc_info.value) == "traversals must contain unique items"
