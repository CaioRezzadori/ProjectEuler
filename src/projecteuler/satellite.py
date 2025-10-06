from typing import Any


def set_node(
    tree: dict[str, Any], element: str, inorder_traversal: list[str]
) -> dict[str, Any] | Any:
    if "v" in list(tree.keys()):
        tree_val = tree["v"]
    else:
        tree["v"] = element
        tree["l"] = {}
        tree["r"] = {}
        return tree
    if inorder_traversal.index(element) < inorder_traversal.index(tree_val):
        return set_node(tree["l"], element, inorder_traversal)
    return set_node(tree["r"], element, inorder_traversal)


def tree_from_traversals(preorder: list[str], inorder: list[str]) -> dict[str, Any]:
    def error_randling(param1: list[str], param2: list[str]) -> None:
        len_param1 = len(param1)
        len_param2 = len(param2)
        if sorted(param1) != sorted(param2):
            if len_param1 != len_param2:
                raise ValueError("traversals must have the same length")
            raise ValueError("traversals must have the same elements")
        if len_param1 != len(set(param1)) or len_param2 != len(set(param2)):
            raise ValueError("traversals must contain unique items")

    error_randling(preorder, inorder)

    tree: dict[str, Any] = {}

    for element in preorder:
        set_node(tree, element, inorder)

    return tree
