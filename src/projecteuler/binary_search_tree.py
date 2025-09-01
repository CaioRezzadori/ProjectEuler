from __future__ import annotations
from typing import Optional


class TreeNode:

    def __init__(
        self,
        data: Optional[str],
        left: Optional[TreeNode] = None,  # Duvidas de tipagem
        right: Optional[TreeNode] = None,
    ) -> None:
        self.data = data
        self.left = left
        self.right = right

    def __str__(self) -> str:
        return f"TreeNode(data={self.data}, left={self.left}, right={self.right})"

    def insert(self, num: str) -> None:
        if not self.data:
            self.data = num
        elif float(num) <= float(self.data):
            if not self.left:
                self.left = TreeNode(num, None, None)
            else:
                self.left.insert(num)
        else:
            if not self.right:
                self.right = TreeNode(num, None, None)
            else:
                self.right.insert(num)

    def sort_tree(self, l: list[Optional[str]]) -> None:
        if self.left:
            self.left.sort_tree(l)
        l.append(self.data)
        if self.right:
            self.right.sort_tree(l)


class BinarySearchTree:
    def __init__(self, tree_data: list[str]) -> None:
        self.tree_data = tree_data

    def data(self):
        tree = TreeNode(None, None, None)
        for num in self.tree_data:
            tree.insert(num)
        return tree

    def sorted_data(self) -> list[Optional[str]]:
        tree = self.data()
        tree_data_sorted: list[Optional[str]] = []
        tree.sort_tree(tree_data_sorted)
        return tree_data_sorted
