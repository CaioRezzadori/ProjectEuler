from typing import Optional


class Zipper:

    def __init__(
        self,
        tree: "Optional[Zipper]",
        parent: "Optional[Zipper]",
    ):
        self.tree = tree
        self.parent = parent

    def __bool__(self):
        return self.tree is not None

    @staticmethod
    def from_tree(tree):
        return Zipper(tree, None)

    def value(self):
        return self.tree["value"]

    def set_value(self, value):
        self.tree["value"] = value
        return self

    def left(self):
        return Zipper(self.tree["left"], self)

    def set_left(self, treeLeft):
        self.tree["left"] = treeLeft
        return self

    def right(self):
        return Zipper(self.tree["right"], self)

    def set_right(self, treeRight):
        self.tree["right"] = treeRight
        return self

    def up(self):
        return self.parent

    def to_tree(self):
        if self.parent:
            return self.parent.to_tree()
        return self.tree
