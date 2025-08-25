class TreeNode:
    def __init__(
        self,
        data: str | None,
        left: None = None,  # Duvidas de tipagem
        right: None = None,
    ):
        self.data = data
        self.left = left
        self.right = right

    def __str__(self):
        return f"TreeNode(data={self.data}, left={self.left}, right={self.right})"

    def insert(self, num: str) -> None:
        if self.data is None:
            self.data = num
        elif float(num) <= float(self.data):
            if self.left is None:
                self.left = TreeNode(num, None, None)
            else:
                self.left.insert(num)
        else:
            if self.right is None:
                self.right = TreeNode(num, None, None)
            else:
                self.right.insert(num)

    def sort_tree(self, l: list[str]):
        if self.left is not None:
            self.left.sort_tree(l)
        l.append(self.data)
        if self.right is not None:
            self.right.sort_tree(l)


class BinarySearchTree:
    def __init__(self, tree_data: list[str]):
        self.tree_data = tree_data

    def data(self):
        tree = TreeNode(None, None, None)
        for num in self.tree_data:
            tree.insert(num)
        return tree

    def sorted_data(self):
        tree = self.data()
        tree_data_sorted: list[str] = []
        tree.sort_tree(tree_data_sorted)
        return tree_data_sorted
