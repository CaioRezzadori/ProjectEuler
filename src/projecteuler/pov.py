from json import dumps
from multiprocessing import Value


class Tree:
    def __init__(self, label: str, children: "list[Tree] | None" = None):
        self.label = label
        self.children = children if children is not None else []

    def __dict__(self):
        return {self.label: [c.__dict__() for c in sorted(self.children)]}

    def __str__(self, indent=None):
        return dumps(self.__dict__(), indent=indent)

    def __lt__(self, other: "Tree"):
        return self.label < other.label

    def __eq__(self, other: "Tree") -> bool:
        return self.__dict__() == other.__dict__()

    def __find_node__(self, node: str) -> list[str]:
        def find_node_rec(tree: "Tree", node: str, path: list[str]) -> list[str]:
            path += [tree.label]
            if tree.label == node:
                return path
            for subtree in tree.children:
                new_path = find_node_rec(subtree, node, [])
                if node in new_path:
                    return path + new_path
            path.pop()
            return path

        return find_node_rec(self, node, [])

    def parent_to_child(self, child: str) -> "Tree | None":
        for id, children in enumerate(self.children):
            if children.label == child:
                child_tree = self.children.pop(id)
                child_tree.children.append(self)
                return child_tree

    def from_pov(self, from_node: str):
        path_to_node = self.__find_node__(from_node)
        if not path_to_node:
            raise ValueError("Tree could not be reoriented")
        tree_pov = self
        for child in path_to_node[1:]:
            tree_pov = tree_pov.parent_to_child(child)
        return tree_pov

    def path_to(self, from_node: str, to_node: str):
        tree_pov = self.from_pov(from_node)
        path = tree_pov.__find_node__(to_node)
        if not path:
            raise ValueError("No path found")
        return path
