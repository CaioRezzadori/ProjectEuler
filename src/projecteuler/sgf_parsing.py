class SgfTree:
    def __init__(self, properties=None, children=None):
        self.properties = properties or {}
        self.children = children or []

    def __eq__(self, other):
        if not isinstance(other, SgfTree):
            return False
        for key, value in self.properties.items():
            if key not in other.properties:
                return False
            if other.properties[key] != value:
                return False
        for key in other.properties.keys():
            if key not in self.properties:
                return False
        if len(self.children) != len(other.children):
            return False
        for child, other_child in zip(self.children, other.children):
            if child != other_child:
                return False
        return True

    def __ne__(self, other):
        return not self == other


def parse(input_string):
    if not input_string:
        raise ValueError("tree missing")

    if len(input_string) < 2 or input_string[0] != "(" or input_string[-1] != ")":
        raise ValueError("tree missing")

    content = input_string[1:-1]
    if not content:
        raise ValueError("tree with no nodes")

    pos = 0
    nodes = []
    children = []

    # Parse nodes
    while pos < len(content):
        if content[pos] == ";":
            node, pos = parse_node(content, pos)
            nodes.append(node)
        elif content[pos] == "(":
            break
        else:
            pos += 1

    if not nodes:
        raise ValueError("tree with no nodes")

    # Parse children
    while pos < len(content):
        if content[pos] == "(":
            child_content, pos = parse_parentheses(content, pos)
            child = parse("(" + child_content + ")")
            children.append(child)
        else:
            pos += 1

    # Handle sequential nodes (first becomes child of root)
    if len(nodes) > 1:
        first_child = SgfTree(properties=nodes[1])
        if len(nodes) > 2:
            first_child.children = [SgfTree(properties=p) for p in nodes[2:]]
        children.insert(0, first_child)

    return SgfTree(properties=nodes[0], children=children)


def parse_node(s, pos):
    if s[pos] != ";":
        raise ValueError("node should start with ';'")

    pos += 1
    properties = {}

    while pos < len(s) and s[pos] not in (";", "("):
        # Parse property identifier
        prop_start = pos
        while pos < len(s) and s[pos].isupper():
            pos += 1

        if pos < len(s) and s[pos].islower():
            raise ValueError("property must be in uppercase")

        prop_id = s[prop_start:pos]

        # Check for property values
        if pos >= len(s) or s[pos] != "[":
            raise ValueError("properties without delimiter")

        values = []
        while pos < len(s) and s[pos] == "[":
            pos += 1
            value = []
            escape = False

            while pos < len(s):
                char = s[pos]
                if escape:
                    if char == "\n":
                        pass  # remove escaped newline
                    elif char == "\t":
                        value.append(" ")  # escaped tab becomes space
                    elif char in ["\\", "]"]:
                        value.append(char)  # keep escaped chars
                    else:
                        value.append(char)  # other escaped characters
                    escape = False
                elif char == "\\":
                    escape = True
                elif char == "]":
                    break
                elif char == "\t":
                    value.append(" ")  # convert tab to space
                else:
                    value.append(char)
                pos += 1

            if pos >= len(s) or s[pos] != "]":
                raise ValueError("unterminated property value")
            pos += 1
            values.append("".join(value))

        properties[prop_id] = values

    return properties, pos


def parse_parentheses(s, pos):
    if s[pos] != "(":
        raise ValueError("expected '('")

    pos += 1
    start = pos
    balance = 1

    while pos < len(s) and balance > 0:
        if s[pos] == "(":
            balance += 1
        elif s[pos] == ")":
            balance -= 1
        pos += 1

    if balance != 0:
        raise ValueError("unbalanced parentheses")

    return s[start : pos - 1], pos
