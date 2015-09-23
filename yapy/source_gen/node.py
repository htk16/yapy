class Node:
    """Abstract Syntax Tree node"""
    MAGIC_PRIME_NUMBER = 9999991  # for calculating a hash value

    def __new__(cls, *args, **kwargs):
        node = object.__new__(cls)
        cls.__init__(node, *args, **kwargs)
        setattr(node, "_fields", None)
        setattr(node, "_hash_cache", None)
        return node

    def _equal_type(self, other) -> bool:
        return self.__class__ == other.__class__

    def _equal_properties(self, other) -> bool:
        return self._fields == other._fields

    def __eq__(self, other) -> bool:
        return self._equal_type(other) and self._equal_properties(other)

    def __hash__(self):
        def _reduce_hashes(lst, init):
            return functools.reduce(lambda acc, e: (acc * _calc_hash(e)) % self.MAGIC_PRIME_NUMBER,
                                    lst,
                                    init)

        def _calc_hash(v):
            if isinstance(v, list):
                return _reduce_hashes(v, 1)
            else:
                return hash(v)

        if self._hash_cache is None:
            children = (i[1] for i in self.fields())
            self._hash_cache = _reduce_hashes(children, hash(type(self)))

        return self._hash_cache

    def __str__(self) -> str:
        return "{0}({1})".format(
            self.__class__.__name__,
            ", ".join("{0}={1}".format(k, _node2str(v))
                      for (k, v) in self.fields()))

    def fields(self):
        return self._fields.items()

    def _init_fields(self, fields: dict):
        self._fields = fields

    def _get_field_value(self, key: str):
        return self._fields[key]

    def is_block(self) -> bool:
        """Return True if self is a block node"""
        return False


def _node2str(node) -> str:
    if isinstance(node, list):
        return "[{0}]".format(", ".join(str(e) for e in node))
    elif isinstance(node, str):
        return '"{0}"'.format(node)
    else:
        return str(node)


def walk(node: Node):
    """Return a node traverse iterator"""
    yield node

    for _, child in node.fields():
        if isinstance(child, list):
            for elem in child:
                yield from walk(elem)
        else:
            yield from walk(child)


def walk_in_scope(node: Node):
    """Return a node traverse iterator in current scope"""
    if not isinstance(node, Node):
        return

    yield node
    for _, child in node.fields():
        # Skip local block
        if isinstance(child, list):
            for elem in child:
                yield from walk_in_scope(elem)
        elif (not isinstance(child, Node)) or child.is_block():
            continue
        else:
            yield from walk_in_scope(child)
