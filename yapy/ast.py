import functools


class Node:
    """Abstract Syntax Tree node"""
    MAGIC_PRIME_NUMBER = 9999991  # for calculating a hash value

    def _equal_type(self, other) -> bool:
        return self.__class__ == other.__class__

    def _equal_properties(self, other) -> bool:
        return self.__dict__ == other.__dict__

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

        # TODO cache hash value
        children = (i[1] for i in self.fields())
        return _reduce_hashes(children, hash(type(self)))

    def __str__(self) -> str:
        return "{0}({1})".format(
            self.__class__.__name__,
            ", ".join("{0}={1}".format(k, _node2str(v))
                      for (k, v) in self.fields()))

    def fields(self):
        for (k, v) in self.__dict__.items():
            if len(k) > 0 and k[0] == "_":
                yield (k[1:], v)

    def is_block(self) -> bool:
        """Return True if self is a block node (Block or StatementBlock)"""
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


class NodeVisitor:
    def visit(self, node):
        """Visit a node."""
        method = 'visit_' + node.__class__.__name__
        visitor = getattr(self, method, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        """Called if no explicit visitor function exists for a node."""
        for field, value in node.fields():
            if isinstance(value, list):
                for item in value:
                    if isinstance(item, Node):
                        self.visit(item)
            elif isinstance(value, Node):
                self.visit(value)


class NodeTransducer:
    def __new__(cls, *args, **kwargs):
        node = object.__new__(cls)
        cls.__init__(node, *args, **kwargs)
        setattr(node, "_cache", {})
        return node


    def transduce(self, node) -> Node:
        """transduce an AST."""
        if not self.need_transduce(node):
            return node

        if isinstance(node, Node):
            transducer_name = 'transduce_' + node.__class__.__name__
            transducer = getattr(self, transducer_name, self.transduce_generic_node)
            return transducer(node)
        elif isinstance(node, list):
            return list(self.transduce(child) for child in node)
        else:
            return node

    def transduce_generic_node(self, node: Node) -> Node:
        return self.transduce(
                node.__class__(**dict((k, self.transduce(n)) for (k, n) in node.fields())))

    def need_transduce(self, node) -> bool:
        def _check():
            if isinstance(node, Node):
                predicate_name = 'need_transduce_' + node.__class__.__name__
                predicate = getattr(self, predicate_name, self.generic_predicate)
                return predicate(node)
            elif isinstance(node, list):
                return any(self.need_transduce(n) for n in node)
            else:
                return False

        if isinstance(node, list):
            # list is unhashable type
            return _check()

        if node not in self._cache:
            # memoize hash value
            self._cache[node] = _check()
        return self._cache[node]

    def generic_predicate(self, node: Node) -> bool:
        return any(self.need_transduce(child) for (name, child) in node.fields())


class Module(Node):
    """Module

    Module ::= StatementBlock
    """
    def __init__(self, block: "StatementBlock"):
        self._block = block

    @property
    def block(self):
        return self._block


class Interactive(Node):
    """Interactive

    Interactive ::= StatementBlock
    """
    def __init__(self, block: "StatementBlock"):
        self._block = block

    @property
    def block(self):
        return self._block


class StatementBlock(Node):
    """StatementBlock

    StatementBlock ::= '{' Statements '}' | Statement
    """
    def __init__(self, statements: list):
        self._statements = statements

    @property
    def statements(self) -> list:
        return self._statements

    def is_block(self) -> bool:
        """Return True if self is a block node (Block or StatementBlock)"""
        return True


class Statement(Node):
    """Abstract class for Statement

    Statement ::=  VariableBinding
                | Import
                | FunctionDefinition
                | Return
                | IfStatement
                | Assert
                | Expression
    """


class ModuleName(Node):
    """Module name

    ModuleName::= Identifier { '.' Identifier }
    """
    def __init__(self, names: list):
        self._names = names

    @property
    def names(self) -> list:
        return self._names

    @property
    def name(self) -> str:
        return ".".join(self.names)


class Import(Statement):
    """Module import

    Import ::= 'import' ModuleName
    """
    def __init__(self, module_name: ModuleName):
        self._module_name = module_name

    @property
    def module_name(self):
        return self._module_name


class FunctionDefinition(Statement):
    """Function definition

    FunctionDefinition ::= 'fun' Identifier '(' Parameters ')' -> Type ':' FunctionBody
    FunctionBody ::= '{' Statements '}' | Statement
    Parameters ::= [ TypedVariable { ',' TypedVariable } ]
    """
    def __init__(self, name: str, params: list, return_type: "Type", body: StatementBlock):
        self._name = name
        self._params = params
        self._return_type = return_type
        self._body = body

    @property
    def name(self) -> str:
        return self._name

    @property
    def params(self) -> list:
        return self._params

    @property
    def return_type(self) -> "Type":
        return self._return_type

    @property
    def body(self) -> StatementBlock:
        return self._body


class Return(Statement):
    """Return statement"""
    def __init__(self, value: "Expression"):
        self._value = value

    @property
    def value(self) -> "Expression":
        return self._value


class IfStatement(Statement):
    """If statement"""
    def __init__(self, cond: "Expression", then_statements: list, else_statements: list):
        self._cond = cond
        self._then_statements = then_statements
        self._else_statements = else_statements

    @property
    def cond(self) -> "Expression":
        return self._cond

    @property
    def then_statements(self) -> list:
        return self._then_statements

    @property
    def else_statements(self) -> list:
        return self._else_statements


class Assert(Statement):
    """Assertion

    Assert ::= 'assert' '(' Expression [',' Expression] ')'
    """
    def __init__(self, expr: "Expression", msg: "Expression"):
        self._expr = expr
        self._msg = msg

    @property
    def expr(self) -> "Expression":
        return self._expr

    @property
    def msg(self) -> "Expression":
        return self._msg


class VariableBinding(Statement):
    """Variable Binding

    VariableBinding ::= 'let' TypedVariable '=' Expression
    """
    def __init__(self, var: "TypedVariable", expr: "Expression"):
        self._var = var
        self._expr = expr

    @property
    def var(self) -> "TypedVariable":
        return self._var

    @property
    def expr(self) -> "Expression":
        return self._expr


class Expression(Statement):
    """Abstract class for Expression

     Expression ::= Term2
     Term2 ::= BinaryOperations
             | Term1
     Term1 ::= FunctionCall
             | Subscript
             | Term0
     Term0 ::= If
              | For
              | UnaryOperation
              | List
              | Dict
              | Function
              | Primitive
              | Variable
              | '(' Expression ')'
    """


class Block(Node):
    """Block

    Block ::= '{' Statements '}'
    """
    def __init__(self, statements: list):
        self._statements = statements

    @property
    def statements(self) -> list:
        return self._statements

    def is_block(self) -> bool:
        """Return True if self is a block node (Block or StatementBlock)"""
        return True


class If(Expression):
    """if expression

    If ::= 'if' Expression 'then' IfBlock [ 'else' IfBlock ]
    IfBlock ::= Expression | Block
    """
    def __init__(self, cond: Expression, then_expr: Block, else_expr: Block):
        self._cond = cond
        self._then_expr = then_expr
        self._else_expr = else_expr

    @property
    def cond(self) -> Expression:
        return self._cond

    @property
    def then_expr(self) -> Node:
        return self._then_expr

    @property
    def else_expr(self) -> Node:
        return self._else_expr


class For(Expression):
    """For expression

    For ::= 'for' '(' Variable 'in' Expression ')' Expression
    """
    def __init__(self, var: "Variable", expr: Expression, body: Expression):
        self._var = var
        self._expr = expr
        self._body = body

    @property
    def var(self) -> "Variable":
        return self._var

    @property
    def expr(self) -> Expression:
        return self._expr

    @property
    def body(self) -> Expression:
        return self._body


class FunctionCall(Expression):
    """Function call

    FunctionCall ::= Expression '(' [ Expression { ',' Expression } ] ')'
    """
    def __init__(self, func: Expression, params: list):
        self._func = func
        self._params = params

    @property
    def func(self) -> Expression:
        return self._func

    @property
    def params(self) -> Expression:
        return self._params


class Subscript(Expression):
    """Subscript

    Subscript ::= Expression '[' (Index | Slice) ']'
    Index ::= Expression
    Slice ::= Expression ':' Expression
    """
    def __init__(self, value: Expression, range: Expression):
        self._value = value
        self._range = range

    @property
    def value(self):
        return self._value

    @property
    def range(self):
        return self._range


class Index(Node):
    """Subscript index"""
    def __init__(self, value: Expression):
        self._value = value

    @property
    def value(self):
        return self._value


class Slice(Node):
    """Subscript slice"""
    def __init__(self, lower: Expression, upper: Expression, step: Expression):
        self._lower = lower
        self._upper = upper
        self._step = step

    @property
    def lower(self):
        return self._lower

    @property
    def upper(self):
        return self._upper

    @property
    def step(self):
        return self._step


class UnaryOperator(Node):
    """Unary operator"""
    def __init__(self, op: str):
        self._op = op

    @property
    def op(self) -> str:
        return self._op


class UnaryOperation(Expression):
    """Unary operation

    UnaryOperation ::= UnaryOperator Expression
    """
    def __init__(self, expr: Expression, op: UnaryOperator):
        self._expr = expr
        self._op = op

    @property
    def expr(self) -> Expression:
        return self._expr

    @property
    def op(self) -> UnaryOperator:
        return self._op


class BinaryOperations(Expression):
    """Binary operations

    BinaryOperations ::= Term1 { BinaryOperator Term1 }
    """
    def __init__(self, terms_and_ops: list):
        assert len(terms_and_ops) % 2 == 1
        self._terms_and_ops = terms_and_ops

    @property
    def terms_and_ops(self) -> list:
        return self._terms_and_ops


class BinaryOperator(Node):
    """Binary Operator"""
    def __init__(self, op: str):
        self._op = op

    @property
    def op(self) -> str:
        return self._op


class BinaryOperation(Expression):
    """Binary Operation"""
    def __init__(self, lhs: Expression, op: BinaryOperator, rhs: Expression):
        self._lhs = lhs
        self._op = op
        self._rhs = rhs

    @property
    def lhs(self) -> Expression:
        return self._lhs

    @property
    def op(self) -> BinaryOperator:
        return self._op

    @property
    def rhs(self) -> Expression:
        return self._rhs


class Attribute(Expression):
    """Attribute

    Attribute ::= Expression '.' Identifier"""
    def __init__(self, expr: Expression, attr: str):
        self._expr = expr
        self._attr = attr

    @property
    def expr(self) -> Expression:
        return self._expr

    @property
    def attr(self) -> str:
        return self._attr


class List(Expression):
    """List

    List ::= '[' [ Expression { ',' Expression } ] ']'
    """
    def __init__(self, exprs: list):
        self._exprs = exprs

    @property
    def exprs(self) -> list:
        return self._exprs


class Set(Expression):
    """Set

    Set ::= '{' [ Expression { ',' Expression } ] '}'
    """
    def __init__(self, exprs: list):
        self._exprs = exprs

    @property
    def exprs(self):
        return self._exprs


class Dict(Expression):
    """Dictionary

    Dict ::= '{' [ KeyValue { ',' KeyValue } ] '}'
    """
    def __init__(self, kvs: list):
        self._kvs = kvs

    @property
    def kvs(self) -> list:
        return self._kvs


class KeyValue(Node):
    """Key Value pair

    KeyValue ::= Expression ':' Expression
    """
    def __init__(self, key: Expression, value: Expression):
        self._key = key
        self._value = value

    @property
    def key(self):
        return self._key

    @property
    def value(self):
        return self._value


class Function(Expression):
    """Anonymous function

    Function ::= 'fn' '(' Parameters ')' -> Type ':' Block
    """
    def __init__(self, params: list, return_type: "Type", body: Expression):
        self._params = params
        self._return_type = return_type
        self._body = body

    @property
    def params(self) -> list:
        return self._params

    @property
    def return_type(self) -> "Type":
        return self._return_type

    @property
    def body(self) -> Expression:
        return self._body


class Variable(Expression):
    """Variable

    Variable ::= Identifier
    """
    def __init__(self, name: str):
        self._name = name

    @property
    def name(self) -> str:
        return self._name


class TypedVariable(Expression):
    """Typed Variable

    TypedVariable ::= Variable ':' Type
    """
    def __init__(self, var: Variable, type_name: "Type"):
        self._var = var
        self._type_name = type_name

    @property
    def var(self) -> Variable:
        return self._var

    @property
    def type_name(self) -> "Type":
        return self._type_name


class String(Expression):
    """String

    String ::= r"\".*\""
    """
    def __init__(self, contents: str):
        self._contents = contents

    @property
    def contents(self):
        return self._contents


class Integer(Expression):
    """Integer

    Integer ::= r"-?[1-9]*[0-9]"
    """
    def __init__(self, value: int):
        self._value = value

    @property
    def value(self):
        return self._value


class Float(Expression):
    """Float number

    Float ::= r"-?[1-9]*[0-9](\.[0-9]+)"
    """
    def __init__(self, value: float):
        self._value = value

    @property
    def value(self) -> float:
        return self._value


class Boolean(Expression):
    """Boolean

    Boolean ::= 'True' | 'False'
    """
    def __init__(self, value: bool):
        self._value = value

    @property
    def value(self) -> bool:
        return self._value


class NoneValue(Expression):
    """None value

    None ::= 'None'
    """
    def __init__(self):
        pass


class Type(Node):
    """Abstract class for type notations

    Type ::= PrimitiveType
           | GenericType
           | TypeTuple
           | Any
           | Unsolved
    """


class PrimitiveType(Type):
    """Primitive type

    PrimitiveType ::= ModuleName TypeName
    TypeName ::= r"[A-Z][a-zA-Z_1-9]*"
    """
    def __init__(self, module_name: ModuleName, name: str):
        self._module_name = module_name
        self._name = name

    @property
    def module_name(self) -> ModuleName:
        return self._module_name

    @property
    def name(self) -> str:
        return self._name


class GenericType(Type):
    """Generic type

    GenericType ::= ModuleName TypeName '[' TypeParameters ']'
    TypeParameters ::= Type { ',' Type }
    """
    def __init__(self, module_name: ModuleName, name: str, params: list):
        self._module_name = module_name
        self._name = name
        self._params = params

    @property
    def module_name(self) -> ModuleName:
        return self._module_name

    @property
    def name(self) -> str:
        return self._name

    @property
    def params(self) -> list:
        return self._params


class TypeTuple(Type):
    """Type Tuple

    TypeTuple ::= '(' Type { ',' Type } ')'
    """
    def __init__(self, types: list):
        self._types = types

    @property
    def types(self):
        return self._types


class Any(Type):
    """Any type"""
    pass


class Unsolved(Type):
    """Unsolved type"""
    pass
