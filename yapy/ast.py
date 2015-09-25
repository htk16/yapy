"""ast.py: yapy AST definitions

This source is generated by source_gen/gen_ast.py.
Don't modify this source directly.
"""
import functools


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


class Module(Node):
    """Module

    Module ::= StatementBlock
    """
    def __init__(self, block: "StatementBlock"):
        self._init_fields({"block": block})
    
    @property
    def block(self) -> "StatementBlock":
        return self._get_field_value("block")
    

class Interactive(Node):
    """Interactive

    Interactive ::= StatementBlock
    """
    def __init__(self, block: "StatementBlock"):
        self._init_fields({"block": block})
    
    @property
    def block(self) -> "StatementBlock":
        return self._get_field_value("block")
    

class StatementBlock(Node):
    """StatementBlock

    StatementBlock ::= '{' Statements '}' | Statement
    """
    def __init__(self, statements: list):
        self._init_fields({"statements": statements})
    
    @property
    def statements(self) -> list:
        return self._get_field_value("statements")
    
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
        self._init_fields({"names": names})
    
    @property
    def names(self) -> list:
        return self._get_field_value("names")
    
    @property
    def name(self) -> str:
        return ".".join(self.names)
    

class Import(Statement):
    """Module import

    Import ::= 'import' ModuleName
    """
    def __init__(self, module_name: ModuleName):
        self._init_fields({"module_name": module_name})
    
    @property
    def module_name(self) -> ModuleName:
        return self._get_field_value("module_name")
    

class FunctionDefinition(Statement):
    """Function definition

    FunctionDefinition ::= 'fun' Identifier '(' Parameters ')' -> Type ':' FunctionBody
    FunctionBody ::= '{' Statements '}' | Statement
    Parameters ::= [ TypedVariable { ',' TypedVariable } ]
    """
    def __init__(self, name: str, params: list, return_type: "Type", body: StatementBlock):
        self._init_fields({"name": name, "params": params, "return_type": return_type, "body": body})
    
    @property
    def name(self) -> str:
        return self._get_field_value("name")
    
    @property
    def params(self) -> list:
        return self._get_field_value("params")
    
    @property
    def return_type(self) -> "Type":
        return self._get_field_value("return_type")
    
    @property
    def body(self) -> StatementBlock:
        return self._get_field_value("body")
    

class Return(Statement):
    """Return statement"""
    def __init__(self, value: "Expression"):
        self._init_fields({"value": value})
    
    @property
    def value(self) -> "Expression":
        return self._get_field_value("value")
    

class IfStatement(Statement):
    """If statement"""
    def __init__(self, cond: "Expression", then_statement: list, else_statement: list):
        self._init_fields({"cond": cond, "then_statement": then_statement, "else_statement": else_statement})
    
    @property
    def cond(self) -> "Expression":
        return self._get_field_value("cond")
    
    @property
    def then_statement(self) -> list:
        return self._get_field_value("then_statement")
    
    @property
    def else_statement(self) -> list:
        return self._get_field_value("else_statement")
    

class Assert(Statement):
    """Assertion

    Assert ::= 'assert' '(' Expression [',' Expression] ')'
    """
    def __init__(self, expr: "Expression", msg: "Expression"):
        self._init_fields({"expr": expr, "msg": msg})
    
    @property
    def expr(self) -> "Expression":
        return self._get_field_value("expr")
    
    @property
    def msg(self) -> "Expression":
        return self._get_field_value("msg")
    

class VariableBinding(Statement):
    """Variable Binding

    VariableBinding ::= 'let' TypedVariable '=' Expression
    """
    def __init__(self, var: "TypedVariable", expr: "Expression"):
        self._init_fields({"var": var, "expr": expr})
    
    @property
    def var(self) -> "TypedVariable":
        return self._get_field_value("var")
    
    @property
    def expr(self) -> "Expression":
        return self._get_field_value("expr")
    

class Expression(Node):
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
    def __init__(self):
        self._init_fields({})
    

class Block(Expression):
    """Block

    Block ::= '{' Statements '}'
    """
    def __init__(self, statements: list):
        self._init_fields({"statements": statements})
    
    @property
    def statements(self) -> list:
        return self._get_field_value("statements")
    
    def is_block(self) -> bool:
        """Return True if self is a block node (Block or StatementBlock)"""
        return True
        

class If(Expression):
    """if expression

    If ::= 'if' Expression 'then' IfBlock [ 'else' IfBlock ]
    IfBlock ::= Expression | Block
    """
    def __init__(self, cond: Expression, then_expr: Expression, else_expr: Expression):
        self._init_fields({"cond": cond, "then_expr": then_expr, "else_expr": else_expr})
    
    @property
    def cond(self) -> Expression:
        return self._get_field_value("cond")
    
    @property
    def then_expr(self) -> Expression:
        return self._get_field_value("then_expr")
    
    @property
    def else_expr(self) -> Expression:
        return self._get_field_value("else_expr")
    

class For(Expression):
    """For expression

    For ::= 'for' '(' Variable 'in' Expression ')' Expression
    """
    def __init__(self, var: "Variable", expr: Expression, body: Expression):
        self._init_fields({"var": var, "expr": expr, "body": body})
    
    @property
    def var(self) -> "Variable":
        return self._get_field_value("var")
    
    @property
    def expr(self) -> Expression:
        return self._get_field_value("expr")
    
    @property
    def body(self) -> Expression:
        return self._get_field_value("body")
    

class FunctionCall(Expression):
    """Function call

    FunctionCall ::= Expression '(' [ Expression { ',' Expression } ] ')'
    """
    def __init__(self, func: Expression, params: list):
        self._init_fields({"func": func, "params": params})
    
    @property
    def func(self) -> Expression:
        return self._get_field_value("func")
    
    @property
    def params(self) -> list:
        return self._get_field_value("params")
    

class Subscript(Expression):
    """Subscript

    Subscript ::= Expression '[' (Index | Slice) ']'
    Index ::= Expression
    Slice ::= Expression ':' Expression
    """
    def __init__(self, value: Expression, range: Node):
        self._init_fields({"value": value, "range": range})
    
    @property
    def value(self) -> Expression:
        return self._get_field_value("value")
    
    @property
    def range(self) -> Node:
        return self._get_field_value("range")
    

class Index(Node):
    """Subscript index"""
    def __init__(self, value: Expression):
        self._init_fields({"value": value})
    
    @property
    def value(self) -> Expression:
        return self._get_field_value("value")
    

class Slice(Node):
    """Subscript slice"""
    def __init__(self, lower: Expression, upper: Expression, step: Expression):
        self._init_fields({"lower": lower, "upper": upper, "step": step})
    
    @property
    def lower(self) -> Expression:
        return self._get_field_value("lower")
    
    @property
    def upper(self) -> Expression:
        return self._get_field_value("upper")
    
    @property
    def step(self) -> Expression:
        return self._get_field_value("step")
    

class UnaryOperator(Node):
    """Unary operator"""
    def __init__(self, op: str):
        self._init_fields({"op": op})
    
    @property
    def op(self) -> str:
        return self._get_field_value("op")
    

class UnaryOperation(Expression):
    """Unary operation

    UnaryOperation ::= UnaryOperator Expression
    """
    def __init__(self, expr: Expression, op: UnaryOperator):
        self._init_fields({"expr": expr, "op": op})
    
    @property
    def expr(self) -> Expression:
        return self._get_field_value("expr")
    
    @property
    def op(self) -> UnaryOperator:
        return self._get_field_value("op")
    

class BinaryOperations(Expression):
    """Binary operations

    BinaryOperations ::= Term1 { BinaryOperator Term1 }
    """
    def __init__(self, terms_and_ops: list):
        self._init_fields({"terms_and_ops": terms_and_ops})
    
    @property
    def terms_and_ops(self) -> list:
        return self._get_field_value("terms_and_ops")
    

class BinaryOperator(Node):
    """Binary Operator"""
    def __init__(self, op: str):
        self._init_fields({"op": op})
    
    @property
    def op(self) -> str:
        return self._get_field_value("op")
    

class BinaryOperation(Expression):
    """Binary Operation"""
    def __init__(self, lhs: Expression, op: BinaryOperator, rhs: Expression):
        self._init_fields({"lhs": lhs, "op": op, "rhs": rhs})
    
    @property
    def lhs(self) -> Expression:
        return self._get_field_value("lhs")
    
    @property
    def op(self) -> BinaryOperator:
        return self._get_field_value("op")
    
    @property
    def rhs(self) -> Expression:
        return self._get_field_value("rhs")
    

class Attribute(Expression):
    """Attribute

    Attribute ::= Expression '.' Identifier
    """
    def __init__(self, expr: Expression, attr: str):
        self._init_fields({"expr": expr, "attr": attr})
    
    @property
    def expr(self) -> Expression:
        return self._get_field_value("expr")
    
    @property
    def attr(self) -> str:
        return self._get_field_value("attr")
    

class List(Expression):
    """List

    List ::= '[' [ Expression { ',' Expression } ] ']'
    """
    def __init__(self, exprs: list):
        self._init_fields({"exprs": exprs})
    
    @property
    def exprs(self) -> list:
        return self._get_field_value("exprs")
    

class Set(Expression):
    """Set

    Set ::= '{' [ Expression { ',' Expression } ] '}'
    """
    def __init__(self, exprs: list):
        self._init_fields({"exprs": exprs})
    
    @property
    def exprs(self) -> list:
        return self._get_field_value("exprs")
    

class Dict(Expression):
    """Dictionary

    Dict ::= '{' [ KeyValue { ',' KeyValue } ] '}'
    """
    def __init__(self, kvs: list):
        self._init_fields({"kvs": kvs})
    
    @property
    def kvs(self) -> list:
        return self._get_field_value("kvs")
    

class KeyValue(Node):
    """Key Value pair

    KeyValue ::= Expression ':' Expression
    """
    def __init__(self, key: Expression, value: Expression):
        self._init_fields({"key": key, "value": value})
    
    @property
    def key(self) -> Expression:
        return self._get_field_value("key")
    
    @property
    def value(self) -> Expression:
        return self._get_field_value("value")
    

class Function(Expression):
    """Anonymous function

    Function ::= 'fn' '(' Parameters ')' -> Type ':' Block
    """
    def __init__(self, params: list, return_type: "Type", body: Expression):
        self._init_fields({"params": params, "return_type": return_type, "body": body})
    
    @property
    def params(self) -> list:
        return self._get_field_value("params")
    
    @property
    def return_type(self) -> "Type":
        return self._get_field_value("return_type")
    
    @property
    def body(self) -> Expression:
        return self._get_field_value("body")
    

class Variable(Expression):
    """Variable

    Variable ::= Identifier
    """
    def __init__(self, name: str):
        self._init_fields({"name": name})
    
    @property
    def name(self) -> str:
        return self._get_field_value("name")
    

class TypedVariable(Expression):
    """Typed Variable

    TypedVariable ::= Variable ':' Type
    """
    def __init__(self, var: Variable, type_name: "Type"):
        self._init_fields({"var": var, "type_name": type_name})
    
    @property
    def var(self) -> Variable:
        return self._get_field_value("var")
    
    @property
    def type_name(self) -> "Type":
        return self._get_field_value("type_name")
    

class String(Expression):
    """String

    String ::= r"".*""
    """
    def __init__(self, contents: str):
        self._init_fields({"contents": contents})
    
    @property
    def contents(self) -> str:
        return self._get_field_value("contents")
    

class Integer(Expression):
    """Integer

    Integer ::= r"-?[1-9]*[0-9]"
    """
    def __init__(self, value: int):
        self._init_fields({"value": value})
    
    @property
    def value(self) -> int:
        return self._get_field_value("value")
    

class Float(Expression):
    """Float number

    Float ::= r"-?[1-9]*[0-9](\.[0-9]+)"
    """
    def __init__(self, value: float):
        self._init_fields({"value": value})
    
    @property
    def value(self) -> float:
        return self._get_field_value("value")
    

class Boolean(Expression):
    """Boolean

    Boolean ::= 'True' | 'False'
    """
    def __init__(self, value: bool):
        self._init_fields({"value": value})
    
    @property
    def value(self) -> bool:
        return self._get_field_value("value")
    

class NoneValue(Expression):
    """None value

    None ::= 'None'
    """
    def __init__(self):
        self._init_fields({})
    

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
        self._init_fields({"module_name": module_name, "name": name})
    
    @property
    def module_name(self) -> ModuleName:
        return self._get_field_value("module_name")
    
    @property
    def name(self) -> str:
        return self._get_field_value("name")
    

class GenericType(Type):
    """Generic type

    GenericType ::= ModuleName TypeName '[' TypeParameters ']'
    TypeParameters ::= Type { ',' Type }
    """
    def __init__(self, module_name: ModuleName, name: str, params: list):
        self._init_fields({"module_name": module_name, "name": name, "params": params})
    
    @property
    def module_name(self) -> ModuleName:
        return self._get_field_value("module_name")
    
    @property
    def name(self) -> str:
        return self._get_field_value("name")
    
    @property
    def params(self) -> list:
        return self._get_field_value("params")
    

class TypeTuple(Type):
    """Type Tuple

    TypeTuple ::= '(' Type { ',' Type } ')'
    """
    def __init__(self, types: list):
        self._init_fields({"types": types})
    
    @property
    def types(self) -> list:
        return self._get_field_value("types")
    

class Any(Type):
    """Any type"""
    def __init__(self):
        self._init_fields({})
    

class Unsolved(Type):
    """Unsolved type"""
    def __init__(self):
        self._init_fields({})
    

