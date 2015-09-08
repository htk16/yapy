import yapy.parser as parser
from yapy.ast import *


def parse(p, cnt) -> Node:
    return p.parseString(cnt, parseAll=True)[0]

def module(*names) -> ModuleName:
    return ModuleName(list(names))

def ptype(*names) -> PrimitiveType:
    return PrimitiveType(module(*names[:-1]), names[-1])

def gtype(names: list, params: list) -> GenericType:
    return GenericType(module(*names[:-1]), names[-1], params)

def var(name: str) -> Variable:
    return Variable(name)

def tvar(name: str, type_name: Type) -> TypedVariable:
    return TypedVariable(var(name), type_name)

def bop(name: str) -> BinaryOperator:
    return BinaryOperator(name)

def bops(*elems: list) -> BinaryOperations:
    return BinaryOperations(list(elems))


def test_type_parsing():
    """Tests for type notations parsing"""
    assert parse(parser.TypeName, "Int") == "Int"
    assert parse(parser.TypeName, "Foo_Bar_2000") == "Foo_Bar_2000"
    assert parse(parser.ModuleName, "funpy") == module("funpy")
    assert parse(parser.ModuleName, "funpy.ast") == module("funpy", "ast")

    assert parse(parser.Type, "Float") == ptype("Float")
    assert parse(parser.Type, "funpy.ast.Float") == ptype("funpy", "ast", "Float")
    assert parse(parser.Type, "List[Int]") == gtype(["List"], [ptype("Int")])
    assert parse(parser.Type, "funpy.List[a.b.Int, Float]") == \
        gtype(["funpy", "List"], [ptype("a", "b", "Int"), ptype("Float")])
    assert parse(parser.Type, "(Int, Float, funpy.List[Boolean])") == \
        TypeTuple([ptype("Int"), ptype("Float"), gtype(["funpy", "List"], [ptype("Boolean")])])


def test_primitive_parsing():
    """Tests for primitive types parsing"""
    assert parse(parser.Primitive, "11") == Integer(11)
    assert parse(parser.Primitive, "3.14") == Float(3.14)
    assert parse(parser.Primitive, "True") == Boolean(True)
    assert parse(parser.Primitive, "False") == Boolean(False)
    assert parse(parser.Primitive, '""') == String("")
    assert parse(parser.Primitive, '"hoge"') == String("hoge")


def test_expression_parsing():
    """Tests for expression parsing"""
    parse_expr = lambda source: parse(parser.Expression, source)

    # Term0: Primitive data structures
    assert parse_expr("hoge") == Variable("hoge")
    assert parse_expr("_ff3") == Variable("_ff3")
    assert parse_expr("[1, 2, 4]") == List([Integer(i) for i in [1, 2, 4]])
    assert parse_expr('{"hoge", "__init__"}') == Set([String(c) for c in ["hoge", "__init__"]])
    assert parse_expr('{"Python": 3, "C++": 17}') == \
           Dict([KeyValue(String(k), Integer(v))
                 for (k, v) in [("Python", 3), ("C++", 17)]])
    assert parse(parser.TypedVariable, "hoge: Int") == \
           TypedVariable(Variable("hoge"), PrimitiveType(ModuleName([]), "Int"))
    assert parse_expr('("hoge")') == String("hoge")
    assert parse_expr('(1 + 2)') == bops(Integer(1), bop("+"), Integer(2))

    # Term1: Attribute reference
    assert parse_expr("x.y") == Attribute(var("x"), "y")

    # Term2: Function call
    assert parse_expr("abort()") == FunctionCall(var("abort"), [])
    assert parse_expr("sum(1, x)") == \
           FunctionCall(var("sum"), [Integer(1), var("x")])

    # Term3: Basic expressions
    assert parse_expr('if x = 1 then "a" else "b"') == \
           If(bops(var("x"), bop("="), Integer(1)),
              Block([String("a")]),
              Block([String("b")]))
    assert parse_expr('fn() = print("Hello, world!")') == \
           Function([],
                    Unsolved(),
                    Block([FunctionCall(var("print"), [String("Hello, world!")])]))
    assert parse_expr('fn(): Unit = print("Hello, world!")') == \
           Function([],
                    ptype("Unit"),
                    Block([FunctionCall(var("print"), [String("Hello, world!")])]))
    assert parse_expr("fn(d1, d2) = 16") == \
           Function([tvar("d1", Unsolved()), tvar("d2", Unsolved())],
                    Unsolved(),
                    Block([Integer(16)]))
    assert parse_expr("fn(d1: Int, d2: Int): Int = 16") == \
           Function([tvar("d1", ptype("Int")), tvar("d2", ptype("Int"))],
                    ptype("Int"),
                    Block([Integer(16)]))
    assert parse_expr("fn(lhs: str, rhs: str): str = { let s: str = lhs + rhs; s }") == \
           Function([tvar("lhs", ptype("str")), tvar("rhs", ptype("str"))],
                    ptype("str"),
                    Block([
                        VariableBinding(
                            tvar("s", ptype("str")),
                            bops(var("lhs"), bop("+"), var("rhs"))),
                        var("s")]))
    assert parse_expr("fn(lhs: str, rhs: str): str = { let s: str = lhs + rhs\n s }") == \
           Function([tvar("lhs", ptype("str")), tvar("rhs", ptype("str"))],
                    ptype("str"),
                    Block([
                        VariableBinding(
                            tvar("s", ptype("str")),
                            bops(var("lhs"), bop("+"), var("rhs"))),
                        var("s")]))

    # Term4: Unary Operation
    assert parse_expr("!e") == UnaryOperation(var("e"), UnaryOperator("!"))

    # Term5: Binary Operation
    assert parse(parser.BinaryOperator, "+") == bop("+")
    assert parse(parser.BinaryOperator, ">>") == bop(">>")
    assert parse_expr("2.56") == Float(2.56)
    assert parse_expr("2.56 + 3.14") == bops(Float(2.56), bop("+"), Float(3.14))
    assert parse_expr("10 + 2 * 3") == \
           bops(Integer(10), bop("+"), Integer(2), bop("*"), Integer(3))


def test_statement_parsing():
    """Test for statement parsing"""
    parse_stmt = lambda source: parse(parser.Statement, source)

    assert parse(parser.Statement, "for (e in [1, 2, 4]) (e + 1)") == \
           For(var("e"),
               List([Integer(1), Integer(2), Integer(4)]),
               Block([bops(var("e"), bop("+"), Integer(1))]))
    assert parse(parser.Statement, "let foo: Float = 3.14") == \
           VariableBinding(tvar("foo", ptype("Float")), Float(3.14))
    assert parse(parser.Statement, "import urllib.request") == Import(module("urllib", "request"))
    assert parse(parser.Statement, "def mul(x: Int, y: Int): Int = (x * y)") == \
           FunctionDefinition("mul",
                              [tvar("x", ptype("Int")), tvar("y", ptype("Int"))],
                              ptype("Int"),
                              Block([bops(var("x"), bop("*"), var("y"))]))
    assert parse(parser.Statement, "assert(True)") == Assert(Boolean(True), NoneValue())
    assert parse(parser.Statement, 'assert(False, "fail")') == Assert(Boolean(False), String("fail"))


def test_module_parsing():
    """Test for module parsing"""
    assert parse(parser.Module, """
    def sum(x: Int, y: Int): Int = (x + y)
    let result: Int = sum(1, 2)""") == \
           Module([
               FunctionDefinition("sum",
                                  [tvar("x", ptype("Int")), tvar("y", ptype("Int"))],
                                  ptype("Int"),
                                  Block([bops(var("x"), bop("+"), var("y"))])),
               VariableBinding(tvar("result", ptype("Int")),
                               FunctionCall(var("sum"),
                                            [Integer(1), Integer(2)]))])

