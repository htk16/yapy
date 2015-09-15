from yapy.ast import *
import yapy.parser as parser
import yapy.macro as macro


def parse_stmt(cnt):
    return parser.Statement.parseString(cnt, parseAll=True)[0]


bop = BinaryOperation
op = BinaryOperator


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


def test_binary_operations():
    expander = macro.MacroExpander()
    transduce = expander.transduce
    assert transduce(Integer(1)) == Integer(1)
    assert transduce(parse_stmt("1 + 2")) == bop(Integer(1), op("+"), Integer(2))
    assert transduce(parse_stmt("1 + 2 * 3")) == \
        bop(Integer(1), op("+"), bop(Integer(2), op("*"), Integer(3)))
    assert transduce(parse_stmt("10 * 200 + 3000")) == \
        bop(bop(Integer(10), op("*"), Integer(200)), op("+"), Integer(3000))
    assert transduce(parse_stmt("let x: bool = (1 + 2 = 3 * 4 + 5)")) == \
        VariableBinding(
            var=tvar("x", ptype("bool")),
            expr=bop(bop(Integer(1), op("+"), Integer(2)),
                     op("="),
                     bop(bop(Integer(3), op("*"), Integer(4)), op("+"), Integer(5))))


def test_hoisting_anonymous_functions():
    hoister = macro.BlockHoister.AnonymousFunctionHoister
    parse_module = parser.parse_module
    assert not hoister().need_transduce(parse_module("1 + 2 * 3; [1, 2, 3]"))
    assert hoister().need_transduce(parse_module("fn(): Int = 10"))
    assert hoister().need_transduce(parse_module("1 + (fn(): Int = 10)()"))

    hst = hoister()
    transduced_ast = hst.transduce(parse_module("1 + (fn(): Int = 10)()"))
    assert len(hst.funcs) == 1
    assert transduced_ast == parse_module("1 + {0}()".format(hst.funcs[0].name))

    hst = hoister()
    transduced_ast = hst.transduce(parse_module("(fn(x: Int): Int = { print(x); x })() + 2 * 3"))
    assert len(hst.funcs) == 1
    assert transduced_ast == parse_module("{0}() + 2 * 3".format(hst.funcs[0].name))


def test_anonymous_functions():
    expander = macro.MacroExpander()
    transduce = expander.transduce
    parse_module = parser.parse_module
    assert expander.need_transduce(parse_module("1 + (fn(): Int = 10)()"))

