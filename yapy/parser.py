import functools
import pyparsing
import yapy.ast as ast


pyparsing.ParserElement.enablePackrat()
pyparsing.ParserElement.setDefaultWhitespaceChars(" \n")

# Parser
pyparsing.ParserElement.enablePackrat()
uppercases = pyparsing.alphas[26:]
keyword_chars = pyparsing.alphanums + "_"
Ign = pyparsing.Suppress
Literal = pyparsing.Literal
Keyword = lambda s: pyparsing.Keyword(s, keyword_chars)
Word = pyparsing.Word
OneOrMore = pyparsing.OneOrMore
ZeroOrMore = pyparsing.ZeroOrMore
Regex = pyparsing.Regex
Forward = pyparsing.Forward
Combine = pyparsing.Combine
Optional = pyparsing.Optional
delimitedList = pyparsing.delimitedList
NewLine = pyparsing.White("\n").suppress()
OpNewLine = Optional(NewLine)
NewLines = OneOrMore(NewLine).suppress()
OpNewLines = Optional(NewLines).suppress()
def Literals(texts: list):
    assert len(texts) > 0
    return functools.reduce(lambda acc, elem: acc | Literal(elem), texts[1:], Literal(texts[0]))


# Primitive Types
Integer = Word(pyparsing.nums)("value").setParseAction(lambda t: ast.Integer(int(t.value)))
Float = Regex("[1-9]*[0-9]\.[0-9]+")("value").setParseAction(lambda t: ast.Float(float(t.value)))
Boolean = (Literal("True") | Literal("False"))("value").setParseAction(lambda t: ast.Boolean(t.value == "True"))
String = (Ign('"') + pyparsing.SkipTo('"')("cnt") + Ign('"')).setParseAction(lambda t: ast.String(t.cnt))
NoneValue = Literal("None").setParseAction(lambda _: ast.NoneValue())
Primitive = (Float | Integer | Boolean | String | NoneValue)

Identifier = Regex("[_a-zA-Z][_a-zA-Z0-9]*")
Variable = Identifier("name").setParseAction(lambda t: ast.Variable(t[0]))
# 下記の形式で変数名が得られない場合がある原因がよくわからない
# Variable = Identifier("name").setParseAction(lambda t: ast.Variable(t.name))

# Types
Dot = Ign(".").leaveWhitespace()
ModuleName = delimitedList(Identifier, Dot)("names").setParseAction(lambda t: ast.ModuleName(t.names.asList()))
TypeName = Identifier
Type = Forward()
Comma = Literal(",").setWhitespaceChars(" \t\n")
TypeParameters = (Ign("[") + delimitedList(Type, Comma)("params") + Ign(']')).setParseAction(
    lambda t: t.params)
def create_type_ast(s: str, loc: int, toks: pyparsing.ParseResults):
    module_name = ast.ModuleName(toks.names[:-1])
    typename = toks.names[-1]
    return ast.PrimitiveType(module_name, typename) \
        if toks.params == "" \
        else ast.GenericType(module_name, typename, toks.params.asList())
PrimitiveType_GenericType = (
    delimitedList(Identifier, Dot)("names") +
    Optional(TypeParameters)("params")).setParseAction(create_type_ast)
TypeTuple = (Ign('(') + OpNewLines + delimitedList(Type, Comma)("types") + OpNewLines + Ign(')')).setParseAction(
    lambda t: ast.TypeTuple(t.types.asList()))
Type << (PrimitiveType_GenericType | TypeTuple)

# Expressions
Expression = Forward()

# Term0: Primitive data structures
List = (Ign('[') + OpNewLines +delimitedList(Expression, Comma)("elems") + OpNewLines + Ign(']')).setParseAction(
    lambda t: ast.List(t.elems.asList()))
Set = (Ign('{') + OpNewLines + delimitedList(Expression, Comma)("elems") + OpNewLines + Ign('}')).setParseAction(
    lambda t: ast.Set(t.elems.asList()))
KeyValue = (Expression("key") + Ign(":") + OpNewLine + Expression("value")).setParseAction(
    lambda t: ast.KeyValue(t.key, t.value))
Dict = (Ign('{') + OpNewLines + delimitedList(KeyValue)("elems") + OpNewLines + Ign('}')).setParseAction(
    lambda t: ast.Dict(t.elems.asList()))
TypedVariable = (Variable("var") +
                 Optional(Ign(":") + OpNewLine + Type, default=ast.Unsolved())("var_type")).setParseAction(
    lambda t: ast.TypedVariable(t.var, t.var_type[0]))
Term0 = (List
         | Set
         | Dict
         | Primitive
         | Variable
         | Ign("(") + Expression + Ign(")"))

# Term1: Attribute reference
Attribute = (Term0("expr") + Ign(".") + Identifier("attr")).setParseAction(lambda t: ast.Attribute(t.expr, t.attr))
Term1 = (Term0 ^ Attribute)

# Term2: Function call
Arguments = Optional(delimitedList(Expression, Comma).setParseAction(lambda t: t.asList()), [])
FunctionCall = (Term1("func") + Ign('(') + Arguments("args") + Ign(')')).setParseAction(
    lambda t: ast.FunctionCall(t.func, t.args.asList()))

def create_Index(s: str, loc: int, toks: pyparsing.ParseResults):
    print("Index:", toks)
    print("Index: value", toks.value)
    return ast.Index(toks.value)
# Index = Expression("value").setParseAction(create_Index)
# Index = Expression("val").setParseAction(lambda t: ast.Index(t.val))
Index = Expression("value").setParseAction(lambda _s, _loc, toks: ast.Index(toks.value))
Slice = (Expression("lower") + Ign(":") + OpNewLine + Expression("upper")).setParseAction(
    lambda t: ast.Slice(t.lower, t.upper, ast.Integer(1)))
# Range = (Index ^ Slice)
Range = Index
def create_Subscript(s: str, loc: int, toks: pyparsing.ParseResults):
    print("Subscript:", toks)
    print("Subscript attr:", toks.asDict())
    return ast.Subscript(toks.value, toks.range)
Subscript = (Term1("value") + Ign('[') + Range("range") + Ign(']')).setParseAction(create_Subscript)
# Subscript = (Term1("value") + Ign('[') + Range("range") + Ign(']')).setParseAction(
#     lambda _s, _loc, toks: ast.Subscript(toks.value, toks.range))
Term2 = (Term1 ^ FunctionCall ^ Subscript)

# Term3: Basic expressions
Statement = Forward()
def create_block_ast(s: str, loc: int, toks: pyparsing.ParseResults):
    if len(toks.stmts) > 0:
        return ast.Block(toks.stmts.asList())
    else:
        return ast.Block([toks.expr])
BlockDelimiter = (Literal(";") ^ OpNewLines)
Block = ((Ign('{') + OpNewLines +
          delimitedList(Statement, BlockDelimiter)("stmts") +
          OpNewLines + Ign('}')) ^
         Expression("expr")).setParseAction(create_block_ast)
If = (Ign("if") + OpNewLine + Expression("cond") + OpNewLine +
      Ign("then") + Block("then_expr") + OpNewLine +
      Ign("else") + Block("else_expr")).setParseAction(
    lambda t: ast.If(t.cond, t.then_expr, t.else_expr))
Parameters = Optional(delimitedList(TypedVariable, Comma).setParseAction(lambda t: t.asList()), [])
Function = (Ign("fn") + Ign('(') + Parameters("params") + Ign(')') +
            Optional(Ign(":") + Type, default=ast.Unsolved())("return_type") +
            Ign('=') + Block("body")).setParseAction(
    lambda t: ast.Function(t.params.asList(), t.return_type[0], t.body))
Term3 = (If
         | Function
         | Term2)

# Term4: Unary Operation
unary_operators = ["-", "!"]
UnaryOperator = Literals(unary_operators)("op").setParseAction(lambda t: ast.UnaryOperator(t.op))
UnaryOperation = (UnaryOperator("op") + Expression("expr")).setParseAction(lambda t: ast.UnaryOperation(t.expr, t.op))
Term4 = (UnaryOperation
         | Term3)

# Term5: Binary Operationw
BinaryOperator = Word("=<>@^|&+-*/%?!~")("bop").setParseAction(lambda t: ast.BinaryOperator(t.bop))
BinaryOperations = (Term4 + OneOrMore(OpNewLine + BinaryOperator + OpNewLine + Term4)).setParseAction(
    lambda t: ast.BinaryOperations(t.asList()))
Term5 = BinaryOperations | Term4
Expression << Term5

# Statement
For = (Ign("for") + Ign('(') + OpNewLine + Variable("var") + OpNewLine +
       Ign("in") + Expression("elems") + OpNewLine + Ign(')') + OpNewLine +
       Block("body")).setParseAction(
    lambda t: ast.For(t.var, t.elems, t.body))
VariableBinding = (Ign("let") + TypedVariable("tvar") +
                   OpNewLine + Ign("=") + OpNewLine + Expression("value")).setParseAction(
    lambda t: ast.VariableBinding(t.tvar, t.value))
Import = (Ign("import") + ModuleName("names")).setParseAction(lambda t: ast.Import(t.names))
FunctionDefinition = (Ign("def") + Identifier("name") +
                      Ign('(') + OpNewLine + Parameters("params") + OpNewLine + Ign(')') +
                      Ign(":") + Type("return_type") +
                      Ign('=') + OpNewLine + Block("body")).setParseAction(
    lambda t: ast.FunctionDefinition(t.name, t.params.asList(), t.return_type, t.body))
Statement << (For
              | VariableBinding
              | Import
              | FunctionDefinition
              | Expression)

# Module & Interactive
Module = delimitedList(Statement, BlockDelimiter)("stmts").setParseAction(lambda t: ast.Module(t.stmts.asList()))
Interactive = delimitedList(Statement, BlockDelimiter)("stmts").setParseAction(
    lambda t: ast.Interactive(t.stmts.asList()))


def parse_module(source: str) -> ast.Module:
    """Parse and Return a yapy Module AST"""
    return Module.parseString(source, parseAll=True)[0]


def parse_interactive(source: str) -> ast.Statement:
    """Parse and Return a yapy Interactive AST"""
    return Interactive.parseString(source, parseAll=True)[0]


def parse_expression(source: str) -> ast.Statement:
    """Parse and Return a yapy expression AST"""
    return Expression.parseString(source, parseAll=True)[0]