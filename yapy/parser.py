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
Integer = Word(pyparsing.nums).setParseAction(lambda t: ast.Integer(int(t[0])))
Float = Regex("[1-9]*[0-9]\.[0-9]+").setParseAction(lambda t: ast.Float(float(t[0])))
Boolean = (Literal("True") | Literal("False")).setParseAction(lambda t: ast.Boolean(t[0] == "True"))
String = (Ign('"') + pyparsing.SkipTo('"') + Ign('"')).setParseAction(lambda t: ast.String(t[0]))
NoneValue = Literal("None").setParseAction(lambda _: ast.NoneValue())
Primitive = (Float | Integer | Boolean | String | NoneValue)

Identifier = Regex("[_a-zA-Z][_a-zA-Z0-9]*")
Variable = Identifier.copy().setParseAction(lambda t: ast.Variable(t[0]))

# Types
Dot = Ign(".").leaveWhitespace()
ModuleName = delimitedList(Identifier, Dot).setParseAction(lambda t: ast.ModuleName(t.asList()))
TypeName = Identifier
Type = Forward()
Comma = Literal(",").setWhitespaceChars(" \t\n")
TypeParameters = (Ign("[") + delimitedList(Type, Comma) + Ign(']')).setParseAction(lambda t: [t.asList()])
BaseType = delimitedList(Identifier, Dot).setParseAction(lambda t: [ast.ModuleName(t[:-1]), t[-1]])
PrimitiveType = BaseType.copy().addParseAction(lambda t: ast.PrimitiveType(t[0], t[1]))
GenericType = (BaseType + TypeParameters).setParseAction(lambda t: ast.GenericType(t[0], t[1], t[2]))
TypeTuple = (Ign('(') + OpNewLines + delimitedList(Type, Comma) + OpNewLines + Ign(')')).setParseAction(
    lambda t: ast.TypeTuple(t.asList()))
Type << (PrimitiveType ^ GenericType ^ TypeTuple)

# Expressions
Expression = Forward()


# Term0: Primitive data structures
def container_elements(elem_parser):
    return Optional(delimitedList(elem_parser, Comma))
List = (Ign('[') + OpNewLines + container_elements(Expression) + OpNewLines + Ign(']')).setParseAction(
    lambda t: ast.List(t.asList()))
Set = (Ign('{') + OpNewLines + container_elements(Expression) + OpNewLines + Ign('}')).setParseAction(
    lambda t: ast.Set(t.asList()))
KeyValue = (Expression + Ign(":") + OpNewLine + Expression).setParseAction(lambda t: ast.KeyValue(t[0], t[1]))
Dict = (Ign('{') + OpNewLines + container_elements(KeyValue) + OpNewLines + Ign('}')).setParseAction(
    lambda t: ast.Dict(t.asList()))
TypedVariable = (Variable +
                 Optional(Ign(":") + OpNewLine + Type, default=ast.Unsolved())).setParseAction(
    lambda t: ast.TypedVariable(t[0], t[1]))
Term0 = (List
         | Dict
         | Set
         | Primitive
         | Variable
         | Ign("(") + Expression + Ign(")"))


# Term1: Attribute reference
Attribute = (Term0 + Ign(".") + Identifier).setParseAction(lambda t: ast.Attribute(t[0], t[1]))
Term1 = (Term0 ^ Attribute)


# Term2: Function call
Arguments = Optional(delimitedList(Expression, Comma).setParseAction(lambda t: [t.asList()]), [])
FunctionCall = (Term1 + Ign('(') + Arguments + Ign(')')).setParseAction(
    lambda t: ast.FunctionCall(t[0], t[1]))
Index = Expression.copy().setParseAction(lambda t: ast.Index(t[0]))
Slice = (Expression + Ign(":") + OpNewLine + Expression).setParseAction(lambda t: ast.Slice(t[0], t[1], ast.Integer(1)))
Range = (Index ^ Slice)
Subscript = (Term1 + Ign('[') + Range + Ign(']')).setParseAction(lambda t: ast.Subscript(t[0], t[1]))
Term2 = (Term1 ^ FunctionCall ^ Subscript)


# Term3: Basic expressions
Statement = Forward()
BlockDelimiter = (Literal(";") ^ OpNewLines)
Block = ((Ign('{') + OpNewLines +
          delimitedList(Statement, BlockDelimiter) +
          OpNewLines + Ign('}')) ^ Expression).setParseAction(lambda t: ast.Block(t.asList()))
If = (Ign("if") + OpNewLine + Expression + OpNewLine +
      Ign("then") + Block + OpNewLine +
      Ign("else") + Block).setParseAction(lambda t: ast.If(t[0], t[1], t[2]))
Parameters = Optional(delimitedList(TypedVariable, Comma).setParseAction(lambda t: [t.asList()]), [])
Function = (Ign("fn") + Ign('(') + Parameters + Ign(')') +
            Optional(Ign(":") + Type, default=ast.Unsolved()) +
            Ign('=') + Block).setParseAction(lambda t: ast.Function(t[0], t[1], t[2]))
Term3 = (If
         | Function
         | Term2)


# Term4: Unary Operation
unary_operators = ["-", "!"]
UnaryOperator = Literals(unary_operators).setParseAction(lambda t: ast.UnaryOperator(t[0]))
UnaryOperation = (UnaryOperator + Expression).setParseAction(lambda t: ast.UnaryOperation(t[1], t[0]))
Term4 = (UnaryOperation
         | Term3)


# Term5: Binary Operation
BinaryOperator = Word("=<>@^|&+-*/%?!~").setParseAction(lambda t: ast.BinaryOperator(t[0]))
BinaryOperations = (Term4 + OneOrMore(OpNewLine + BinaryOperator + OpNewLine + Term4)).setParseAction(
    lambda t: ast.BinaryOperations(t.asList()))
Term5 = BinaryOperations | Term4
Expression << Term5


# Statement
StatementBlockDelimiter = BlockDelimiter
StatementBlock = ((Ign('{') + OpNewLines +
                   delimitedList(Statement, StatementBlockDelimiter) +
                   OpNewLines + Ign('}')) ^ Statement).setParseAction(lambda t: ast.StatementBlock(t.asList()))
FunctionDefinition = (Ign("def") + Identifier +
                      Ign('(') + OpNewLine + Parameters + OpNewLine + Ign(')') +
                      Ign(":") + Type +
                      Ign('=') + OpNewLine + StatementBlock).setParseAction(
    lambda t: ast.FunctionDefinition(t[0], t[1], t[2], t[3]))
For = (Ign("for") + Ign('(') + OpNewLine + Variable + OpNewLine +
       Ign("in") + Expression + OpNewLine + Ign(')') + OpNewLine + Block).setParseAction(
    lambda t: ast.For(t[0], t[1], t[2]))
VariableBinding = (Ign("let") + TypedVariable + OpNewLine +
                   Ign("=") + OpNewLine + Expression).setParseAction(lambda t: ast.VariableBinding(t[0], t[1]))
Import = (Ign("import") + ModuleName).setParseAction(lambda t: ast.Import(t[0]))
Assert = (Ign("assert") + Ign("(") + Expression +
          Optional(Ign(",") + Expression, ast.NoneValue()) +
          Ign(")")).setParseAction(lambda t: ast.Assert(t[0], t[1]))
Statement << (For
              | VariableBinding
              | Import
              | FunctionDefinition
              | Assert
              | Expression)


# Module & Interactive
ImplicitStatementBlock = delimitedList(Statement, StatementBlockDelimiter).setParseAction(
    lambda t: ast.StatementBlock(t.asList()))
Module = ImplicitStatementBlock.copy().addParseAction(lambda t: ast.Module(t[0]))
Interactive = ImplicitStatementBlock.copy().addParseAction(lambda t: ast.Interactive(t[0]))


def parse_module(source: str) -> ast.Module:
    """Parse and Return a yapy Module AST"""
    return Module.parseString(source, parseAll=True)[0]


def parse_interactive(source: str) -> ast.Statement:
    """Parse and Return a yapy Interactive AST"""
    return Interactive.parseString(source, parseAll=True)[0]


def parse_expression(source: str) -> ast.Statement:
    """Parse and Return a yapy expression AST"""
    return Expression.parseString(source, parseAll=True)[0]