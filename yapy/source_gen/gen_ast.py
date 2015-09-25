AST_DOCUMENT = '''"""ast.py: yapy AST definitions

This source is generated by source_gen/gen_ast.py.
Don't modify this source directly.
"""
'''
MODULES = ["functools"]
NODE_DEFINITION = "node.py"


class ASTDef:
    """AST definition"""
    def __init__(self, name: str, super_class: str, doc: str, fields: list, **opts):
        self.name = name
        self.super_class = super_class
        self.doc = doc
        self.fields = fields
        self.opts = opts

    HEADER_TEMPLATE = '''class {name}({super_class}):
    """{doc}"""
'''

    def __str__(self):
        header = self.HEADER_TEMPLATE.format(
            name=self.name,
            super_class=self.super_class,
            doc=self.doc)

        if self.opts.get("abstract", False):
            # Abstract class
            return header
        else:
            # Concrete class
            init = self._init()
            properties = self._properties()
            methods = self._methods()
            body = "\n".join(filter(lambda s: s != "", (init, properties, methods)))
            return "{0}{1}".format(header, body)

    INIT_TEMPLATE = '''    def __init__(self{separator}{args}):
        self._init_fields({init_fields})
    '''

    def _init(self) -> str:
        if len(self.fields) == 0:
            separator = ""
        else:
            separator = ", "
        args = ", ".join("{0}: {1}".format(t[0], t[1]) for t in self.fields)
        init_fields = "{{{0}}}".format(
            ", ".join('"{0}": {0}'.format(t[0]) for t in self.fields))
        return self.INIT_TEMPLATE.format(separator=separator, args=args, init_fields=init_fields)

    PROPERTY_TEMPLATE = '''    @property
    def {field}(self) -> {field_type}:
        return self._get_field_value("{field}")
    '''

    def _properties(self) -> str:
        return "\n".join(self.PROPERTY_TEMPLATE.format(field=field, field_type=field_type)
                         for field, field_type in self.fields)

    IS_BLOCK = '''    def is_block(self) -> bool:
        """Return True if self is a block node (Block or StatementBlock)"""
        return True
        '''

    def _methods(self) -> str:
        def _listup_methods():
            if self.opts.get("block", False):
                yield self.IS_BLOCK

            additions = self.opts.get("additions", "")
            if additions != "":
                yield additions

        return "\n".join(_listup_methods())


class Node(ASTDef):
    def __init__(self, name: str, *args, **kwargs):
        ASTDef.__init__(self, name, "Node", *args, **kwargs)


class Statement(ASTDef):
    def __init__(self, name: str, *args, **kwargs):
        ASTDef.__init__(self, name, "Statement", *args, **kwargs)


class Expression(ASTDef):
    def __init__(self, name: str, *args, **kwargs):
        ASTDef.__init__(self, name, "Expression", *args, **kwargs)


class Type(ASTDef):
    def __init__(self, name: str, *args, **kwargs):
        ASTDef.__init__(self, name, "Type", *args, **kwargs)


TOP_AST_DEFINITIONS = (
    # Module
    Node("Module", """Module

    Module ::= StatementBlock
    """, [("block", '"StatementBlock"')]),

    # Interactive
    Node("Interactive", """Interactive

    Interactive ::= StatementBlock
    """, [("block", '"StatementBlock"')]),

    # StatementBlock
    Node("StatementBlock", """StatementBlock

    StatementBlock ::= '{' Statements '}' | Statement
    """, [("statements", "list")], block=True),
)


STATEMENT_AST_DEFINITIONS = (
    # Statement
    Node("Statement", """Abstract class for Statement

    Statement ::=  VariableBinding
                | Import
                | FunctionDefinition
                | Return
                | IfStatement
                | Assert
                | Expression
    """, [], abstract=True),

    # ModuleName
    Node("ModuleName", """Module name

    ModuleName::= Identifier { '.' Identifier }
    """, [("names", "list")],
         additions="""    @property
    def name(self) -> str:
        return ".".join(self.names)
    """),

    # Import
    Statement("Import", """Module import

    Import ::= 'import' ModuleName
    """, [("module_name", "ModuleName")]),

    # FunctionDefinition
    Statement("FunctionDefinition", """Function definition

    FunctionDefinition ::= 'fun' Identifier '(' Parameters ')' -> Type ':' FunctionBody
    FunctionBody ::= '{' Statements '}' | Statement
    Parameters ::= [ TypedVariable { ',' TypedVariable } ]
    """, [("name", "str"),
          ("params", "list"),
          ("return_type", '"Type"'),
          ("body", "StatementBlock")]),

    # Return
    Statement("Return", "Return statement", [("value", '"Expression"')]),

    # IfStatement
    Statement("IfStatement", "If statement", [("cond", '"Expression"'),
                                              ("then_statement", "list"),
                                              ("else_statement", "list")]),

    # Assert
    Statement("Assert", """Assertion

    Assert ::= 'assert' '(' Expression [',' Expression] ')'
    """, [("expr", '"Expression"'),
          ("msg", '"Expression"')]),

    # VariableBinding
    Statement("VariableBinding", """Variable Binding

    VariableBinding ::= 'let' TypedVariable '=' Expression
    """, [("var", '"TypedVariable"'), ("expr", '"Expression"')])
)


EXPRESSION_AST_DEFINITIONS = (
    # Exression
    Node("Expression", """Abstract class for Expression

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
    """, [], abstact=True),

    # Block
    Expression("Block", """Block

    Block ::= '{' Statements '}'
    """, [("statements", "list")], block=True),

    # If
    Expression("If", """if expression

    If ::= 'if' Expression 'then' IfBlock [ 'else' IfBlock ]
    IfBlock ::= Expression | Block
    """, [("cond", "Expression"),
          ("then_expr", "Expression"),
          ("else_expr", "Expression")]),

    # For
    Expression("For", """For expression

    For ::= 'for' '(' Variable 'in' Expression ')' Expression
    """, [("var", '"Variable"'),
          ("expr", "Expression"),
          ("body", "Expression")]),

    # FunctionCall
    Expression("FunctionCall", """Function call

    FunctionCall ::= Expression '(' [ Expression { ',' Expression } ] ')'
    """, [("func", "Expression"),
          ("params", "list")]),

    # Subscript
    Expression("Subscript", """Subscript

    Subscript ::= Expression '[' (Index | Slice) ']'
    Index ::= Expression
    Slice ::= Expression ':' Expression
    """, [("value", "Expression"),
          ("range", "Node")]),

    # Index
    Node("Index", """Subscript index""", [("value", "Expression")]),

    # Slice
    Node("Slice", """Subscript slice""",
         [("lower", "Expression"),
          ("upper", "Expression"),
          ("step", "Expression")]),

    # UnaryOperator
    Node("UnaryOperator", "Unary operator", [("op", "str")]),

    # UnaryOperation
    Expression("UnaryOperation", """Unary operation

    UnaryOperation ::= UnaryOperator Expression
    """, [("expr", "Expression"),
          ("op", "UnaryOperator")]),

    # BinaryOperations
    Expression("BinaryOperations", """Binary operations

    BinaryOperations ::= Term1 { BinaryOperator Term1 }
    """, [("terms_and_ops", "list")]),

    # BinaryOperator
    Node("BinaryOperator", "Binary Operator", [("op", "str")]),

    # BinaryOperation
    Expression("BinaryOperation", "Binary Operation",
               [("lhs", "Expression"),
                ("op", "BinaryOperator"),
                ("rhs", "Expression")]),

    # Attribute
    Expression("Attribute", """Attribute

    Attribute ::= Expression '.' Identifier
    """, [("expr", "Expression"),
          ("attr", "str")]),

    # List
    Expression("List", """List

    List ::= '[' [ Expression { ',' Expression } ] ']'
    """, [("exprs", "list")]),

    # Set
    Expression("Set", """Set

    Set ::= '{' [ Expression { ',' Expression } ] '}'
    """, [("exprs", "list")]),

    # Dict
    Expression("Dict", """Dictionary

    Dict ::= '{' [ KeyValue { ',' KeyValue } ] '}'
    """, [("kvs", "list")]),

    # KeyValue
    Node("KeyValue", """Key Value pair

    KeyValue ::= Expression ':' Expression
    """, [("key", "Expression"),
          ("value", "Expression")]),

    # Function
    Expression("Function", """Anonymous function

    Function ::= 'fn' '(' Parameters ')' -> Type ':' Block
    """, [("params", "list"),
          ("return_type", '"Type"'),
          ("body", "Expression")]),

    # Variable
    Expression("Variable", """Variable

    Variable ::= Identifier
    """, [("name", "str")]),

    # TypedVariable
    Expression("TypedVariable", """Typed Variable

    TypedVariable ::= Variable ':' Type
    """, [("var", "Variable"),
          ("type_name", '"Type"')]),

    # String
    Expression("String", """String

    String ::= r"\".*\""
    """, [("contents", "str")]),

    # Integer
    Expression("Integer", """Integer

    Integer ::= r"-?[1-9]*[0-9]"
    """, [("value", "int")]),

    # Float
    Expression("Float", """Float number

    Float ::= r"-?[1-9]*[0-9](\.[0-9]+)"
    """, [("value", "float")]),

    # Boolean
    Expression("Boolean", """Boolean

    Boolean ::= 'True' | 'False'
    """, [("value", "bool")]),

    # NoneValue
    Expression("NoneValue", """None value

    None ::= 'None'
    """, [])
)


TYPE_AST_DEFINITIONS = (
    # Type
    Node("Type", """Abstract class for type notations

    Type ::= PrimitiveType
           | GenericType
           | TypeTuple
           | Any
           | Unsolved
    """, [], abstract=True),

    # PrimitiveType
    Type("PrimitiveType", """Primitive type

    PrimitiveType ::= ModuleName TypeName
    TypeName ::= r"[A-Z][a-zA-Z_1-9]*"
    """, [("module_name", "ModuleName"),
          ("name", "str")]),

    # GenericType
    Type("GenericType", """Generic type

    GenericType ::= ModuleName TypeName '[' TypeParameters ']'
    TypeParameters ::= Type { ',' Type }
    """, [("module_name", "ModuleName"),
          ("name", "str"),
          ("params", "list")]),

    # TypeTuple
    Type("TypeTuple", """Type Tuple

    TypeTuple ::= '(' Type { ',' Type } ')'
    """, [("types", "list")]),

    # Any
    Type("Any", "Any type", []),

    # Unsolved
    Type("Unsolved", "Unsolved type", [])
)


AST_DEFINITIONS = TOP_AST_DEFINITIONS + \
                  STATEMENT_AST_DEFINITIONS + \
                  EXPRESSION_AST_DEFINITIONS + \
                  TYPE_AST_DEFINITIONS


def generate_ast_definitions(os):
    """Generate yapy AST definitions"""
    insert_two_lines = lambda: os.write("\n\n")

    # write doc strings
    os.write(AST_DOCUMENT)

    # write imports
    for module in MODULES:
        os.write("import {0}\n".format(module))
    insert_two_lines()

    # write Node class and functions
    with open(NODE_DEFINITION) as node_def:
        os.write(node_def.read())

    # write AST definitions
    insert_two_lines()
    for ast_def in AST_DEFINITIONS:
        os.write(str(ast_def))
        insert_two_lines()


if __name__ == "__main__":
    import sys
    generate_ast_definitions(sys.stdout)