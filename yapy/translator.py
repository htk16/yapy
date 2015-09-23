import ast as pyast
import yapy.ast as ast


class TranslationError(Exception):
    """Translation Error"""
    pass


def translate(node: ast.Node):
    """Retrun a Python AST translated from a Yapy AST."""
    return PythonTranslator().translate(node)


def create_Compare_constructor(op):
    def create_Compare(translator: "PythonTranslator", node: ast.BinaryOperation) -> pyast.Compare:
        return pyast.Compare(
            left=translator.translate(node.lhs),
            ops=[op],
            comparators=[translator.translate(node.rhs)])
    return create_Compare


def create_BinOp_constructor(op):
    def create_BinOp(translator: "PythonTranslator", node: ast.BinaryOperation) -> pyast.BinOp:
        return pyast.BinOp(
            left=translator.translate(node.lhs),
            op=op,
            right=translator.translate(node.rhs))
    return create_BinOp


class PythonTranslator:
    """Translator from Yapy to Python"""

    def translate(self, node):
        """translate a Yapy AST to a Python AST"""
        if isinstance(node, ast.Node):
            translator_name = 'translate_' + node.__class__.__name__
            translator = getattr(self, translator_name, None)
            if translator is None:
                raise TranslationError("Unsupported Yapy AST Node: {0}".format(node.__class__.__name__))
            return translator(node)
        elif isinstance(node, list):
            return list(self.translate(child) for child in node)
        else:
            raise TranslationError("Unsupported type: {0} {1}".format(type(node), str(node)))

    def translate_as_stmt(self, node):
        if isinstance(node, ast.Expression):
            return pyast.Expr(value=self.translate(node))
        elif isinstance(node, ast.Statement):
            return self.translate(node)
        elif isinstance(node, list):
            return list(self.translate_as_stmt(child) for child in node)
        else:
            raise TranslationError("Unsupported Yapy AST Node: {0}".format(node))

    def _translate_python_arguments(self, tvars: list) -> pyast.arguments:
        """Translate typed variable list to python 'arguments' AST"""
        return pyast.arguments(
            args=list(pyast.arg(arg=tv.var.name, annotation=None) for tv in tvars),
            vararg=None,
            kwonlyargs=[],
            kw_defaults=[],
            kwarg=None,
            defaults=[])

    def translate_Module(self, node: ast.Module) -> pyast.Module:
        return pyast.Module(body=self.translate(node.block))

    def translate_Interactive(self, node: ast.Interactive) -> pyast.Interactive:
        return pyast.Interactive(body=self.translate(node.block))

    def translate_Import(self, node: ast.Import) -> pyast.Import:
        return pyast.Import(names=[pyast.alias(name=node.module_name.name, asname=None)])

    def translate_StatementBlock(self, node: ast.StatementBlock) -> list:
        return [self.translate_as_stmt(stmt) for stmt in node.statements]

    def translate_FunctionDefinition(self, node: ast.FunctionDefinition) -> pyast.FunctionDef:
        return pyast.FunctionDef(
            name=node.name,
            args=self._translate_python_arguments(node.params),
            body=self.translate(node.body),
            decorator_list=[],
            returns=None)

    def translate_Return(self, node: ast.Return) -> pyast.Return:
        return pyast.Return(value=self.translate(node.value))

    def translate_IfStatement(self, if_stmts: ast.IfStatement) -> pyast.If:
        return pyast.If(test=if_stmts.cond,
                        body=if_stmts.then_statements,
                        orelse=if_stmts.else_statements)

    def translate_Assert(self, node: ast.Assert) -> pyast.Assert:
        return pyast.Assert(test=self.translate(node.expr),
                            msg=self.translate(node.msg))

    def translate_VariableBinding(self, node: ast.VariableBinding) -> pyast.Assign:
        return pyast.Assign(
            targets=[pyast.Name(
                id=node.var.var.name,
                ctx=pyast.Store())],
            value=self.translate(node.expr))

    def translate_If(self, node: ast.If) -> pyast.IfExp:
        return pyast.IfExp(
            test=self.translate(node.cond),
            body=self.translate(node.then_expr),
            orelse=self.translate(node.else_expr))

    def translate_Attribute(self, node: ast.Attribute) -> pyast.Attribute:
        return pyast.Attribute(
            value=self.translate(node.expr),
            attr=node.attr,
            ctx=pyast.Load())

    def translate_FunctionCall(self, node: ast.FunctionCall) -> pyast.Call:
        return pyast.Call(
            func=self.translate(node.func),
            args=list(self.translate(arg) for arg in node.params),
            keywords=[],
            starargs=None,
            kwargs=None)

    def translate_Subscript(self, node: ast.Subscript) -> pyast.Subscript:
        return pyast.Subscript(
            value=self.translate(node.value),
            slice=self.translate(node.range),
            ctx=pyast.Load())

    def translate_Index(self, node: ast.Index) -> pyast.Index:
        return pyast.Index(value=self.translate(node.value))

    def translate_Slice(self, node: ast.Slice) -> pyast.Slice:
        return pyast.Slice(
            lower=self.translate(node.lower),
            upper=self.translate(node.upper),
            step=self.translate(node.step))

    UNARY_OPERATORS = {"-": pyast.USub(),
                       "!": pyast.Not()}

    def translate_UnaryOperation(self, node: ast.UnaryOperation) -> pyast.UnaryOp:
        if node.op.op not in self.UNARY_OPERATORS:
            raise TranslationError("Unsupported unary operator: {0}".format(node.op))

        return pyast.UnaryOp(
            op=self.UNARY_OPERATORS[node.op.op],
            operand=self.translate(node.expr))

    BINARY_OPERATORS = {"+": create_BinOp_constructor(pyast.Add()),
                        "-": create_BinOp_constructor(pyast.Sub()),
                        "**": create_BinOp_constructor(pyast.Pow()),
                        "*": create_BinOp_constructor(pyast.Mult()),
                        "/": create_BinOp_constructor(pyast.Div()),
                        "%": create_BinOp_constructor(pyast.Mod()),
                        "|": create_BinOp_constructor(pyast.BitOr()),
                        "&": create_BinOp_constructor(pyast.BitAnd()),
                        "^": create_BinOp_constructor(pyast.BitXor()),
                        "<<": create_Compare_constructor(pyast.LShift()),
                        ">>": create_Compare_constructor(pyast.RShift()),
                        "=": create_Compare_constructor(pyast.Eq()),
                        "!=": create_Compare_constructor(pyast.NotEq()),
                        "<=": create_Compare_constructor(pyast.LtE()),
                        "<": create_Compare_constructor(pyast.Lt()),
                        ">=": create_Compare_constructor(pyast.GtE()),
                        ">": create_Compare_constructor(pyast.Gt())}

    def translate_BinaryOperation(self, node: ast.BinaryOperation) -> pyast.BinOp:
        if node.op.op not in self.BINARY_OPERATORS:
            raise TranslationError("Unsupported binary operator: {0}".format(node.op))

        constructor = self.BINARY_OPERATORS[node.op.op]
        return constructor(self, node)

    def translate_List(self, node: ast.List) -> pyast.List:
        return pyast.List(
            elts=list(self.translate(expr) for expr in node.exprs),
            ctx=pyast.Load())

    def translate_Set(self, node: ast.Set) -> pyast.Set:
        return pyast.Set(
            elts=list(self.translate(expr) for expr in node.exprs),
            ctx=pyast.Load())

    def translate_Dict(self, node: ast.Dict) -> pyast.Dict:
        return pyast.Dict(
            keys=list(self.translate(kv.key) for kv in node.kvs),
            values=list(self.translate(kv.value) for kv in node.kvs))

    def translate_Variable(self, node: ast.Variable, ctx=pyast.Load()) -> pyast.Name:
        return pyast.Name(id=node.name, ctx=ctx)

    def translate_TypedVariable(self, node: ast.TypedVariable, ctx=pyast.Load()) -> pyast.Name:
        return self.translate(node.var, ctx)

    def translate_String(self, node: ast.String) -> pyast.Str:
        return pyast.Str(s=node.contents)

    def translate_Integer(self, node: ast.Integer) -> pyast.Num:
        return pyast.Num(n=node.value)

    def translate_Float(self, node: ast.Float) -> pyast.Num:
        return pyast.Num(n=node.value)

    def translate_Boolean(self, node: ast.Boolean) -> pyast.NameConstant:
        return pyast.NameConstant(value=node.value)

    def translate_NoneValue(self, _: ast.NoneValue) -> pyast.NameConstant:
        return pyast.NameConstant(value=None)

