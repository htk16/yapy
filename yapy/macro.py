import yapy.ast as ast
import itertools


class MacroExpantionError(Exception):
    """Macro Expantion Error"""
    pass


def expand_macro(node: ast.Node) -> ast.Node:
    """Return a Yapy AST node expanded macros recursively."""
    return MacroExpander().transduce(node)


class MacroExpander(ast.NodeTransducer):
    """Yapy macro expander"""

    OPERATOR_PRIORITIES = {
        "||": 1,
        "&&": 2,
        "in": 3,
        "is": 3,
        "<": 3,
        "<=": 3,
        ">": 3,
        ">=": 3,
        "!=": 3,
        "=": 3,
        "|": 4,
        "^": 5,
        "&": 6,
        "<<": 7,
        ">>": 7,
        "+": 8,
        "-": 8,
        "*": 9,
        "/": 9,
        "//": 9,
        "%": 9,
        "**": 10
    }

    def need_transduce_BinaryOperations(self, _: ast.BinaryOperations) -> bool:
        return True

    def transduce_BinaryOperations(self, node: ast.BinaryOperations) -> ast.Node:
        """Expand a binary operations node to binary operation nodes."""
        exprs = []
        ops = []

        def construct_binary_operation():
            nonlocal exprs, ops
            assert len(exprs) > 1 and len(ops) > 0
            op = ops.pop()["op"]
            rhs = self.transduce(exprs.pop())
            lhs = self.transduce(exprs.pop())
            bop = ast.BinaryOperation(lhs, op, rhs)
            exprs.append(bop)

        exprs_and_ops = node.terms_and_ops
        for i in range(len(exprs_and_ops)):
            if i % 2 == 0:
                # expression
                expr = exprs_and_ops[i]  # type: ast.Expression
                exprs.append(expr)
            else:
                # operator
                op = exprs_and_ops[i]  # type: ast.BinaryOperator
                if op.op not in self.OPERATOR_PRIORITIES:
                    raise MacroExpantionError("unsupported binary operator {0}".format(op.op))
                priority = self.OPERATOR_PRIORITIES[op.op]

                if len(ops) == 0:
                    ops.append({"op": op, "priority": priority})
                else:
                    # compare operator's priority
                    if ops[-1]["priority"] > priority:
                        construct_binary_operation()
                    ops.append({"op": op, "priority": priority})

        while len(ops) > 0:
            construct_binary_operation()

        assert len(exprs) == 1
        return exprs[0]

    def _need_transduce_Statements(self, statements: list) -> bool:
        return any(self.need_transduce(n) for n in
                   itertools.chain.from_iterable(ast.walk_in_scope(s) for s in statements))

    def _iter_transduced_Statements(self, statements: list):
        return (self.transduce(stmt) for stmt in AnonymousFunctionHoister.hoist_anonymous_functions(statements))

    def need_transduce_Block(self, block: ast.Block) -> bool:
        return self._need_transduce_Statements(block.statements)

    def transduce_Block(self, block: ast.Block) -> ast.Block:
        return ast.Block(statements=list(self._iter_transduced_Statements(block.statements)))

    def need_transduce_Module(self, module: ast.Module) -> bool:
        return self._need_transduce_Statements(module.statements)

    def transduce_Module(self, module: ast.Module) -> ast.Module:
        return ast.Module(statements=list(self._iter_transduced_Statements(module.statements)))

    def need_transduce_Interactive(self, interactive: ast.Interactive) -> bool:
        return self._need_transduce_Statements(interactive.statements)

    def transduce_Interactive(self, interactive: ast.Interactive) -> ast.Interactive:
        return ast.Interactive(statements=list(self._iter_transduced_Statements(interactive.statements)))

    def transduce_FunctionDefinition(self, definition: ast.FunctionDefinition) -> ast.FunctionDefinition:
        transduced_body = self.transduce(definition.body)

        if not isinstance(transduced_body.statements[-1], ast.Expression):
            return ast.FunctionDefinition(
                name=definition.name,
                params=definition.params,
                return_type=definition.return_type,
                body=transduced_body)
        else:
            # Transduce last expression to Return statement
            transduced_statements = transduced_body.statements.copy()
            transduced_statements[-1] = ast.Return(value=transduced_statements[-1])
            return ast.FunctionDefinition(
                name=definition.name,
                params=definition.params,
                return_type=definition.return_type,
                body=ast.Block(transduced_statements)
            )

    def need_transduce_FunctionDefinition(self, func_def: ast.FunctionDefinition) -> bool:
        # TODO: Check last statement is a Return statement
        return self.need_transduce(func_def.body) or isinstance(func_def.body.statements[-1], ast.Expression)

    def transduce_Function(self, _: ast.Function) -> ast.Node:
        raise MacroExpantionError("Must not expand a Function node directly.")

    def need_transduce_Function(self, _: ast.Node) -> bool:
        return True


class AnonymousFunctionHoister(ast.NodeTransducer):
    """Anonymous Function Hoister"""
    def __init__(self):
        self._funcs = []

    @classmethod
    def has_Function_node(cls, node: ast.Node) -> bool:
        return any(isinstance(n, ast.Function) for n in ast.walk_in_scope(node))

    @classmethod
    def hoist_anonymous_functions(cls, stmts: list):
        for stmt in stmts:
            if cls.has_Function_node(stmt):
                # Hoist anonymous function
                hoister = AnonymousFunctionHoister()
                transduced_stmt = hoister.transduce(stmt)
                assert len(hoister.funcs) > 0
                yield from hoister.funcs
                yield transduced_stmt
            else:
                yield stmt

    @property
    def funcs(self) -> list:
        return self._funcs

    def _add_hoisted_function(self, node: ast.FunctionDefinition):
        self._funcs.append(node)

    def transduce_Function(self, node: ast.Function) -> ast.Node:
        import uuid
        # TODO: Return Lambda node if node's body has only one expression
        # TODO: Check conflict other names
        func_name = "_func_{0}".format(str(uuid.uuid4()).replace("-", "_"))
        hoisted_func = ast.FunctionDefinition(
            name=func_name,
            params=node.params,
            return_type=node.return_type,
            body=node.body)
        self._add_hoisted_function(hoisted_func)
        return ast.Variable(func_name)

    def need_transduce_Function(self, _: ast.Function) -> bool:
        # Function node be must transduced to Lambda or FunctionDefinition
        return True

    def need_transduce_Block(self, _: ast.Block) -> bool:
        return False


class ResultStoringTransducer(ast.NodeTransducer):
    def __init__(self, result_name: str):
        self._result_name = result_name

    def _add_result_storing_operation(self, node):
        """Return Module or Interative AST node added a result storing operation

        :param node: Module or Interactive AST node
        :return: transduced AST node to added result storing operation
        """
        if isinstance(node.statements[-1], ast.Expression):
            transduced_statements = node.statements.copy()
            # result = transduced_statements[-1]
            # is_none = ast.BinaryOperation(
            #     lhs=result,
            #     op=ast.BinaryOperator(op="!="),
            #     rhs=ast.NoneValue()
            # )
            # bind_result = ast.VariableBinding(
            #     var=ast.TypedVariable(ast.Variable(self._result_name), ast.Any),
            #     expr=result
            # )
            # transduced_statements[-1] = ast.IfStatement(
            #     cond=is_none,
            #     then_statements=[bind_result],
            #     else_statements=[]
            # )
            transduced_statements[-1] = ast.VariableBinding(
                var=ast.TypedVariable(ast.Variable(self._result_name), ast.Any),
                expr=node.statements[-1]
            )
            return node.__class__(statements=transduced_statements)
        else:
            return node

    def need_transduce_Module(self, _: ast.Module) -> bool:
        return True

    def transduce_Module(self, module: ast.Module) -> ast.Module:
        return self._add_result_storing_operation(module)

    def need_transduce_Interactive(self, _: ast.Interactive) -> bool:
        return True

    def transduce_Interactive(self, interactive: ast.Interactive) -> ast.Interactive:
        return self._add_result_storing_operation(interactive)
