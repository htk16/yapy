import yapy.ast as ast
import itertools
import functools
import enum


class MacroExpantionError(Exception):
    """Macro Expantion Error"""
    pass


def expand_macro(node: ast.Node) -> ast.Node:
    """Return a Yapy AST node expanded macros recursively."""
    # TODO expand macros until halting
    return MacroExpander().transduce(BlockHoister().transduce(node))


class BlockHoister(ast.NodeTransducer):
    """Block hoister"""
    def _need_transduce_Statements(self, statements: list) -> bool:
        return any(self.need_transduce(n) for n in
                   itertools.chain.from_iterable(ast.walk_in_scope(s) for s in statements))

    def _iter_transduced_Statements(self, statements: list):
        return (self.transduce(stmt) for stmt in self.AnonymousFunctionHoister.hoist_anonymous_functions(statements))

    def need_transduce_StatementBlock(self, block: ast.StatementBlock) -> bool:
        return self._need_transduce_Statements(block.statements)

    def transduce_StatementBlock(self, block: ast.StatementBlock) -> ast.StatementBlock:
        return ast.StatementBlock(statements=list(self._iter_transduced_Statements(block.statements)))

    def need_transduce_Block(self, _: ast.Block) -> bool:
        return True

    def transduce_Block(self, _: ast.Block):
        raise MacroExpantionError("Must not expand a Block node directly.")

    def transduce_Function(self, _: ast.Function) -> ast.Node:
        raise MacroExpantionError("Must not expand a Function node directly.")

    def need_transduce_Function(self, _: ast.Node) -> bool:
        return True

    class AnonymousFunctionHoister(ast.NodeTransducer):
        """Anonymous Function Hoister"""
        def __init__(self):
            self._funcs = []

        @classmethod
        def is_target_node(cls, node: ast.Node) -> bool:
            return isinstance(node, ast.Function) or \
                   isinstance(node, ast.If)

        @classmethod
        def has_target_node(cls, node: ast.Node) -> bool:
            return any(cls.is_target_node(n) for n in ast.walk_in_scope(node))

        @classmethod
        def hoist_anonymous_functions(cls, stmts: list):
            for stmt in stmts:
                if cls.has_target_node(stmt):
                    # Hoist anonymous function
                    hoister = cls()
                    transduced_stmt = hoister.transduce(stmt)
                    yield from hoister.funcs
                    yield transduced_stmt
                else:
                    yield stmt

        @classmethod
        def convert_Block_to_FunctionBody(cls, block: ast.Block) -> ast.StatementBlock:
            return ast.StatementBlock(block.statements)

        def _create_hoisted_function(self, params: list, body: ast.StatementBlock, return_type: ast.Type) -> str:
            """Create a hoisted function and return its name

            :param params: function parameters
            :param body: function body
            :param return_type: return type
            :return: name of a hoisted function
            """
            import uuid
            # TODO: Check conflict other names
            func_name = "_func_{0}".format(str(uuid.uuid4()).replace("-", "_"))
            hoisted_func = ast.FunctionDefinition(
                name=func_name,
                params=params,
                return_type=return_type,
                body=body)
            self._funcs.append(hoisted_func)  # add hoisted function
            return func_name

        @property
        def funcs(self) -> list:
            return self._funcs

        def transduce_Function(self, node: ast.Function) -> ast.Node:
            # TODO: Return Lambda node if node's body has only one expression
            func_body = self.convert_Block_to_FunctionBody(node.body)
            func_name = self._create_hoisted_function(node.params, func_body, node.return_type)
            return ast.Variable(func_name)

        def need_transduce_Function(self, _: ast.Function) -> bool:
            # Function node be must transduced to Lambda or FunctionDefinition
            return True

        def transduce_Block(self, block: ast.Block) -> ast.Node:
            if len(block.statements) == 1 and isinstance(block.statements[0], ast.Expression):
                # single expression block
                return block.statements[0]
            else:
                # multi statement block
                func_body = self.convert_Block_to_FunctionBody(block)
                func_name = self._create_hoisted_function([], func_body, ast.Unsolved)
                return ast.FunctionCall(func=ast.Variable(func_name), params=[])

        def need_transduce_Block(self, _: ast.Block) -> bool:
            return True


class BinaryOperatorInfo:
    """Binary operator infomation"""
    __slots__ = ["operator", "priority", "association"]

    class Association(enum.Enum):
        LEFT = 0
        RIGHT = 1

    def __init__(self, op: str, priority: int, association):
        self.operator = op
        self.priority = priority
        self.association = association

    @classmethod
    def create_operator_priorities(cls, defines) -> dict:
        priorities = {}
        for priority, define in enumerate(defines):
            for association, ops in define.items():
                for op in ops:
                    priorities[op] = cls(ast.BinaryOperator(op), priority, association)
        return priorities


class MacroExpander(ast.NodeTransducer):
    """Yapy macro expander"""

    _LEFT = BinaryOperatorInfo.Association.LEFT
    _RIGHT = BinaryOperatorInfo.Association.RIGHT

    _OPERATOR_PRIORITIES = (
        # Low priorities
        {_LEFT: (), _RIGHT: ("||", )},
        {_LEFT: (), _RIGHT: ("&&", )},
        {_LEFT: ("in", "is", "<", "<=", ">", ">=", "!=", "="), _RIGHT: ()},
        {_LEFT: (), _RIGHT: ("|", )},
        {_LEFT: (), _RIGHT: ("^", )},
        {_LEFT: (), _RIGHT: ("&", )},
        {_LEFT: (), _RIGHT: ("<<", ">>")},
        {_LEFT: (), _RIGHT: ("+", "-")},
        {_LEFT: (), _RIGHT: ("*", "/", "//", "%")},
        {_LEFT: (), _RIGHT: ("**", )},
        {_LEFT: (".", ), _RIGHT: ()},
        # High priorities
    )

    OPERATOR_PRIORITIES = BinaryOperatorInfo.create_operator_priorities(_OPERATOR_PRIORITIES)

    def need_transduce_BinaryOperations(self, _: ast.BinaryOperations) -> bool:
        return True

    def transduce_BinaryOperations(self, node: ast.BinaryOperations) -> ast.Node:
        """Expand a binary operations node to binary operation nodes."""
        exprs = []
        ops = []

        def get_prev_priority():
            nonlocal ops
            assert len(ops) != 0
            return ops[-1].priority

        def pop_prev_op():
            nonlocal ops
            assert len(ops) != 0
            return ops.pop().operator

        def construct_binary_operation():
            nonlocal exprs, ops
            assert len(exprs) > 1 and len(ops) > 0
            op = pop_prev_op()
            rhs = self.transduce(exprs.pop())
            lhs = self.transduce(exprs.pop())
            bop = ast.BinaryOperation(lhs, op, rhs)
            exprs.append(bop)

        exprs_and_ops = node.terms_and_ops
        for i, v in enumerate(exprs_and_ops):
            if i % 2 == 0:
                # expression
                exprs.append(v)
            else:
                # operator
                op = v  # type: ast.BinaryOperator
                if op.op not in self.OPERATOR_PRIORITIES:
                    raise MacroExpantionError("unsupported binary operator {0}".format(op.op))
                info = self.OPERATOR_PRIORITIES[op.op]  # type: BinaryOperatorInfo

                while (len(ops) > 0) and \
                        ((info.association == BinaryOperatorInfo.Association.LEFT and get_prev_priority() >= info.priority) or \
                         (info.association == BinaryOperatorInfo.Association.RIGHT and get_prev_priority() > info.priority)):
                    construct_binary_operation()
                ops.append(info)

        while len(ops) > 0:
            construct_binary_operation()

        assert len(exprs) == 1
        return exprs[0]

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
                body=ast.StatementBlock(transduced_statements))

    def need_transduce_FunctionDefinition(self, func_def: ast.FunctionDefinition) -> bool:
        return self.need_transduce(func_def.body) or isinstance(func_def.body.statements[-1], ast.Expression)


class ResultStoringTransducer(ast.NodeTransducer):
    def __init__(self, result_name: str):
        self._result_name = result_name

    def _add_result_storing_operation(self, node):
        """Return Module or Interative AST node added a result storing operation

        :param node: Module or Interactive AST node
        :return: transduced AST node to added result storing operation
        """
        if isinstance(node.block.statements[-1], ast.Expression):
            transduced_statements = node.block.statements.copy()
            transduced_statements[-1] = ast.VariableBinding(
                var=ast.TypedVariable(ast.Variable(self._result_name), ast.Any()),
                expr=transduced_statements[-1]
            )
            transduced_statement_block = ast.StatementBlock(statements=transduced_statements)
            return node.__class__(block=transduced_statement_block)
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
