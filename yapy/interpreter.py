import argparse
import ast
import enum
import yapy.parser
import yapy.macro
import yapy.translator
import pyparsing
import prompt_toolkit.shortcuts
import prompt_toolkit.history


class InterpreterError(Exception):
    """Yapy interpreter Error"""
    pass


def compile_yapy(source: str, filename: str, mode: str="exec", verbose=False, result_name=None):
    """Compile a yapy source to a python code object

    :param source: yapy source text
    :param filename: source file name
    :param mode: compilation mode ("exec", "single" or "eval")
    :param verbose: print debug infomation if True
    :param result_name: variable name to store a result (if result_name is None, result isn't preserved)
    :return: python code object
    """
    # Parse a yapy program
    if mode == "exec":
        parsed_ast = yapy.parser.parse_module(source)
    elif mode == "single":
        parsed_ast = yapy.parser.parse_interactive(source)
    elif mode == "eval":
        parsed_ast = yapy.parser.parse_expression(source)
    else:
        raise InterpreterError("Unknown exec mode: {0}".format(mode))

    if verbose:
        print("parse result:", parsed_ast)

    # Macro expansion
    expanded_ast = yapy.macro.expand_macro(parsed_ast)
    if verbose:
        print("macro expanded:", expanded_ast)

    # Transduce AST to store result
    if (result_name is not None) and (mode in ("exec", "single")):
        expanded_ast = yapy.macro.ResultStoringTransducer(result_name).transduce(expanded_ast)
        if verbose:
            print("macro expanded:", expanded_ast)

    # Translate to Python AST
    translated_python_ast = ast.fix_missing_locations(yapy.translator.translate(expanded_ast))
    if verbose:
        print(ast.dump(translated_python_ast))

    # Compile to Python Bite code
    code_object = compile(translated_python_ast, filename, mode)

    return code_object


def create_initial_environment() -> dict:
    """Create and return an initial yapy environment"""
    def yapy_assert(v: bool):
        assert v

    env = {"__builtins__": globals()["__builtins__"],
           "require": yapy_assert}
    return env


class ExecutionStatus(enum.Enum):
    OK = 0
    KEYBOARD_INTERRUPT = 1
    PARSE_ERROR = 2
    MACRO_EXPANSION_ERROR = 3
    TRANSLATION_ERROR = 4
    ASSERTION_ERROR = 5
    INTERNAL_ERROR = 6


class ExecutionResults:
    """yapy execution results"""
    def __init__(self, status, env: dict, exception: Exception=None):
        self._status = status
        self._env = env
        self._exception = exception

    @classmethod
    def create_execution_results(cls, env: dict, e: Exception=None):
        if e is None:
            return cls(ExecutionStatus.OK, env)
        elif isinstance(e, KeyboardInterrupt):
            return cls(ExecutionStatus.KEYBOARD_INTERRUPT, env, e)
        elif isinstance(e, pyparsing.ParseException):
            return cls(ExecutionStatus.PARSE_ERROR, env, e)
        elif isinstance(e, yapy.macro.MacroExpantionError):
            return cls(ExecutionStatus.MACRO_EXPANSION_ERROR, env, e)
        elif isinstance(e, yapy.translator.TranslationError):
            return cls(ExecutionStatus.TRANSLATION_ERROR, env, e)
        elif isinstance(e, AssertionError):
            return cls(ExecutionStatus.ASSERTION_ERROR, env, e)
        else:
            return cls(ExecutionStatus.INTERNAL_ERROR, env, e)

    @property
    def status(self):
        return self._status

    @property
    def env(self) -> dict:
        return self._env

    @property
    def exception(self) -> Exception:
        return self._exception

    MESSAGES = {ExecutionStatus.PARSE_ERROR: "Syntax error: {0}",
                ExecutionStatus.KEYBOARD_INTERRUPT: "",
                ExecutionStatus.MACRO_EXPANSION_ERROR: "Macro expansion error: {0}",
                ExecutionStatus.TRANSLATION_ERROR: "Translation error: {0}",
                ExecutionStatus.ASSERTION_ERROR: "Assertion error: {0}",
                ExecutionStatus.INTERNAL_ERROR: "INTERNAL ERROR!"}

    @property
    def ok(self) -> bool:
        return self.status == ExecutionStatus.OK

    @property
    def fatal(self) -> bool:
        return self.status == ExecutionStatus.INTERNAL_ERROR

    @property
    def message(self) -> str:
        if self.ok:
            return ""
        else:
            return self.MESSAGES[self.status].format(self.exception)


def run_REPL(verbose: bool=False, env: dict=None) -> ExecutionResults:
    """Execute yapy Read-Eval-Print-Loop"""
    def _output_error(msg):
        import sys
        print(msg, file=sys.stderr)

    if env is None:
        env = create_initial_environment()

    result_id = 1
    history = prompt_toolkit.history.History()

    try:
        while True:
            try:
                source = prompt_toolkit.shortcuts.get_input("In [{0}]: ".format(result_id), history=history)
                result_name = "_{0}".format(result_id)
                code_object = compile_yapy(source, "<REPL>", "single", verbose=verbose, result_name=result_name)
                exec(code_object, env, env)
                result = env.get(result_name, None)
                if result is not None:
                    print("Out[{0}]:".format(result_id), result)
                print()
                result_id += 1
            except pyparsing.ParseException as e:
                _output_error(ExecutionResults.create_execution_results(env, e).message)

    except KeyboardInterrupt as e:
        return ExecutionResults.create_execution_results(env, e)
    except Exception as e:
        results = ExecutionResults.create_execution_results(env, e)
        _output_error(results.message)
        if results.fatal:
            import traceback
            traceback.print_exc()
        return results

    else:
        return ExecutionResults.create_execution_results(env)


def run_interpreter(source: str, filename: str, env: dict=None) -> ExecutionResults:
    """
    Run yapy interpreter
    :param source: yapy source contents
    :param filename: yapy source file name
    :param env: global and local environment. create an initial environment if env is None
    :return: execution results
    """
    results = compile_and_execute(source, filename, env)

    if not results.ok:
        import sys
        print(results.message, file=sys.stderr)

        if results.fatal:
            import traceback
            traceback.print_exc()

    return results


def compile_and_execute(source: str, filename: str, env: dict=None):
    """
    Compile and execute a yapy source
    :param source: yapy source contents
    :param filename: yapy source file name
    :param env: global and local environment. create an initial environment if env is None
    :return: execution results
    """
    if env is None:
        env = create_initial_environment()

    try:
        code_object = compile_yapy(source, filename, "exec", verbose=False)
        exec(code_object, env, env)
    except Exception as e:
        return ExecutionResults.create_execution_results(env, e)
    else:
        return ExecutionResults.create_execution_results(env)


def create_argparser() -> argparse.ArgumentParser:
    """Create and return a command line parameter parser."""
    parser = argparse.ArgumentParser(description="yapy: Yet Another Python language")
    parser.add_argument("-f", "--file", dest="file", metavar="FILE", type=str, help="yapy source file")
    parser.add_argument("-v", "--verbose", dest="verbose", default=False,
                        action='store_true', help="Show internal states")
    return parser


if __name__ == "__main__":
    parser = create_argparser()
    args = parser.parse_args()
    if args.file is None:
        # run REPL
        run_REPL(verbose=args.verbose)
    else:
        # compile and execute
        with open(args.file) as f:
            run_interpreter(f.read(), args.file)
