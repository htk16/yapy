import argparse
import ast
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
    env = {}
    env["__builtins__"] = globals()["__builtins__"]
    return env


def run_REPL(verbose=False):
    """Execute yapy Read-Eval-Print-Loop"""
    def _output_error(msg):
        import sys
        print(msg, file=sys.stderr)

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
                _output_error("Syntax error: {0}".format(e))

    except yapy.macro.MacroExpantionError as e:
        _output_error("Macro expansion error: {0}".format(e))
    except yapy.translator.TranslationError as e:
        _output_error("Translation error: {0}".format(e))
    except Exception as e:
        _output_error("Internal error: {0}".format(e))


def compile_and_execute(source: str, filename: str):
    """
    Compile and execute a yapy source
    :param source: yapy source contents
    """
    def _output_error(msg):
        import sys
        print(msg, file=sys.stderr)

    try:
        code_object = compile_yapy(source, filename, "exec", verbose=False)
        env = create_initial_environment()
        exec(code_object, env, env)
    except pyparsing.ParseException as e:
        _output_error("Syntax error: {0}".format(e))
    except yapy.macro.MacroExpantionError as e:
        _output_error("Macro expansion error: {0}".format(e))
    except yapy.translator.TranslationError as e:
        _output_error("Translation error: {0}".format(e))
    except Exception as e:
        _output_error("Internal error: {0}".format(e))


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
            compile_and_execute(f.read(), args.file)
