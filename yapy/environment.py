import builtins
import itertools
import functools


def create_initial_environment() -> dict:
    """Create and return an initial yapy environment"""
    return {"__builtins__": create_builtin_dict(),
            "itertools": itertools,
            "functools": functools}


def create_builtin_dict() -> dict:
    """Create and return a builtin yapy environment"""
    yapy_builtins = builtins.__dict__.copy()  # type: dict
    yapy_builtins.update({
        "reduce": functools.reduce,
    })
    return yapy_builtins
