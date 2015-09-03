import yapy.interpreter as interpreter
ExecutionStatus = interpreter.ExecutionStatus

def _test(source,
          expection_status=interpreter.ExecutionStatus.OK,
          expection_env: dict=None,
          sourcename: str="*TEST*",
          env: dict=None):
    results = interpreter.compile_and_execute(source, sourcename, env)
    assert results.status == expection_status

    if expection_env:
        for k, v in expection_env.items():
            assert k in results.env
            assert results.env[k] == v


def _fail(source, **args):
    _test(source, ExecutionStatus.ASSERTION_ERROR, **args)


def test_boolean():
    """Tests for boolean"""
    _test("require(True)")
    _fail("require(False)")

    _test("require(!False)")
    _fail("require(!True)")

    _test("require(True = True)")
    _test("require(False = False)")
    _fail("require(True = False)")

    _test("require(True | True)")
    _test("require(True | False)")
    _test("require(False | True)")
    _fail("require(False | False)")

    _test("require(True & True)")
    _fail("require(True & False)")
    _fail("require(False & True)")
    _fail("require(False & False)")


def test_numbers():
    """Tests for numbers"""
    _test("0")
    _test("1024")
    _test("3.14")
    _test("6.02 * 10 ** 23")
    _test("-256")
    _test("-3.14")

    _test("require(1 = 1)")
    _fail("require(1 = 0)")
    _test("require(0 != 1)")
    _test("require(1 + 2 * 3 = 7)")
    _fail("require(1 + 2 * 3 = 6)")
    _test("require(100 + 20 + 3 = 200 - 77)")
    _test("require(25 = 100 / 4)")


def test_strings():
    """Tests for strings"""
    _test('require("hoge" = "hoge")')
    _test('require("hoge" != "fuga")')