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


def _require(source, **args):
    _test('require({0})'.format(source), **args)


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
    _require('"hoge" = "hoge"')
    _require('"hoge" != "fuga"')
    _require('"" = ""')


def test_list():
    """Tests for lists"""
    _require('len([]) = 0')
    _require('["foo", "bar"] = ["foo", "bar"]')
    _require('len([1, 2, 4]) = 3')
    _require('len(["hoge"] * 5) = 5')


def test_functions():
    """Tests for functions"""
    # sum
    _test("""
    def sum(x: Int, y: Int): Int = x + y
    require(sum(0, 0) = 0)
    require(sum(4, 16) = 20)
    require(sum(1024, -512) = 512)
    """)

    # factrial
    _test("""
    def fact(n: Int): Int = if n < 2 then 1 else n * fact(n - 1)
    require(fact(0) = 1)
    require(fact(1) = 1)
    require(fact(10) = 3628800)
    """)

    # quick sort
    _test("""
    def qsort(xs: List[Int]): List[Int] = {
        if len(xs) <= 1
        then xs
        else {
            let mid: Int = xs[0]
            qsort(list(filter(fn(x) = x < mid, xs))) + [mid] + qsort(list(filter(fn(x) = x > mid, xs)))
        }
    }

    require(qsort([1]) = [1])
    require(qsort([4, 2, 3, 1]) = [1, 2, 3, 4])""")