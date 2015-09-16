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


def _assert(source, **args):
    _test('assert({0})'.format(source), **args)


def _assert_fail(source, **args):
    _fail('assert({0})'.format(source), **args)


def test_boolean():
    """Tests for boolean"""
    _assert("True")
    _assert_fail("(False)")

    _assert("!False")
    _assert_fail("(!True)")

    _assert("True = True")
    _assert("False = False")
    _assert_fail("True = False")

    _assert("True | True")
    _assert("True | False")
    _assert("False | True")
    _assert_fail("False | False")

    _assert("True & True")
    _assert_fail("True & False")
    _assert_fail("False & True")
    _assert_fail("False & False")


def test_numbers():
    """Tests for numbers"""
    _test("0")
    _test("1024")
    _test("3.14")
    _test("6.02 * 10 ** 23")
    _test("-256")
    _test("-3.14")

    _assert("1 = 1")
    _assert_fail("(1 = 0)")
    _assert("0 != 1")
    _assert("1 + 2 * 3 = 7")
    _assert_fail("(1 + 2 * 3 = 6)")
    _assert("100 + 20 + 3 = 200 - 77")
    _assert("25 = 100 / 4")

    _assert("1.real = 1")
    _assert("1.real.imag = 0")

def test_strings():
    """Tests for strings"""
    _assert('"hoge" = "hoge"')
    _assert('"hoge" != "fuga"')
    _assert('"" = ""')


def test_list():
    """Tests for lists"""
    _assert('len([]) = 0')
    _assert('["foo", "bar"] = ["foo", "bar"]')
    _assert('len([1, 2, 4]) = 3')
    _assert('len(["hoge"] * 5) = 5')


def test_functions():
    """Tests for functions"""
    # sum
    _test("""
    def sum(x: Int, y: Int): Int = x + y
    assert(sum(0, 0) = 0)
    assert(sum(4, 16) = 20)
    assert(sum(1024, -512) = 512)
    """)

    # factrial
    _test("""
    def fact(n: Int): Int = if n < 2 then 1 else n * fact(n - 1)
    assert(fact(0) = 1)
    assert(fact(1) = 1)
    assert(fact(10) = 3628800)
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

    assert(qsort([1]) = [1])
    assert(qsort([4, 2, 3, 1]) = [1, 2, 3, 4])""")