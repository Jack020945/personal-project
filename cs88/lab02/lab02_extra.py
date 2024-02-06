# Optional lab02 questions.

def square(x):
    return x * x

def twice(f,x):
    """Apply f to the result of applying f to x"
    >>> twice(square,3)
    81
    """
    "*** YOUR CODE HERE ***"

def increment(x):
    return x + 1

def apply_n(f, x, n):
    """Apply function f to x n times.

    >>> apply_n(increment, 2, 10)
    12
    """
    "*** YOUR CODE HERE ***"


def zero(f):
    def _zero(x):
        return x

    return _zero

def successor(n):
    def _succ(f):
        def t(x):
            return f(n(f)(x))
        
        return t
    return _succ

def one(f):
    """Church numeral 1: same as successor(zero)"""
    "*** YOUR CODE HERE ***"

def two(f):
    """Church numeral 2: same as successor(successor(zero))"""
    "*** YOUR CODE HERE ***"
three = successor(two)

def church_to_int(n):
    """Convert the Church numeral n to a Python integer.

    >>> church_to_int(zero)
    0
    >>> church_to_int(one)
    1
    >>> church_to_int(two)
    2
    >>> church_to_int(three)
    3
    """
    "*** YOUR CODE HERE ***"

def add_church(m, n):
    """Return the Church numeral for m + n, for Church numerals m and n.

    >>> church_to_int(add_church(two, three))
    5
    """
    "*** YOUR CODE HERE ***"

def mul_church(m, n):
    """Return the Church numeral for m * n, for Church numerals m and n.

    >>> four = successor(three)
    >>> church_to_int(mul_church(two, three))
    6
    >>> church_to_int(mul_church(three, four))
    12
    """
    "*** YOUR CODE HERE ***"

def pow_church(m, n):
    """Return the Church numeral m ** n, for Church numerals m and n.

    >>> church_to_int(pow_church(two, three))
    8
    >>> church_to_int(pow_church(three, two))
    9
    """
    "*** YOUR CODE HERE ***"

