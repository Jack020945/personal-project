HW_SOURCE_FILE=__file__


def pascal(row, column):
    """Returns a number corresponding to the value at that location
    in Pascal's Triangle.
    >>> pascal(0, 0)
    1
    >>> pascal(0, 5)	# Empty entry; outside of Pascal's Triangle
    0
    >>> pascal(3, 2)	# Row 4 (1 3 3 1), 3rd entry
    3
    """
    "*** YOUR CODE HERE ***"
    if column == 0 or row == column:
        return 1
    if column > row:
        return 0

    return pascal(row - 1, column - 1) + pascal(row - 1, column)


def compose1(f, g):
    """"Return a function h, such that h(x) = f(g(x))."""
    def h(x):
        return f(g(x))
    return h

def repeated(f, n):
    """Return the function that computes the nth application of func (recursively!).

    >>> add_three = repeated(lambda x: x + 1, 3)
    >>> add_three(5)
    8
    >>> square = lambda x: x ** 2
    >>> repeated(square, 2)(5) # square(square(5))
    625
    >>> repeated(square, 4)(5) # square(square(square(square(5))))
    152587890625
    >>> repeated(square, 0)(5)
    5
    >>> from construct_check import check
    >>> # ban iteration
    >>> check(HW_SOURCE_FILE, 'repeated',
    ...       ['For', 'While'])
    True
    """
    "*** YOUR CODE HERE ***"
    if n == 0:
        return lambda x: x
    else:
        return compose1(f, repeated(f, n - 1))



def num_eights(x):
    """Returns the number of times 8 appears as a digit of x.

    >>> num_eights(3)
    0
    >>> num_eights(8)
    1
    >>> num_eights(88888888)
    8
    >>> num_eights(2638)
    1
    >>> num_eights(86380)
    2
    >>> num_eights(12345)
    0
    >>> from construct_check import check
    >>> # ban all assignment statements
    >>> check(HW_SOURCE_FILE, 'num_eights',
    ...       ['Assign', 'AugAssign'])
    True
    """
    "*** YOUR CODE HERE ***"
    if x == 0:
        return 0 
    if x % 10 == 8:
        return 1 + num_eights(x // 10)
    else:
        return num_eights(x // 10)


def pingpong(n):
    """Return the nth element of the ping-pong sequence.

    >>> pingpong(8)
    8
    >>> pingpong(10)
    6
    >>> pingpong(15)
    1
    >>> pingpong(21)
    -1
    >>> pingpong(22)
    -2
    >>> pingpong(30)
    -2
    >>> pingpong(68)
    0
    >>> pingpong(69)
    -1
    >>> pingpong(80)
    0
    >>> pingpong(81)
    1
    >>> pingpong(82)
    0
    >>> pingpong(100)
    -6
    >>> from construct_check import check
    >>> # ban assignment statements
    >>> check(HW_SOURCE_FILE, 'pingpong', ['Assign', 'AugAssign'])
    True
    """
    "*** YOUR CODE HERE ***"

    '''def check8(n): 
        if n % 8 == 8:
            return True
        else:
            return check8(n // 10)

    def g(n):
        if n == 1:
            return 1
        if n // 8 != 0 or check8 == True:
            return g(n-1) + 1 + 
        
        else:
            return g(n-1) + 1

    def f(n):
        if n // 8 != 0 or check8 == True:
            return g'''

    def is_turning_point(x):
        if num_eights(x) > 0 or x % 8 ==0:
            return True

        else:
            return False
    

    def pingpong_tracker(index, i, sign = 1):

        if index == n:
            return i

        if is_turning_point(index):
            return pingpong_tracker(index + 1, i - sign, -1 * sign)

        return pingpong_tracker(index + 1, i + sign, sign)

    return pingpong_tracker(1,1)
    

    

