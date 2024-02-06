def naturals():
    i = 1
    while True:
        yield i
        i += 1

class ScaleIterator:
    """An iterator the scales elements of the iterable s by a number k.

    >>> s = ScaleIterator([1, 5, 2], 5)
    >>> list(s)
    [5, 25, 10]

    >>> m = ScaleIterator(naturals(), 2)
    >>> [next(m) for _ in range(5)]
    [2, 4, 6, 8, 10]
    """
    def __init__(self, s, k):
        "*** YOUR CODE HERE ***"
        self.s = s
        self.k = k
        self.index = 0

    def __iter__(self):
        return self

    def __next__(self):
        "*** YOUR CODE HERE ***"
        if not isinstance(self.s, list):
            while True:
                return next(self.s) * self.k
        else:
            if self.index == len(self.s):
                raise StopIteration
            num = self.s[self.index] * self.k
            self.index = self.index + 1
            return num

        

        




class IteratorRestart:
    """
    >>> iterator = IteratorRestart(2, 7)
    >>> for num in iterator:
    ...     print(num)
    2
    3
    4
    5
    6
    7
    >>> for num in iterator:
    ...     print(num)
    2
    3
    4
    5
    6
    7
    """
    def __init__(self, start, end):
        self.start = start
        self.end = end
        self.curr = start

    def __next__(self):
        "*** YOUR CODE HERE ***"
        if self.curr > self.end:
            raise StopIteration
        self.curr = 1 + self.curr
        return self.curr - 1


    def __iter__(self):
        "*** YOUR CODE HERE ***"
        self.curr = self.start
        return self


def scale(s, k):
    """Yield elements of the iterable s scaled by a number k.

    >>> s = scale([1, 5, 2], 5)
    >>> type(s)
    <class 'generator'>
    >>> list(s)
    [5, 25, 10]

    >>> m = scale(naturals(), 2)
    >>> [next(m) for _ in range(5)]
    [2, 4, 6, 8, 10]
    """
    "*** YOUR CODE HERE ***"
    for num in s:
        yield num*k


def remainders_generator(m):
    """
    Takes in an integer m, and yields m different remainder groups
    of m.

    >>> remainders_mod_four = remainders_generator(4)
    >>> for rem_group in remainders_mod_four:
    ...     for _ in range(3):
    ...         print(next(rem_group))
    0
    4
    8
    1
    5
    9
    2
    6
    10
    3
    7
    11
    """
    "*** YOUR CODE HERE ***"
    def inner_gen(x):
        result =  -4+x
        while True:
            result = m + result
            yield result
    for x in range(0,m):
        yield inner_gen(x)




