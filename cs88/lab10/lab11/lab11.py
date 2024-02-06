def naturals():
    """A generator function that yields the infinite sequence of natural
    numbers, starting at 1.

    >>> m = naturals()
    >>> type(m)
    <class 'generator'>
    >>> [next(m) for _ in range(10)]
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    """
    i = 1
    while True:
        yield i
        i += 1


#############
# Iterators #
#############

# Q2
class Str:
    "*** YOUR CODE HERE ***"
    def __init__(self, word):
        self.lst = list(word)
        self.index = 0
        

    def __next__(self):
        if self.index == len(self.lst):
            raise StopIteration
        message = self.lst[self.index]
        self.index += 1
        return message

    def __iter__(self):
        return self


##############
# Generators #
##############

# Q3
def countdown(n):
    """
    >>> from types import GeneratorType
    >>> type(countdown(0)) is GeneratorType # countdown is a generator
    True
    >>> for number in countdown(5):
    ...     print(number)
    ...
    5
    4
    3
    2
    1
    0
    """
    "*** YOUR CODE HERE ***"
    i = n
    while i >= 0:
        yield i
        i = i - 1

class Countdown:
    """
    >>> from types import GeneratorType
    >>> type(Countdown(0)) is GeneratorType # Countdown is an iterator
    False
    >>> for number in Countdown(5):
    ...     print(number)
    ...
    5
    4
    3
    2
    1
    0
    """
    "*** YOUR CODE HERE ***"
    def __init__(self,start):
        self.start = start
    
    def __next__(self):
        if self.start < 0:
            raise StopIteration
        self.start = self.start - 1
        return self.start + 1
    
    def __iter__(self):
        return self


# Q4
def hailstone(n):
    """
    >>> for num in hailstone(10):
    ...     print(num)
    ...
    10
    5
    16
    8
    4
    2
    1
    """
    "*** YOUR CODE HERE ***"
    i = n
    while i > 1:
        yield i
        if i % 2 == 0:
            i = i // 2
        else:
            i = i * 3 + 1
    yield i




# Optional, Extra Credit.
def lab11_extra_credit():
  """
  Fill in the values for these two variables.
  You will get the special code from the study tool when you complete all questions from lab.
  This code will be unique to your okpy email and this lab.
  Go here to practice: https://codestyle.herokuapp.com/cs88-lab11
  """
  okpy_email = "jack020945@berkeley.edu"
  practice_result_code = "f5aaf27f581d06af6071889b7d91821a"
  return (okpy_email, practice_result_code)
        
