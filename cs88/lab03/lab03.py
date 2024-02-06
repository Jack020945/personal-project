# Question 1-4

def where_above(lst, limit):
    """
    where_above behaves like table.where(column, are.above(limit)).
    The analogy is completed if you think of a column of a table as a list and return the filtered column instead of the entire table.

    >>> where_above([1, 2, 3], 2)
    [3]
    >>> where_above(range(13), 10)
    [11, 12]
    >>> where_above(range(123), 120)
    [121, 122]

    """
    
    result = []
    for i in range(len(lst)):
        value = lst[i]
        if value > limit:
            result.append(value)
    return result

            

    
    
    


def minmax(s):
    """Return the minimum and maximum elements of a non-empty list. Hint: start 
    with defining two variables at the beginning. Do not use the built in 
    max or min functions

    >>> minmax([1, 2, -3])
    [-3, 2]
    >>> x = minmax([2])
    >>> x
    [2, 2]
    >>> minmax([4, 5, 4, 5, 1, 9, 0, 7])
    [0, 9]
    >>> minmax([100, -10, 1, 0, 10, -100])
    [-100, 100]
    """
    minimum = maximum = s[0]
    
    for x in s:
        if x > maximum:
            maximum = x
        if x < minimum:
            minimum = x
    return [minimum,maximum]



        
        
    

    


def common_member(lst1, lst2):
    """
    Returns true if there are any common members between
    lst1 and lst2.
    >>> common_member([5, 3, 2, 1], [1, 9, 3, 4, 5])
    True
    >>> common_member([17, 18, 24], [23, 21, 22, 27, 29, 5])
    False
    >>> common_member([5, 7], [7, 3])
    True
    """
    
    x = [value for value in lst1 if value in lst2]
    if len(x)!= 0:
        
        return True
    else:
        return False




def closest_power_2(x):
    """ Returns the closest power of 2 that is less than x
    >>> closest_power_2(6)
    4
    >>> closest_power_2(32)
    16
    >>> closest_power_2(87)
    64
    >>> closest_power_2(4095)
    2048
    >>> closest_power_2(524290)
    524288
    """
    import math
    while x >= 0:
        x = x - 1
        temp = math.log(x, 2)
        if temp == int(temp):
            return int(x)
    


# Optional, Extra Credit.
def lab03_extra_credit():
  """
  Fill in the values for these two variables.
  You will get the special code from the study tool when you complete all quesitons from lab.
  This code will be unique to your okpy email and this lab.
  Go here to practice: https://codestyle.herokuapp.com/cs88-lab03
  """
  okpy_email = "jack020945@berkeley.edu"
  practice_result_code = "60a8e08ffc91394162cc95d858eac2ff"
  return (okpy_email, practice_result_code)

def lab02_extra_credit():
  """
  Fill in the values for these two variables.
  You will get the special code from the study tool when you complete all quesitons from lab.
  This code will be unique to your okpy email and this lab.
  Go here to practice: https://codestyle.herokuapp.com/cs88-lab03
  """
  okpy_email = "jack020945@berkeley.edu"
  practice_result_code = "e5b3705c5120f7a5f77eb663f2500ffe"
  return (okpy_email, practice_result_code)
