# Probably a die-re situation

def flip_dict(dictionary):
    """Returns a flipped version of the original dictionary.

    >>> TAs = {"12pm-2pm": "brian", "10am-12pm": "sophia", "2pm-4pm": "alec"}
    >>> flipped_TAs = flip_dict(TAs)
    >>> sorted_keys = sorted(flipped_TAs)
    >>> sorted_keys
    ['alec', 'brian', 'sophia']
    >>> [flipped_TAs[i] for i in sorted_keys]
    ['2pm-4pm', '12pm-2pm', '10am-12pm']
    """
    return {word:time for time, word in dictionary.items()}
    


def merge_dict(d1, d2):
    """Returns a dictionary with two dictionaries merged together. You can assume that the same keys appear in both dictionaries. 

    >>> data8 = {"midterms":1, "projects":3}
    >>> data100 = {"midterms":2, "projects":3}
    >>> combined = merge_dict(data8, data100)
    >>> combined
    {'midterms': 3, 'projects': 6}
    """
    new_dict = {}
    for key in d1.keys():
        new_dict[key] = d1[key]
    for key in d2.keys():
        if key in new_dict.keys():
            new_dict[key] = new_dict[key] + d2[key]
        else:
            new_dict[key] = d2[key]
    return new_dict

    # new_dict = {}
    # for key in d1.keys():

    #     new_dict[key] = d1[key] + d2[key]
        
    # return new_dict


    

import random
random.seed(42)

def dice(a, b):
    """Construct a die that is a list from a to b inclusive.
    >>> dice(1, 6)
    [1, 2, 3, 4, 5, 6]
    >>> dice(3, 5)
    [3, 4, 5]
    >>> dice(5, 5)
    [5]
    """
    result = list(range(a,b + 1))
    return result

def smallest(die):
    """Return the lowest value die can take on."""
    return min(die)

def largest(die):
    """Return the largest value die can take on."""
    return max(die)

def str_dice(die):
    """Return a string representation of die.

    >>> str_dice(dice(1, 6))
    'die takes on values from 1 to 6'
    """
    return 'die takes on values from {0} to {1}'.format(smallest(die), largest(die))

def roll_dice(die, x):
    """Roll the die x times and return an array of the rolled values.
    >>> roll_dice(dice(5, 5), 4)
    [5, 5, 5, 5]
    >>> max(roll_dice(dice(1, 6), 100))
    6
    >>> min(roll_dice(dice(1, 6), 100))
    1
    >>> x = sum(roll_dice(dice(1, 6), 100))/100 # Finds the mean of 100 dice rolls
    >>> 3 <= x <= 4 # Checks if the mean is between 3 and 4
    True
   """
    lst = []
    for rolls in range(x):
        result = random.choice(die)
        lst.append(result)
    return lst

def rolls_until_six(die):
    """Roll the die until you get a 6 and return the number of rolls it took to do so. 
    If six is not a the possible values to roll, return a string saying '6 is not a possible value of this die'
    >>> rolls_until_six(dice(1, 5))
    '6 is not a possible value of this die'
    >>> rolls_until_six(dice(6, 6)) # Takes one roll to get 6
    1
    >>> x = sum([rolls_until_six(dice(1, 6)) for _ in range(100)])/100 # Repeat 100 times and average
    >>> 5 <= x <= 7 # Check that it takes between 5 and 7 rolls overall on average
    True
    """
    count = 0
    s = smallest(die)
    l = largest(die)
    for numer_rolls in range(s, l + 1):
        if numer_rolls != 6:
            count = count + 1
        else:
            count = count + 1
            return count
    return '6 is not a possible value of this die'

def cup(die1, die2):
    """Construct a cup that contains die1 and die2.
    >>> cup(dice(1, 1), dice(1, 2))
    [[1], [1, 2]]
    """
    lst1 = []
    lst2 = []
    lst = []
    s1 = smallest(die1)
    l1= largest(die1)
    lst1.append(s1)
    lst1.append(l1)
    if lst1[0] == lst1[1]:
        lst1.pop(1) 
    s2 = smallest(die2)
    l2= largest(die2)
    lst2.append(s2)
    lst2.append(l2)
    if lst2[0] == lst2[1]:
        lst2.pop(1)
    lst.append(lst1)
    lst.append(lst2)
    return lst




def add_to_cup(cup, die):
    """Add die to cup.
    >>> cup1 = cup(dice(1, 1), dice(1, 2))
    >>> add_to_cup(cup1, dice(1, 3))
    [[1], [1, 2], [1, 2, 3]]
    """
    lst = []
    lst.append(cup[0])
    lst.append(cup[1])
    lst.append(die)
    return lst

def roll_cup(cup):
    """Roll every die in the cup and return an array of the rolled values.
    >>> roll_cup(cup(dice(1, 1), dice(2, 2)))
    [1, 2]
    """
    lst = []
    for num in cup[0]:
        lst.append(num)
    for num in cup[1]:
        lst.append(num)
    return lst
    
