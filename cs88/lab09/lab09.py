## Trees ##

# Tree Class
class Tree:
    def __init__(self, entry, branches=()):
        self.entry = entry
        for branch in branches:
            assert isinstance(branch, Tree)
        self.branches = list(branches)

    def __repr__(self):
        if self.branches:
            branches_str = ', ' + repr(self.branches)
        else:
            branches_str = ''
        return 'Tree({0}{1})'.format(self.entry, branches_str)

    def __str__(self):
        def print_tree(t, indent=0):
            tree_str = '  ' * indent + str(t.entry) + "\n"
            for b in t.branches:
                tree_str += print_tree(b, indent + 1)
            return tree_str
        return print_tree(self).rstrip()

    def is_leaf(self):
        return not self.branches



# Q1
def same_shape(t1, t2):
    """Returns whether two Trees t1, t2 have the same shape. Two trees have the
    same shape if they have the same number of branches and each of their
    children have the same shape.

    >>> t, s = Tree(1), Tree(3)
    >>> same_shape(t, t)
    True
    >>> same_shape(t, s)
    True
    >>> t = Tree(1, [Tree(2), Tree(3)])
    >>> same_shape(t, s)
    False
    >>> s = Tree(4, [Tree(7)])
    >>> same_shape(t, s)
    False
    >>> s.branches.append(Tree(6)) # Add a new leaf to s to make it same shape as t
    >>> same_shape(t, s)
    True
    """
    if t1.is_leaf() and t2.is_leaf:
        return True
    if len(t1.branches) != len(t2.branches):
        return False
    else:
        for i in range(len(t1.branches)):
            if not same_shape(t1.branches[i], t2.branches[i]):
                return False
        return True



# Q2
def cumulative_sum(t):
    """Return a new Tree, where each entry is the sum of all entries in the
    corresponding subtree of t.

    >>> t = Tree(1, [Tree(3, [Tree(5)]), Tree(7)])
    >>> cumulative = cumulative_sum(t)
    >>> t
    Tree(1, [Tree(3, [Tree(5)]), Tree(7)])
    >>> cumulative
    Tree(16, [Tree(8, [Tree(5)]), Tree(7)])
    >>> cumulative_sum(Tree(1))
    Tree(1)
    """
    if t.is_leaf():
        return Tree(t.entry)
    new_branch = []
    for branch in t.branches:
        new_branch.append(cumulative_sum(branch))
    
    new_entry = t.entry
    for branch in new_branch:
        new_entry += branch.entry
    return Tree(new_entry, new_branch)


# Q3
def leaves(t):
    """Returns a list of all the entries of the leaf nodes of the Tree t.

    >>> leaves(Tree(1))
    [1]
    >>> leaves(Tree(1, [Tree(2, [Tree(3)]), Tree(4)]))
    [3, 4]
    """
    lst = []
    if t.branches == []:
        lst.append(t.entry)
    for branch in t.branches:
        if branch.is_leaf():
            lst.append(branch.entry)
        else:
            lst += leaves(branch)
    return lst




# Q4
def square_tree(t):
    """Mutates a Tree t by squaring all its elements.

    >>> t = Tree(1, [Tree(3, [Tree(5)]), Tree(7)])
    >>> square_tree(t)
    >>> t
    Tree(1, [Tree(9, [Tree(25)]), Tree(49)])
    """
    if t.is_leaf():
        t.entry = t.entry * t.entry
    else:
        t.entry = t.entry * t.entry
        for branch in t.branches:
            square_tree(branch)



# Optional, Extra Credit.
def lab09_extra_credit():
  """
  Fill in the values for these two variables.
  You will get the special code from the study tool when you complete all questions from lab.
  This code will be unique to your okpy email and this lab.
  Go here to practice: https://codestyle.herokuapp.com/cs88-lab04
  """
  okpy_email = "jack020945@berkeley.edu"
  practice_result_code = "ef8013c629bd2dfc656a453abc8a6e4b"
  return (okpy_email, practice_result_code)
