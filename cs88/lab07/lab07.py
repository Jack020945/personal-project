# Car

class Car(object):
    num_wheels = 4
    gas = 30
    headlights = 2
    size = 'Tiny'

    def __init__(self, make, model):
        self.make = make
        self.model = model
        self.color = 'No color yet. You need to paint me.'
        self.wheels = Car.num_wheels
        self.gas = Car.gas

    def paint(self, color):
        self.color = color
        return self.make + ' ' + self.model + ' is now ' + color

    def drive(self):
        if self.wheels < Car.num_wheels or self.gas <= 0:
            return 'Cannot drive!'
        self.gas -= 10
        return self.make + ' ' + self.model + ' goes vroom!'

    def pop_tire(self):
        if self.wheels > 0:
            self.wheels -= 1

    def fill_gas(self):
        self.gas += 20
        return 'Gas level: ' + str(self.gas)



# Keyboard

class Keyboard:
    """A Keyboard takes in a list of buttons, and has a
    dictionary of positions as keys, and Buttons as values.

    >>> b1 = Button("button1", "H")
    >>> b2 = Button("button2", "I")
    >>> k = Keyboard([b1, b2])
    >>> "button1" in k.buttons.keys() # Make sure to add the button to dictionary
    True
    >>> k.buttons["button1"].letter
    'H'
    >>> k.buttons["button1"].name
    'button1'
    >>> k.press("button1")
    'H'
    >>> k.press("button100")
    ''
    >>> b1.pressed
    1
    >>> b2.pressed
    0
    >>> k.typing(["button1", "button2"])
    'HI'
    >>> k.typing(["button2", "button1"])
    'IH'
    >>> b1.pressed # make sure typing calls press!
    3
    >>> b2.pressed
    2
    """

    def __init__(self, buttons):
        self.buttons = {buttons[x].name:buttons[x] for x in range(len(buttons))}



    def press(self, name):
        """Takes in a position of the button pressed, and
        returns that button's output. Return an empty string 
        if the button does not exist. You can access the keys 
        of a dictionary d with d.keys(). """
        if name not in self.buttons.keys():
            return ''
        else:
            pressed_button = self.buttons[name]
            pressed_button.pressed += 1
            return pressed_button.letter

    def typing(self, typing_input):
        """Takes in a list of buttons to be pressed, and
        returns the total output. Make sure to call self.press"""
        string = ''
        for n in typing_input:
            string += self.press(n)
        return string
            

class Button:
    def __init__(self, name, letter):
        self.name = name
        self.letter = letter
        self.pressed = 0


# T88ble

simple_table_rows = [['chocolate',2], ['vanilla', 1]]
simple_table_labels = ['Flavor', 'Price']
longer_table_rows = [[1990, 1, 1.5, 12, 7], [2000, 2, 2.5, 25, 10], [2010, 5, 4, 70, 36]]
longer_table_labels = ['Year', 'Bread price', 'Eggs price', 'Average tank of gas', 'Rent per day']

class T88ble():
    """
    T88ble is an object similar to the Data 8 Table object.
    Here the internal representation is a list of rows.
    """
    def __init__(self, rows=[], labels=[]):
        self.rows = rows
        self.column_labels = labels
    #DO NOT CHANGE THE __repr__ functions
    def __repr__(self):
        result = ""
        result += str(self.column_labels) + "\n"
        for row in self.rows:
            result += str(row) + "\n"
        return result
    def num_rows(self):
        """
        Compute the number of rows in a table.

        >>> simple_table = T88ble(simple_table_rows, simple_table_labels)
        >>> simple_table.num_rows()
        2
        >>> longer_table = T88ble(longer_table_rows, longer_table_labels)
        >>> longer_table.num_rows()
        3
        """
        return len(self.rows)
        

    def num_cols(self):
        """
        Compute the number of cols in a table.

        >>> simple_table = T88ble(simple_table_rows, simple_table_labels)
        >>> simple_table.num_cols()
        2
        >>> longer_table = T88ble(longer_table_rows, longer_table_labels)
        >>> longer_table.num_cols()
        5
        """
        return len(self.column_labels)
        

    def labels(self):
        """
        Lists the column labels in a table.

        >>> simple_table = T88ble(simple_table_rows, simple_table_labels)
        >>> simple_table.labels()
        ['Flavor', 'Price']
        >>> longer_table = T88ble(longer_table_rows, longer_table_labels)
        >>> labels = longer_table.labels() # Make sure to return and not print
        >>> labels
        ['Year', 'Bread price', 'Eggs price', 'Average tank of gas', 'Rent per day']
        """
        return self.column_labels
        

    def column(self, label):
        """
        Returns the values of the column represented by label.

        >>> simple_table = T88ble(simple_table_rows, simple_table_labels)
        >>> simple_table.column("Flavor")
        ['chocolate', 'vanilla']
        >>> longer_table = T88ble(longer_table_rows, longer_table_labels)
        >>> column = longer_table.column("Eggs price")
        >>> column # Make sure to return and not print
        [1.5, 2.5, 4]
        >>> longer_table # Make sure not to mutate the original table
        ['Year', 'Bread price', 'Eggs price', 'Average tank of gas', 'Rent per day']
        [1990, 1, 1.5, 12, 7]
        [2000, 2, 2.5, 25, 10]
        [2010, 5, 4, 70, 36]
        """
        lst = []
        index = self.column_labels.index(label) 
        for item in self.rows:
            lst.append(item[index])
        return lst   

    def with_column(self, label, values):
        """
        Returns a new table with an additional or replaced column.
        label is a string for the name of a column, values is an list

        >>> longer_table = T88ble(longer_table_rows, longer_table_labels)
        >>> new_table = longer_table.with_column('Inflation rate', [i for i in range(longer_table.num_rows())])
        >>> new_table
        ['Year', 'Bread price', 'Eggs price', 'Average tank of gas', 'Rent per day', 'Inflation rate']
        [1990, 1, 1.5, 12, 7, 0]
        [2000, 2, 2.5, 25, 10, 1]
        [2010, 5, 4, 70, 36, 2]
        >>> longer_table # Make sure not to mutate the original table
        ['Year', 'Bread price', 'Eggs price', 'Average tank of gas', 'Rent per day']
        [1990, 1, 1.5, 12, 7]
        [2000, 2, 2.5, 25, 10]
        [2010, 5, 4, 70, 36]
        """
        new_rows = []
        for row in self.rows:
            new_rows.append(row.copy())
        new_column_labels = self.column_labels.copy()
        if (label not in new_column_labels):
            new_column_labels.append(label)
            x = 0
            for item in new_rows:
                item.append(values[x])
                x = x + 1
        else:
            index = new_column_labels.index(label)
            x = 0
            for item in new_rows:
                new_rows[index] = values[x]
                x = x + 1

        result = T88ble(new_rows, new_column_labels)
        return  result

        
        


        

    def select(self, labels):
        """
        Create a copy of a table with only some of the columns,
        reffered to by the list of labels.

        >>> simple_table = T88ble(simple_table_rows, simple_table_labels)
        >>> simple_table.select(["Flavor"])
        ['Flavor']
        ['chocolate']
        ['vanilla']
        >>> simple_table
        ['Flavor', 'Price']
        ['chocolate', 2]
        ['vanilla', 1]
        >>> longer_table = T88ble(longer_table_rows, longer_table_labels)
        >>> selected = longer_table.select(['Year', 'Average tank of gas'])
        >>> selected # Make sure to return and not print
        ['Year', 'Average tank of gas']
        [1990, 12]
        [2000, 25]
        [2010, 70]
        >>> longer_table
        ['Year', 'Bread price', 'Eggs price', 'Average tank of gas', 'Rent per day']
        [1990, 1, 1.5, 12, 7]
        [2000, 2, 2.5, 25, 10]
        [2010, 5, 4, 70, 36]
        """
        
        lst_index = []
        for label in labels:
            index = self.column_labels.index(label)
            lst_index.append(index)
        lst = []
        for row in self.rows:
            temp = []
            for index in lst_index:
                temp.append(row[index])
            lst.append(temp)

        return T88ble(lst, labels)

            
    
            

        

        

        


    def sort(self, label, descending=True):
        """
        Create a copy of a table sorted by the values in a column.
        Defaults to descending = True. Change to descending = False to make it ascending.

        >>> longer_table = T88ble(longer_table_rows, longer_table_labels)
        >>> longer_table.sort("Year")
        ['Year', 'Bread price', 'Eggs price', 'Average tank of gas', 'Rent per day']
        [2010, 5, 4, 70, 36]
        [2000, 2, 2.5, 25, 10]
        [1990, 1, 1.5, 12, 7]
        >>> longer_table
        ['Year', 'Bread price', 'Eggs price', 'Average tank of gas', 'Rent per day']
        [1990, 1, 1.5, 12, 7]
        [2000, 2, 2.5, 25, 10]
        [2010, 5, 4, 70, 36]
        >>> simple_table = T88ble(simple_table_rows, simple_table_labels)
        >>> sorted = simple_table.sort("Price", descending=False)
        >>> sorted
        ['Flavor', 'Price']
        ['vanilla', 1]
        ['chocolate', 2]
        >>> simple_table
        ['Flavor', 'Price']
        ['chocolate', 2]
        ['vanilla', 1]

        """
        index = self.column_labels.index(label)
        lst = []
        for elem in self.rows:
            lst.append(elem[index])
        sorted(lst,reverse = descending)
        lst.reverse()
        new_rows = []
        for i in range(len(self.rows)):
            for row in self.rows:
                if row[index] == lst[i]:
                    new_rows.append(row)
 
        return T88ble(new_rows, self.column_labels)
        


    def where(self, label, filter_fn):
        """
        Create a copy of a table with only the rows that match a filter function.

        >>> def above(x):
        ...     return lambda y: y > x
        ...
        >>> longer_table = T88ble(longer_table_rows, longer_table_labels)
        >>> filtered = longer_table.where('Eggs price', above(2))
        >>> filtered
        ['Year', 'Bread price', 'Eggs price', 'Average tank of gas', 'Rent per day']
        [2000, 2, 2.5, 25, 10]
        [2010, 5, 4, 70, 36]
        >>> longer_table
        ['Year', 'Bread price', 'Eggs price', 'Average tank of gas', 'Rent per day']
        [1990, 1, 1.5, 12, 7]
        [2000, 2, 2.5, 25, 10]
        [2010, 5, 4, 70, 36]
        """
        index = self.column_labels.index(label)
        lst = []
        for row in self.rows:
            if filter_fn(row[index]):
                lst.append(row)
        return T88ble(lst, self.column_labels)

        



# Optional, Extra Credit.
def lab07_extra_credit():
    """
    Fill in the values for these two variables.
    You will get the special code from the study tool when you complete all questions from lab.
    This code will be unique to your okpy email and this lab.
    Go here to practice: https://codestyle.herokuapp.com/cs88-lab07
    """
    okpy_email = "jack020945@berkeley.edu"
    practice_result_code = "7cc148816e9cc3795cc5e1666ca53c16"
    return (okpy_email, practice_result_code)

# Optional, Extra Credit.
def lab2_extra_credit():
    """
    Fill in the values for these two variables.
    You will get the special code from the study tool when you complete all questions from lab.
    This code will be unique to your okpy email and this lab.
    Go here to practice: https://codestyle.herokuapp.com/cs88-lab02
    DO NOT DO THIS IF YOU WERE NOT A LATE ADD FROM CS61A - you risk loosing all extra credit.
    """
    okpy_email = "jack020945@berkeley.edu"
    practice_result_code = "e5b3705c5120f7a5f77eb663f2500ffe"
    return (okpy_email, practice_result_code)

        
