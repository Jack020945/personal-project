# Account

class Account(object):
    """A bank account that allows deposits and withdrawals.

    >>> sophia_account = Account('Sophia')
    >>> sophia_account.deposit(1000000)   # depositing my paycheck for the week
    1000000
    >>> sophia_account.transactions
    [('deposit', 1000000)]
    >>> sophia_account.withdraw(100)      # buying dinner
    999900
    >>> sophia_account.transactions
    [('deposit', 1000000), ('withdraw', 100)]
    """

    interest = 0.02
    balance = 1000

    def __init__(self, account_holder):
        self.balance = 0
        self.holder = account_holder
        self.transactions = []

    def deposit(self, amount):
        """Increase the account balance by amount and return the
        new balance.
        """
        self.transactions.append(('deposit', amount))
        self.balance = self.balance + amount
        return self.balance

    def withdraw(self, amount):
        """Decrease the account balance by amount and return the
        new balance.
        """
        self.transactions.append(('withdraw', amount))
        if amount > self.balance:
            return 'Insufficient funds'
        self.balance = self.balance - amount
        return self.balance


# Errors


class Error:
    """
    >>> err1 = Error(12, "error.py")
    >>> err1.write()
    'error.py:12'

    """
    def __init__(self, line, file):
        "*** YOUR CODE HERE ***"
        self.line = line
        self.file =file

    def write(self):
        return self.file + ':' + str(self.line)

class SyntaxError(Error):
    """
    >>> err1 = SyntaxError(17, "lab10.py")
    >>> err1.write()
    lab10.py:17 SyntaxError : Invalid syntax
    >>> err1.add_code(4, "EOL while scanning string literal")
    >>> err2 = SyntaxError(18, "lab10.py", 4)
    >>> err2.write()
    lab10.py:18 SyntaxError : EOL while scanning string literal

    """
    type = 'SyntaxError'
    msgs = {0 : "Invalid syntax", 1: "Unmatched parentheses", 2: "Incorrect indentation", 3: "missing colon"}

    def __init__(self, line, file, code=0):
        "*** YOUR CODE HERE ***"
        self.line = line
        self.file = file
        self.message = self.msgs[code]

    def write(self):
        end = self.type + ' : ' + self.message
        "*** YOUR CODE HERE ***"
        print (str(self.file) + ':' + str(self.line) + " " + end)

    def add_code(self, code, msg):
        "*** YOUR CODE HERE "
        self.code = code
        self.msgs[self.code] = msg
        self.message = self.msgs[self.code]


class ZeroDivisionError(Error):
    """
    >>> err1 = ZeroDivisionError(273, "lab10.py")
    >>> err1.write()
    lab10.py:273 ZeroDivisionError : division by zero
    """
    type = 'ZeroDivisionError'

    def __init__(self, line, file, message='division by zero'):
        "*** YOUR CODE HERE ***"
        self.line = line
        self.file = file
        self.message = message

    def write(self):
        end = self.type + ' : ' + self.message
        print (str(self.file) + ":" + str(self.line) +' ' + end )


# Quidditch

class QuidditchPlayer:
    def __init__(self, name, base_energy):
        """
        QuidditchPlayers have a name, and begin with base_energy.
        """
        self.name = name
        self.base_energy = base_energy

    def energy(self):
        return self.base_energy

class Beater(QuidditchPlayer):
    role = "bludgers"

    def energy(self, time):
        """
        Returns the amount of energy left after playing for time minutes. 
        After playing for time minutes, Beaters lose their base energy level 
        divided by the number of minutes. If time is 0, catch the ZeroDivisionError 
        and print "You can't divide by zero!" instead.
        >>> fred = Beater("Fred Weasley", 640)
        >>> fred.energy(40)
        624.0
        >>> fred.energy(0)
        You can't divide by zero!
        """
        "*** YOUR CODE HERE ***"
        
        if time == 0:
            print("You can't divide by zero!")
        else:
            divided = self.base_energy / time
            energy_left = self.base_energy - divided
            return energy_left
        

class Chaser(QuidditchPlayer):
    role = "score"
    energy_expended = 20
    
    def __init__(self, name, base_energy, goals):
        """
        Chasers have a name, score goals, and begin with base_energy.
        """
        "*** YOUR CODE HERE ***"
        self.name = name
        self.base_energy = base_energy
        self.goals = goals

        

    def energy(self, time):
        """
        Returns the amount of energy left after playing for time minutes. For every goal 
        they score, they use energy_expended units of energy. In addition, they also use 
        10% of energy_expended if the number of minutes they have played is a multiple of 9.
        >>> katie = Chaser("Katie Bell", 230, 2)
        >>> katie.energy(20)
        190
        >>> ginny = Chaser("Ginny Weasley", 400, 3)
        >>> ginny.energy(45)
        338.0
        """
        "*** YOUR CODE HERE ***"
        if time % 3 == 0:
            energy_left = (self.base_energy - self.energy_expended * self.goals) - 0.1 * self.energy_expended
            return energy_left
        else:
            energy_left = (self.base_energy - self.energy_expended * self.goals)
            return energy_left


        

class Seeker(QuidditchPlayer):
    role = "snitch"
    energy_expended = 5

    def energy(self, time):
        """
        Returns the amount of energy after time minutes. Seekers expend energy_expended 
        units of their energy for every minute they have been playing.
        >>> harry = Seeker("Harry Potter", 700)
        >>> harry.energy(30)
        550
        """
        "*** YOUR CODE HERE ***"
        energy_left = self.base_energy - time * self.energy_expended
        return energy_left
        

class Keeper(QuidditchPlayer):
    role = "guard"
    energy_expended = 50

    def energy(self, time):
        """
        Returns the amount of energy after time minutes. If less than 30 minutes have 
        passed, then Keepers do not lose any energy. If 30 minutes or more have passed, 
        then Keepers expend 80% of their energy_expended units for every full 15 
        minutes that pass.
        >>> oliver = Keeper("Oliver Wood", 380)
        >>> oliver.energy(45)
        260.0
        """
        "*** YOUR CODE HERE ***"
        if time > 30:
            time = time / 15
            energy_left = self.base_energy - 0.8 * self.energy_expended * time
            return energy_left
        else:
            return self.base_energy
        



# Werewolf

def get_most_common_element(lst):
    return max(set(lst), key=lst.count)

class Pl88yer:
    def __init__(self, name):
        self.name = name
        self.active = True

class Werewolf(Pl88yer):
    def __init__(self, name):
        Pl88yer.__init__(self, name)

    def reveal_player_type(self):
        print("You are a werewolf!")

class Villager(Pl88yer):
    def __init__(self, name):
        Pl88yer.__init__(self, name)    

    def reveal_player_type(self):
        print("You are a villager!")

class WerewolfGame:
    def __init__(self, players, your_name):
        """
        Sets the game up. players is a list of strings that are names of all 
        of the players. your_name is a string and must be one of the players.
        >>> game = WerewolfGame(["a", "b", "c", "d", "e", "f"], "a")
        You are a werewolf!
        >>> game.your_name
        'a'
        >>> game.play("b")
        'Keep playing!'
        >>> len(game.werewolves)
        1
        >>> len(game.villagers)
        4
        >>> game.play("c")
        'Keep playing!'
        >>> game.play("d")
        'Keep playing!'
        >>> game.play("a")
        'Villagers win!'
        >>> game.werewolves
        []
        >>> len(game.villagers)
        2
        """
        if len(players) < 4:
            raise Exception("Not enough players!")
        names = players[0:2]
        self.your_name = your_name
        self.werewolves = [Werewolf(w) for w in names]
        self.villagers = [Villager(p) for p in players if p not in names]
        self.name_to_player = {}

        for werewolf in self.werewolves:
            self.name_to_player[werewolf.name] = werewolf

        for villager in self.villagers:
            self.name_to_player[villager.name] = villager

        player = self.name_to_player[your_name]
        player.reveal_player_type()

        self.state = "night"

    def play(self, vote):
        """
        While the game is still being played, make a move. vote is the player 
        who you vote for, because you believe they are on the opposing team. 
        You can continue playing until either the villagers or the werewolves win.
        """
        self.make_move(vote)
        if not self.check_if_end_of_game():
            return "Keep playing!"
        else:
            if len(self.werewolves) == 0:
                return "Villagers win!"
            elif len(self.werewolves) > len(self.villagers):
                return "Werewolves win!"

    def make_move(self, vote):
        """
        Every player votes (players arbitrarily vote for themselves). Then, 
        if the state of the game is day, remove the player with the most votes 
        overall, and set the state to night. If the state of the game is night, 
        remove the player with the most votes by werewolves, and set the state to day.
        """
        votes = []
        werewolf_votes = []

        if self.state == "night":
            werewolf_votes.append(vote)
        votes.append(vote)

        for name in self.name_to_player:
            player = self.name_to_player[name]
            if self.state == "night" and isinstance(player, Werewolf):
                werewolf_votes.append(name)
            votes.append(name)

        if self.state == "day":
            majority_vote = get_most_common_element(votes)
            self.state = "night"
        elif self.state == "night":
            majority_vote = get_most_common_element(werewolf_votes)
            self.state = "day"

        if majority_vote in self.name_to_player:
            print(majority_vote)
            self.remove_player(majority_vote)
        else:
            raise Exception("Invalid player.")
        
    def remove_player(player_to_remove):
        """
        Set the player with the majority vote to inactive, and remove it from 
        its respective list of players.
        """
        player = self.name_to_player[player_to_remove]
        player.active = False

        if player in self.werewolves:
            self.werewolves.remove(player)
        elif player in self.villagers:
            self.villagers.remove(player)
        else:
            print("Player already removed!")

    def check_if_end_of_game(self):
        """
        Returns True if the game is over, and False if it is not. The game is over when 
        there are no werewolves remaining, or if there are more werewolves than villagers.
        """

        if len(self.werewolves) == 0:
            return True
        elif len(self.werewolves) > len(self.villagers):
            return True
        else:
            return False



# Optional, Extra Credit.
def lab10_extra_credit():
  """
  Fill in the values for these two variables.
  You will get the special code from the study tool when you complete all questions from lab.
  This code will be unique to your okpy email and this lab.
  Go here to practice: https://codestyle.herokuapp.com/cs88-lab10
  """
  okpy_email = "jack020945@berkeley.edu"
  practice_result_code = "111f3188a33698814d694447dcd3c620"
  return (okpy_email, practice_result_code)
        
