class Rational():
    def __init__(self, n, d):
            self.numerator = n
            self.denominator = d

def numer(x):
    return x.numerator
def denom(x):
    return x.denominator

def make_rat(n, d):
    rat = Rational(n, d)
    return rat

def add_rat(x, y):
    print(x.numerator, y.numerator, x.denominator, y.denominator)
    return make_rat(((numer(x)* denom(y)) +\
                    (numer(y)* denom(x))),\
                    (denom(x)*numer(y)))
            
rat1 = make_rat(2, 3)
rat2 = make_rat(4, 5)
rat = add_rat(rat1, rat2)
print(rat.numerator, rat.denominator)

