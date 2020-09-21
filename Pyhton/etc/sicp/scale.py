# def car(z):
    # return z("car")

# def cdr(z):
    # return z("cdr")

# def cons(x, y):
    # def dispatch(m):
        # if m == "car":
            # return x
        # elif m == "cdr":
            # return y

    # return dispatch



class cons:
    head = None
    tail = None
    
    def __init__(self, x, y):
        self.head = x
        self.tail = y
    def __str__(self):
        return str('[' + str(self.head) +', '+ str(self.tail) + ']')

def car(cons):
    return cons.head

def cdr(cons):
    return cons.tail


class list_(cons):
    head = None
    tail = None

    def __init__(self, x):
        self.head = x[0]
        if (len(x)!=1):
            self.tail = list_(x[1:])


def scale_list(l, s):
    if l.tail != None:
        return cons(car(l)*s, scale_list(cdr(l), s))
    else:
        return cons(l.head*s, None)

def scale_func(x, s):
    return car(x) * s


print(list_([1, 2, 3, 4]))
print(scale_list(list_([1, 2, 3, 4]), 10))
# print(list(map (lambda x: scale_func(x, 10), [1, 2, 3, 4])))
print(list(map (lambda x: x * 10, [1, 2, 3, 4])))
# print(list(reduce (lambda x: scale_func(x, 10), list_[1, 2, 3, 4])))
