from collections import frozenset
from collections import namedtuple
from math import sqrt

Circle = namedtuple('circle', 'x, y, radius')

circle = Circle(13, 84, 19)

def distance_from_origin(x, y):
    return sqrt(x**2 + y**2)

print(distance_from_origin(circle.x, circle.y))



