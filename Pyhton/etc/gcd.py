def gcd(a, b):
    """TODO: Docstring for gcd.

    :a: int
    :b: int
    :returns: int

    """

    if a == b:
        return a
    elif a > b:
        return gcd(a-b ,b)
    elif a < b:
        return gcd(b-a, a)

print(gcd(54, 48))
