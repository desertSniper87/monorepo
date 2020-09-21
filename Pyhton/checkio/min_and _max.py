def min(*args, **kwargs):
    main_value = 9999
    for i in args:
        if main_value > i:
            main_value = i
    return main_value


def max(*args, **kwargs):
    max_value = -9999
    for i in args:
        if i is not int:


        if max_value < i:
            max_value = i
        print(max_value)
    return max_value


if __name__ == '__main__':
    #These "asserts" using only for self-checking and not necessary for auto-testing
    assert max(3, 2) == 3, "Simple case max"
    assert min(3, 2) == 2, "Simple case min"
    # assert max([1, 2, 0, 3, 4]) == 4, "From a list"
    assert min("hello") == "e", "From string"
    assert max(2.2, 5.6, 5.9, key=int) == 5.6, "Two maximal items"
    assert min([[1, 2], [3, 4], [9, 0]], key=lambda x: x[1]) == [9, 0], "lambda key"
