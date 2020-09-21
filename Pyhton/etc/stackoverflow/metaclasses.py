def choose_class(name):
    """TODO: Docstring for choose_class.

    :name: TODO
    :returns: TODO

    """
    if name == 'foo':
        class Foo(object):
            pass
        return Foo

    else: 
        class Bar(object):
            pass
        return Bar

def main():
    MyClass = choose_class('foo')

if __name__ == "__main__":
    main()


