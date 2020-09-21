title: What I had learnt from SICP.
slug: sicp
category: Computing
tags: programming, algorithm
date: 2018-05-05
modified: 2018-05-05
summary: hi there.
stylesheets: styles.css
<!--Status: draft-->
I wrote this to make a point that, after 33 *(1985-2018)* years, the book **Structure and Implementation of Programming language** is still useful for leaning useful programming concepts.


![SICP Cover]({filename}../images/SICP_cover.jpg){ style="display: block; margin-left: auto; margin-right: auto; width: 50%;"}
<br/>
<br/>
*From [**lambda-the-ultimate** Programming by poking: why MIT stopped teaching SICP:](http://lambda-the-ultimate.org/node/5335)*
> In 1997, Gerald Sussman and Harold Abelson stopped teaching MIT 6.001. They, felt that the SICP curriculum no longer prepared engineers for what engineering is like today. Sussman said that in the 80s and 90s, engineers built complex systems by combining simple and well-understood parts. The goal of SICP was to provide the abstraction language for reasoning about such systems.

> Today, this is no longer the case. Sussman pointed out that engineers now routinely write code for complicated hardware that they don’t fully understand (and often can’t understand because of trade secrecy.) The same is true at the software level. 

The working examples of this book is written in LISP as Gerald Sussman was one of the creators of SCHEME. This is an important observation after the usage of C, C++,  JAVA, Python, Javascript etc in introductory CS courses. I tried to reimplement it using python as I am comfortable using it.


###Chapter 1-4 : The importance of programming abstraction.

From Chapter *1.1 The Elements of Programming*,

1. **Primitive expressions**, which represent the simplest entities the language is concerned with,
2. **Means of combination**, by which compound elements are built from simpler ones, and
3. **Means of abstraction**, by which compound elements can be named and manipulated as units.

For example, in *2.1.1* **Arithmatic Operations for rational numbers**, the concept of data abstraction is introduced.
```python
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
```
Which yields `22 12`
One of the recurring themes of the course is, how complex things can be constructed using simple objects. For example, in LISP, `cons` creates a pair by taking two arguments. We can implement them using python.

```python

class cons:
    head = None
    tail = None
    
    def __init__(self, x, y):
        self.head = x
        self.tail = y
    def __str__(self):
        return str('[' + str(self.head) +', '+ str(self.tail) + ']')

```
`car` returns the first part of the pair and `cdr` returns the second.

```python
def car(cons):
    return cons.head

def cdr(cons):
    return cons.tail
```

We can implement a list using the `cons` construct by merging two cons.
```python
class list_(cons):
    head = None
    tail = None

    def __init__(self, x):
        self.head = x[0]
        if (len(x)!=1):
            self.tail = list_(x[1:])
```
So `print(list_([1, 2, 3, 4]))` returns   `[1, [2, [3, [4, None]]]]`

We can do a simple operation like scaling by 10: 
```python
def scale_list(l, s):
    if l.tail != None:
        return cons(car(l)*s, scale_list(cdr(l), s))
    else:
        return cons(l.head*s, None)
```
So ` print(scale_list(list_([1, 2, 3, 4]), 10))`  returns `[10, [20, [30, [40, None]]]]`

An interesting point is this: We need more abstractions the drown out the complexities in order the design elegant systems. So, we see from our example that, our list has become too complicated to use in real world. So, we introduce iteration*(In the book, it has been done using LISP lambda functions)*

```python
print(list(map (lambda x: x * 10, [1, 2, 3, 4])))
[10, 20, 30, 40]
```

**(To be continued)**
