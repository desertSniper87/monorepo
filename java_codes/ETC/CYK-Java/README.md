# Cocke-Younger-Algorithm 


This is the famous CYK algorithm implemented in Java.
It can be used to test a given string with a grammar file that is in ![Chomsky
Normal Form](https://en.wikipedia.org/wiki/Chomsky_normal_form).

The grammar file, which you can supply yourself or can use one of mine, must
follow these rules which conform to Chomsky Normal Form:

  (0 U 1)+:(0 U 1)+, (0 U 1)+
  
  OR
  
  (0 U 1)+:(a U b)

## Running the algorithm

1. Clone the project, cd into the folder and then the src folder then run javac
   to compile:

    1. `git clone git://github.com/akshayhegde/CYK-Java && cd CYK-Java/src/` 
    
    
    2. `javac Cyk.java`
    
2. Run the program by giving it a grammar file of your choosing and supplying
   a string:

       `java Cyk grammar.txt aabb`

### Requirements
  - Java 7
