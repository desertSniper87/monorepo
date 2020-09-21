#!/usr/bin/env bash

java -jar antlr-4.7.1-complete.jar c_parser/C.g4
javac -cp antlr-4.7.1-complete.jar c_parser/*.java
cd c_parser
java -cp .:../antlr-4.7.1-complete.jar org.antlr.v4.gui.TestRig C compilationUnit -gui ../src.c