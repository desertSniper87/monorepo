#!/usr/bin/env bash

java -jar antlr-4.7.1-complete.jar js_parser/ECMAScript.g4
javac -cp antlr-4.7.1-complete.jar js_parser/*.java
cd js_parser
java -cp .:../antlr-4.7.1-complete.jar org.antlr.v4.gui.TestRig ECMAScript program -gui ../src.js