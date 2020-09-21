#!/usr/bin/env bash

export CLASSPATH=".:antlr-4.7.1-complete.jar:$CLASSPATH"

alias grun='java -Xmx500M -cp "antlr-4.7.1-complete.jar:$CLASSPATH" org.antlr.v4.gui.TestRig'
alias antlr4='java -Xmx500M -cp "antlr-4.7.1-complete.jar:$CLASSPATH" org.antlr.v4.Tool'
