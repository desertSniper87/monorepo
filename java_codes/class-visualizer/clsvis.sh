#!/bin/bash
#
# ClassVisualizer start script.
#
# This is a template, which can be copied and customized per project.
#
# invocation: clsvis.sh [-p path_to_project_file] [-cp classpath_to_add] [path_to_import]
#
DIR=`dirname "$0"`
LIB_PATH="$DIR/clsvis.jar"

#
# Choose java - use JAVA_HOME, if set.
#
if [ "$JAVA_HOME" != "" ]; then
	JAVA="$JAVA_HOME/bin/java"
else
	JAVA="java"
fi

#
# Choose Look and Feel.
#
# modern one - Nimbus
LAF="com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel"
# No explicit settings - java default (Ocean)
# MacOS - java default = system default, use screen menu bar
if [ `uname` == "Darwin" ]; then
    LAF=""
    OPTS="$OPTS -Dapple.laf.useScreenMenuBar=true"
fi

#
# Final settings.
#
# Look and Feel
if [ "$LAF" != "" ]; then
	OPTS="$OPTS -Dswing.defaultlaf=$LAF"
fi
# Memory
OPTS="$OPTS -Xmx512m"
# Additional info
#OPTS="$OPTS -verbose:gc" 

#
# Run.
#
CMD="$JAVA $OPTS -jar $LIB_PATH $@"
echo "$CMD"
$CMD
