@echo off
rem
rem ClassVisualizer start script.
rem
rem This is a template, which can be copied and customized per project.
rem
rem invocation: clsvis.sh [-p path_to_project_file] [-cp classpath_to_add] [path_to_import]
rem
set DIR=%~dp0
set LIB_PATH=%DIR%\clsvis.jar

rem
rem Choose java - use JAVA_HOME, if set.
rem
if "%JAVA_HOME%" == "" (
	set JAVA=java
) else (
	set JAVA=%JAVA_HOME%/bin/java
)

rem
rem Choose Look and Feel.
rem
rem No explicit settings - java default (Ocean)
rem set LAF=
rem Windows default
set LAF=com.sun.java.swing.plaf.windows.WindowsLookAndFeel
rem Windows classic
rem set LAF=com.sun.java.swing.plaf.windows.WindowsClassicLookAndFeel
rem modern one - Nimbus
rem set LAF=com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel

rem
rem Final settings and run.
rem
rem Look and Feel
set OPTS=
if not "%LAF%" == "" (
	set OPTS=-Dswing.defaultlaf=%LAF%
)
rem Memory
set OPTS=%OPTS% -Xmx512m

rem
rem Run.
rem
set CMD="%JAVA%" %OPTS% -jar "%LIB_PATH%" %*
echo %CMD%
%CMD%
