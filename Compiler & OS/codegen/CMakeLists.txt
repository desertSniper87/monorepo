cmake_minimum_required(VERSION 3.6)
project(codegen)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES main.cpp in.c lex.l lex.yy.c lex.yy.cpp SymbolTable.cpp SymbolTable.h token.txt command.py)
add_executable(codegen ${SOURCE_FILES})