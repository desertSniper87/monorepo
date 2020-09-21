cmake_minimum_required(VERSION 3.6) 
project(rolling_stone)
set(CMAKE_CXX_STANDARD 11) 

set(SOURCE_FILES moves.c debug.c ida.c deadlock.c bitstring.c pensearch.c
init.c io.c mark.c conflicts.c deadsearch.c dl.c mymem.c histogram.c
weights.c lowerbound.c hashtable.c stats.c tree.c menu.c macro.c gtv.c time.c)

add_executable(RS ${SOURCE_FILES})






