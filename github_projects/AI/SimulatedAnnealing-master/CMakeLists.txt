cmake_minimum_required(VERSION 3.6)
project(SimulatedAnnealing_master)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES
    sa.cpp
    SA_output.txt
    tai100_10_1.txt
    tai100_20_1.txt
    tai100_5_1.txt
    tai10_3_1.txt
    tai20_10_1.txt
    tai20_20_1.txt
    tai20_5_1.txt
    tai50_10_1.txt
    tai50_20_1.txt
    tai50_5_1.txt
    ts.txt)

add_executable(SimulatedAnnealing_master ${SOURCE_FILES})