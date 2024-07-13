cmake_minimum_required(VERSION 3.27)
project(psim)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include_directories(
    include
    src
)

add_executable(psim
    src/main.c
)
