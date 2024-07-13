cmake_minimum_required(VERSION 3.27)
project(psim)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include_directories(
    src
)

add_executable(psim
    src/main.c
    src/cpu.c
)
