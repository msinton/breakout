#!/bin/sh

PATH=${PATH}:./freeglut-MinGW-3.0.0-1.mp/freeglut/bin/x64/

# might need to run `stack build` first
stack exec breakout-exe
