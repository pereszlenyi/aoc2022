# Advent of Code 2022

This is my attempt to solve the [2022 Advent of Code](https://adventofcode.com/2022) using a different programming language for each day's puzzle.
The purpose is just to learn about these languages.
Currently, I can only program in a handful of languages so I'll need to learn most of them from zero.
I don't know how long it will take or how far I will go but I'll try my best.
I decided to document my progress here.

## General Information

I'm developing using [Ubuntu](https://ubuntu.com/desktop/developers) because that's the best and also for everybody to be able to follow.
Even if your main OS isn't Ubuntu, you can still run it in a [virtual machine](https://ubuntu.com/tutorials/how-to-run-ubuntu-desktop-on-a-virtual-machine-using-virtualbox) or, under Windows, using [WSL](https://ubuntu.com/tutorials/install-ubuntu-on-wsl2-on-windows-11-with-gui-support).
You will need a bunch of compilers, interpreters, build tools, etc. to be able to run the solutions.
To make it easy, you can install all the requirements by running [`install.sh`](install.sh).
(It will ask for your password and you must be able to [sudo](https://en.wikipedia.org/wiki/Sudo).)

Now, let's jump in and solve puzzles!

## Day 1: Fortran

[Fortran](https://en.wikipedia.org/wiki/Fortran) (FORmula TRANslator) is regarded as the [first widely used high-level programming language](https://en.wikipedia.org/wiki/History_of_programming_languages#First_programming_languages).
It was created in 1957 by [John Backus](https://en.wikipedia.org/wiki/John_Backus) and folks at [IBM](https://www.ibm.com/ibm/history/ibm100/us/en/icons/fortran/).
Despite being so old, Fortran is still [alive and under development](https://fortran-lang.org/).
New features are being added with each version.
Some interesting features of the language are being case insensitive and that arrays are indexed from 1.

### Solution for [Day 1: Calorie Counting](https://adventofcode.com/2022/day/1)

The source code of the solution is in [`most_calories.f90`](day_01/most_calories.f90).
To build and run it, just execute [`build_and_run.sh`](day_01/build_and_run.sh).
It will build the source using [CMake](https://cmake.org/) and will run the executable on input [`input.txt`](day_01/input.txt).

Both parts of the puzzle can be solved by calculating the top 3 most Calories which is stored in the `top_calories` variable in our case.
