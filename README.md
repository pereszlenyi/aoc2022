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
Some interesting features of the language are being case insensitive and that [arrays are indexed from 1](day_01/most_calories.f90#L14).

### Solution for [Day 1: Calorie Counting](https://adventofcode.com/2022/day/1) [&#128194;](day_01)

The source code of the solution is in [`most_calories.f90`](day_01/most_calories.f90).
To build and run it, just execute [`build_and_run.sh`](day_01/build_and_run.sh).
It will build the source using [CMake](https://cmake.org/) and will run the executable on input [`input.txt`](day_01/input.txt).

Both parts of the puzzle can be solved by calculating the top 3 most Calories which is stored in the [`top_calories`](day_01/most_calories.f90#L8) variable in our case.

## Day 2: COBOL

[COBOL](https://en.wikipedia.org/wiki/COBOL) (COmmon Business-Oriented Language) was created in 1959 for business use.
While programming the previous day's solution in the *evolved* version of Fortran was essentially painless, the same can't be said about COBOL.
I feel COBOL still has the vibe of what programming was in the 60s.
It's also [really verbose](https://en.wikipedia.org/wiki/COBOL#Syntax).
Since COBOL is still widely used (and also because of its historical importance), I had to include it here.
Furthermore, according to a [course on LinkedIn Learning](https://www.linkedin.com/learning/cobol-essential-training/), "organizations are often willing to pay a premium for candidates who can review and update existing COBOL code".
Some interesting features of the language are:

* Statements end with a period.
* A line is a comment if there is an `*` at column 7.
* Program statements and keywords must start at or after column 8.
* Code can't go past column 72.

### Solution for [Day 2: Rock Paper Scissors](https://adventofcode.com/2022/day/2) [&#128194;](day_02)

Similarly to the above, the source code is in [`rock_paper_scissors.cbl`](day_02/rock_paper_scissors.cbl), to build and run it, execute [`build_and_run.sh`](day_02/build_and_run.sh), and the input is in [`input.txt`](day_02/input.txt).
Fortunately, the puzzle is quite easy.
The only thing that may be a bit interesting (in the solution) is [how the score is calculated](day_02/rock_paper_scissors.cbl#L50-L58).

## Day 3: Lisp

Lisp (LISt Processor) was designed by [John McCarthy](https://en.wikipedia.org/wiki/John_McCarthy_(computer_scientist)) in 1960 and was first implemented by [Steve Russell](https://en.wikipedia.org/wiki/Steve_Russell_(computer_scientist)).
Initially, Lisp was meant to be a completely theoretical (mathematical) programming language but then quickly became the favoured language for early artificial intelligence research.
This origin makes Lisp quite an unusual language compared to Fortran or COBOL.
Lisp has evolved over the years and many dialects emerged.
We use here one of the most common, [ANSI Common Lisp](https://en.wikipedia.org/wiki/Common_Lisp).
Some interesting features of the language are:

* In Lisp, `((`everything`)` `(`is in`)` `(`parentheses`))`.
* Lisp uses a [*prefix notation*](https://en.wikipedia.org/wiki/Polish_notation) which means that `1 + 2` is written as `(+ 1 2)`.
* [Linked list](https://en.wikipedia.org/wiki/Linked_list) is the major data structure and code is also represented as lists.
* The following functions are all used to check for equality: `equal`, `eql`, `eq`,`=`, `equalp`, `string-equal`.

### Solution for [Day 3: Rucksack Reorganization](https://adventofcode.com/2022/day/3) [&#128194;](day_03)

The source code of the solution is in file [`rucksack.lisp`](day_03/rucksack.lisp).
To build and run it, execute [`build_and_run.sh`](day_03/build_and_run.sh).
The solutions to both parts of the puzzle boil down to calculating intersections of sets.
In our solution, it is done without using loops.
This is possible in Lisp by using [higher-order functions](https://en.wikipedia.org/wiki/Higher-order_function) and [recursion](https://en.wikipedia.org/wiki/Recursion) instead.

## Day 4: BASIC (on the Commodore 64)

[BASIC](https://en.wikipedia.org/wiki/BASIC) (Beginners' All-purpose Symbolic Instruction Code) was created by [John G. Kemeny](https://en.wikipedia.org/wiki/John_G._Kemeny) (or, in Hungarian, Kemény János György) and [Thomas E. Kurtz](https://en.wikipedia.org/wiki/Thomas_E._Kurtz) in 1963.
They wanted to ease access to computers for students without strong scientific backgrounds.
Later in the 80s, BASIC became hugely popular thanks to the emergence of [early home computers](https://en.wikipedia.org/wiki/Home_computer).
These computers came with a [BASIC interpreter](https://en.wikipedia.org/wiki/BASIC_interpreter) preinstalled.
It comes as no surprise that many of today's famous tech [people started with BASIC](https://youtu.be/nnjg3G2gkok?t=12).

On a personal note, BASIC was also the first programming language for me.
It ran on the legendary [Commodore 64](https://en.wikipedia.org/wiki/Commodore_64) that my parents bought way back, even before the fall of the [Iron Curtain](https://en.wikipedia.org/wiki/Iron_Curtain).
Although most of the time I was [p](https://youtu.be/vdQnaLyYoM4?t=44)[l](https://youtu.be/5JF9gTknSK8?t=29)[a](https://youtu.be/hDAhixO2t5w?t=13)[y](https://youtu.be/Y0pLMkAtwpQ?t=23)[i](https://youtu.be/ivHFP3dJAkM?t=26)[n](https://youtu.be/WAfc_Ugki5U?t=30)[g](https://youtu.be/KyjYejrJxek?t=18) [g](https://youtu.be/CX8jdKNO4t8?t=11)[a](https://youtu.be/ekAVecHyN_4?t=9)[m](https://youtu.be/LWFGZ8Gj1Ws?t=18)[e](https://youtu.be/6BOXR008V2I?t=226)[s](https://youtu.be/X1jKHHeufS0?t=135) on it, I occasionally tried to write some *basic* programs too.
As a tribute to this amazing machine, I decided to solve this day's puzzle using [C64's BASIC](https://www.c64-wiki.com/wiki/C64-Commands).
Some interesting features of (this) BASIC are:

* It was originally developed by [Microsoft](https://en.wikipedia.org/wiki/Microsoft_BASIC).
* Lines of the program code have to be numbered.
* [Variable](https://www.c64-wiki.com/wiki/Variable) names have to be unique in their first two letters, as the rest is ignored by the interpreter.
* BASIC received [harsh criticism](https://programmingisterrible.com/post/40132515169/dijkstra-basic) from [Dijkstra](https://en.wikipedia.org/wiki/Edsger_W._Dijkstra).

### Solution for [Day 4: Camp Cleanup](https://adventofcode.com/2022/day/4) [&#128194;](day_04)

The challenge started by figuring out how to run a BASIC code written for the Commodore 64.
Unfortunately, I don't have access to a Commodore any more.
So, the next best thing is [VICE](https://vice-emu.sourceforge.io/), "the Versatile Commodore Emulator".
Running [`install.sh`](install.sh) will set this up for you.
The source of the solution is in [`cleanup.bas`](day_04/cleanup.bas) and the input is in [`input.txt`](day_04/input.txt), as usual.
Running this on the emulator is not entirely trivial though.
While we are used to [ASCII](https://en.wikipedia.org/wiki/ASCII) text files on the PC, Commodore uses a different character set (called [PETSCII](https://www.c64-wiki.com/wiki/PETSCII)).
This requires the source and input files to be [converted](day_04/run.sh#L23-L26).
The converted files then have to be [written to a virtual disk image](day_04/run.sh#L27-L32).
This disk image could be written to an actual floppy disk and then the program could be run on an actual C64.
We can also [start the emulator](day_04/run.sh#L35-L36) with the disk image attached.
All of this is done by [`run.sh`](day_04/run.sh), so all you need to do is run it.
It will open the emulator window and start the program.
With the default input, you will see the following on the emulator's screen:

~~~
    **** COMMODORE 64 BASIC V2 ****

 64K RAM SYSTEM  38911 BASIC BYTES FREE

READY.
LOAD"*",8,1:

SEARCHING FOR *
LOADING
READY.
RUN:

PROGRAM STARTED
PROCESSING INPUT... DONE
# OF FULL CONTAINMENTS: 2
# OF OVERLAPS: 4

READY.
~~~

You can look at the source code inside the emulator by typing in [LIST](https://www.c64-wiki.com/wiki/LIST) and you can run the program again by [RUN](https://www.c64-wiki.com/wiki/RUN_(BASIC)).

## Day 5: Pascal (with Turbo Pascal)

[Pascal](https://en.wikipedia.org/wiki/Pascal_(programming_language)) was designed by [Niklaus Wirth](https://en.wikipedia.org/wiki/Niklaus_Wirth) in 1970.
He named the language after the French scientist [Blaise Pascal](https://en.wikipedia.org/wiki/Blaise_Pascal).
In the 90s, [Commodore](#day-4-basic-on-the-commodore-64) home computers were replaced by [IBM PC compatible computers](https://en.wikipedia.org/wiki/IBM_PC_compatible) and, at the same time, the popularity of [BASIC](#day-4-basic-on-the-commodore-64) declined.
A major cause of this decline was the increasing popularity of Pascal and, in particular, [Turbo Pascal](https://en.wikipedia.org/wiki/Turbo_Pascal).

Turbo Pascal was first released in 1983 by [Borland](https://en.wikipedia.org/wiki/Borland).
At that time, typically, programmers used multiple tools to create an executable program, such as a text editor to write the code and a compiler to compile it.
In Turbo Pascal, all these functionalities were integrated in an [integrated development environment (IDE)](https://en.wikipedia.org/wiki/Integrated_development_environment).
Contributing to its popularity, the compilation time was fast and it produced efficient executables, hence the name "Turbo".
Pascal (and Turbo Pascal) was also widely used in introductory courses to programming.
It was also the first language that I learnt in school.
We used Turbo Pascal (obviously) that ran on [MS-DOS](https://en.wikipedia.org/wiki/MS-DOS).
Later in the early 2000s, Borland released a few Turbo Pascal versions as [freeware](https://en.wikipedia.org/wiki/Freeware).
To honor its legacy, I solved this day's puzzle using the latest free version of Turbo Pascal, [version 5.5](https://en.wikipedia.org/wiki/Turbo_Pascal#Version_5.5).

Some interesting features of the language are:

* Operator `:=` ([walrus operator](https://en.wiktionary.org/wiki/walrus_operator)) is used to assign a value to a variable.
* Multi-line comments are between `(*` and `*)` and single-line comments are enclosed by `{` and `}`.
* Blocks of code are written between the `begin` and `end` keywords.
* A [pointer](https://www.tutorialspoint.com/pascal/pascal_pointers.htm) type is defined by prefixing the base type with `^`.
The address of a variable can be obtained by the `@` operator.
* The `string` type is a character array where the character at index `0` defines the length of the string.
Consequently, the maximum length of a `string` is 255 and the characters of the string start at index `1`.

### Solution for [Day 5: Supply Stacks](https://adventofcode.com/2022/day/5) [&#128194;](day_05)

Since Turbo Pascal 5.5 is for DOS, we will use [DOSBox](https://www.dosbox.com/) to run it.
DOSBox is a great DOS emulator that you can use to run many of the classic DOS games.
Running [`install.sh`](install.sh) installs DOSBox and downloads Turbo Pascal 5.5.

The source code of the solution is in [`stacks.pas`](day_05/stacks.pas) and the default input is in [`input.txt`](day_05/input.txt).
As usual, to build and run the solution, you just need to execute [`build_and_run.sh`](day_05/build_and_run.sh).
This script does a couple of things.
It [creates a shared folder](day_05/build_and_run.sh#L41-L43) that will be the `C:` drive in DOS.
Then it [extracts Turbo Pascal 5.5](day_05/build_and_run.sh#L47-L48) from the downloaded zip file and [copies `stacks.pas` and `input.txt`](day_05/build_and_run.sh#L49-L50) to the shared folder.
It then [starts DOSBox](day_05/build_and_run.sh#L54-L58) and, inside DOSBox, [compiles the source file](day_05/build_and_run.sh#L57) using `TPC.EXE`, the command-line compiler.
Finally, it will [run the executable](day_05/build_and_run.sh#L58) with `input.txt`.

With the DOSBox window open, you can start the Turbo Pascal IDE by typing in `Disk1\TURBO.EXE`.
Then you can load `stacks.pas` into the editor, edit the code, build it, run it, etc.
The source code is also compatible with [Free Pascal](https://www.freepascal.org/), so you can also use the Free Pascal Compiler to compile `stacks.pas`.

Now, let's have some notes on the solution to the puzzle.
We have to create a data structure to store the crates and stacks.
I decided to use [linked lists](https://en.wikipedia.org/wiki/Linked_list) for both the crates and the stacks because they can be dynamically extended as the input file is processed.
Crates of a stack are stored in a linked list whose nodes have type [`CrateNode`](day_05/stacks.pas#L9-L12).
Stacks are also in a linked list made up of nodes of type [`StackNode`](day_05/stacks.pas#L17-L21).
So, essentially, we have a linked list of linked lists.
What remains is just to implement the functions that manipulate these lists.
Note that, [memory is only allocated when reading the initial configuration](day_05/stacks.pas#L97) of the crates.
Later, [move operations](day_05/stacks.pas#L125) only manipulate pointers.
