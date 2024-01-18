# aoc2023
Advent of Code 2023 challenge solutions

This solution was coded base on a very clever idea I found in the AOC 2023 solutions megathread:

https://github.com/clrfl/AdventOfCode2023/blob/master/12/explanation.ipynb

The basic idea is to set up a simple NFA (Nondeterministic Finite Automaton) based on the input group numbers that tell you how many springs are in valid spring arrangements for each line.

Not having any list or dictionary data structures in COBOL, instead I set up a table of elements where the index of the table represents the state mumber and the value of the table element counts the number of springs in that state.  The table is a sparse array substituting for a dictionary where the state is in the dictionary only when the count value is non-zero.

Obviously this will only work for numbered NFA states, but it is probably extensible to non-numeric state names with a little ingenuity.
