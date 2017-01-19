# Advent of Code

## Introduction

Advent of code is an annual programming challenge with 25 problems released
daily from the 1st through 25th of December. This project consists of solutions
to the problems released in December of 2016. The solutions are implemented in
Haskell. The majority of functions are implemented in a tail recursive manner
as this greatly decreases run time and memory usage.

The problems themselves are presented as part of a story. The story consists of
Santa attempting recover stars which have been stolen from him by the Easter
Bunny. He collects two stars for each problem solved, as well as advancing
farther in the Easter Bunny's Headquarters.

## Project Structure

Each directory in this project is a self contained solution to one problem. The
solution can be found in main.hs. Important and non-trivial functions are
preceded by comments expounding their usage and implementation. When compiled,
the program takes two arguments, the first being the run mode (either
-1, -2, or -12), which determines if solutions are found for part 1, part 2, or
both. The second argument is the file containing the input for the problem. The
input which was used for my specific solution can be found in input.txt.

## Resources

[Advent of Code 2016](http://adventofcode.com/2016)
