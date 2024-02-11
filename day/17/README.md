# aoc2023
Advent of Code 2023 challenge solutions

Note that this solution uses external COBOL subroutines PTYQPUSH and PTYQPOP that implement a min-heap priority queue that guarantees FIFO popping for equal key values.

Part 2 is implemented via PARM input to specify the minimum and maximum values that the lava cart can move.

Note also that there is a bunch of debugging code here dedicated to verifying the PUSH/POP functionality.  The amount of program source could be reduced significantly by removing all of the PUSH/POP verification code.  There is also a lot of "internal" structure definition and code that should be internal to just the PUSH and POP code and should be opaque to the using program.  If I get any time I may clean this all up but it solves the challenge as-is, so I decided to post it anyway, warts and all.
