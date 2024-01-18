# aoc2023
Advent of Code 2023 challenge solutions

These solutions were all compiled and run using the IBM Enterprise COBOL V6.4 compiler on the IBM z/OS operating system.

In particular they use the IBM compiler option of being able to have "debugging lines" marked with "D" in the comment column (column 7) which are not compiled or executed unless there is a specific phrase in the CONFIGURATION SECTION in the SOURCE-COMPUTER paragraph, to wit:

```
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * UNCOMMENT "WITH DEBUGGING" CLAUSE FOR DEBUG LINES TO EXECUTE.
       SOURCE-COMPUTER.
           Z-SYSTEM
      *        WITH DEBUGGING MODE
           .
```

If the phrase ```WITH DEBUGGING MODE``` is not commented, then lines with "D" in column 7 WILL be compiled and executed.

They may or may not compile and run using other COBOL compilers.  If you get them running using any other COBOL, drop me a note on how you did it and what you had to change to make them work.
