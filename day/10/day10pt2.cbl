       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY10PT2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * UNCOMMENT WITH DEBUGGING CLAUSE FOR DEBUG LINES TO EXECUTE.
       SOURCE-COMPUTER.
           Z-SYSTEM
               WITH DEBUGGING MODE
           .
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
              ASSIGN TO AOCINPUT
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
      *-------------
       FILE SECTION.

       FD  INPUT-FILE
           RECORD VARYING 1 TO 32756 DEPENDING ON LINELEN
           RECORDING MODE V.
       01  INPUT-FIELDS.
           05  LINEINPUT         PIC X(32756).

       WORKING-STORAGE SECTION.
       01  INPUT-FILE-VARIABLES.
           05  LINELEN           PIC  9(18) BINARY.
           05  LINECTR           PIC S9(18) BINARY VALUE +0.
           05  SW-END-OF-FILE    PIC  X(01) VALUE SPACES.
               88 END-OF-FILE               VALUE "Y".

       01  SUBSCRIPT-VARIABLES.
           05  SS1               PIC S9(18) BINARY.
           05  SS2               PIC S9(18) BINARY.
           05  SS3               PIC S9(18) BINARY.
           05  SS4               PIC S9(18) BINARY.
           05  SS5               PIC S9(18) BINARY.
           05  ITEMCOUNT         PIC S9(18) BINARY.
           05  ITEMINDEX         PIC S9(18) BINARY.

       01 CHANGED-FLAG           PIC X.
          88 HASCHANGED                 VALUE 'Y'.
          88 HASNOTCHANGED              VALUE 'N'.

       01  ESCAPE-CHAR PIC X VALUE '^'.
       01  SEPARATOR-CHAR PIC X VALUE '|'.
       01  REFERENCE-STRING PIC X(256) VALUE
           'ONE^|UNO||THREE^^^^|FOUR^^^|^CUATRO|'.

       01  INPUT-STRING PIC X(256).
       01  C PIC 999.
       01  ESCAPED PIC X.

       01  T PIC 999.
       01  T-MAX PIC 999.
       01  T-LIM PIC 999 VALUE 128.
       01  TOKEN-TABLE.
           03  TOKEN-ENTRY OCCURS 128.
               05  TOKEN-LEN PIC 999.
               05  TOKEN PIC X(256).

       01  L PIC 999.
       01  L-LIM PIC 999 VALUE 256.

       01  ERROR-FOUND PIC X.

       01  GAME-VARIABLES.
           05  PART1-VALUE       PIC  9(18) VALUE ZEROES.
           05  PART2-VALUE       PIC  9(18) VALUE ZEROES.
           05  SUM-FNUM          PIC S9(18) VALUE ZEROES.
           05  TOTAL-NUM         PIC S9(18) VALUE ZEROES.
           05  GRIDSTEPS         PIC  9(9).
           05  GRIDLEN           PIC  9(9).
           05  START-ROW         PIC  9(9).
           05  START-COL         PIC  9(9).
           05  ADJ-ROW           PIC  9(9).
           05  ADJ-COL           PIC  9(9).
           05  CURR-ROW          PIC  9(9).
           05  CURR-COL          PIC  9(9).
           05  SRCH-ROW          PIC  9(9).
           05  SRCH-COL          PIC  9(9).
           05  SADD-ROW          PIC  9(9).
           05  SADD-COL          PIC  9(9).
           05  INTERIOR          PIC  9(9).
           05  INTERSECT         PIC  9(9).
           05  NORTH-PIPES       PIC  X(4)  VALUE "S|LJ".
           05  SOUTH-PIPES       PIC  X(4)  VALUE "S|7F".
           05  EAST-PIPES        PIC  X(4)  VALUE "S-7J".
           05  WEST-PIPES        PIC  X(4)  VALUE "S-LF".
           05  START-PIPE-SET    PIC  X(3).
           05  START-PIPES       PIC  X(6)  VALUE "|-LJ7F".
           05  CURR-PIPE         PIC  X.
           05  TEST-PIPE         PIC  X.
           05  SEEN-FOUND-SW     PIC  X.
               88  NOT-SEEN-FOUND           VALUE "N".
               88  SEEN-FOUND               VALUE "Y".
           05  PART-NO           PIC  9(1)  VALUE ZEROES.

       01  GRID-TABLE.
           05  GRIDMAX               PIC  9(9) BINARY VALUE 0.
           05  GRID-AREA.
               10  GRID-DATA         OCCURS 20000 TIMES.
                   15  GRID          PIC  X(256).

       01  SEEN-PIPES-TABLE.
           05  SEENMAX               PIC  9(9) BINARY VALUE 0.
           05  SEEN-AREA.
               10  SEEN-DATA         OCCURS 20000 TIMES
                                     INDEXED BY SEENNDX.
                   15  SEEN-ROW      PIC  9(9) BINARY VALUE 0.
                   15  SEEN-COL      PIC  9(9) BINARY VALUE 0.

       01  CHECK-PIPES-TABLE.
           05  CHECKMAX              PIC  9(9) BINARY VALUE 0.
           05  CHECK-AREA.
               10  CHECK-DATA        OCCURS 20000 TIMES.
                   15  CHECK-ROW     PIC  9(9) BINARY VALUE 0.
                   15  CHECK-COL     PIC  9(9) BINARY VALUE 0.

       01  CORNER-PIPES-TABLE.
           05  CORNERMAX             PIC  9(9) BINARY VALUE 0.
           05  CORNER-AREA.
               10  CORNER-DATA       OCCURS 20000 TIMES.
                   15  CORNER-PIPE   PIC  X.

       01  TEMP-GRID.
                   15  TEMP-VAL      PIC  X(256).

       LINKAGE SECTION.
       01  PARM-AREA.
           05  PARM-LEN              PIC S9(4) BINARY.
           05  PARM-PART             PIC  9.

       PROCEDURE DIVISION USING PARM-AREA.
           PERFORM 000-HOUSEKEEPING
           PERFORM 100-PROCESS-INPUT-DATA
           PERFORM 200-FIND-LOOP
           PERFORM 300-CLEAR-UNSEEN
           PERFORM 400-FIND-INTERIOR
           COMPUTE PART1-VALUE = GRIDSTEPS / 2
           DISPLAY "PART 1 = " PART1-VALUE
           IF PART-NO = 2
               COMPUTE PART2-VALUE = INTERIOR
               DISPLAY "PART 2 = " PART2-VALUE
           END-IF
           PERFORM 900-WRAP-UP
           GOBACK.

       000-HOUSEKEEPING.
           IF PARM-LEN = 0 OR PARM-PART NOT NUMERIC OR
              (PARM-PART NOT = 1 AND 2)
               MOVE 1 TO PART-NO
           ELSE
               MOVE PARM-PART TO PART-NO
           END-IF
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE
               AT END SET END-OF-FILE TO TRUE
               NOT AT END
                   ADD +1 TO LINECTR
                   MOVE LINELEN TO GRIDLEN
DEBUG>*            DISPLAY "LINE " LINECTR " LEN=" LINELEN
           END-READ
           .

       100-PROCESS-INPUT-DATA.
           MOVE 0 TO START-ROW START-COL
           PERFORM UNTIL END-OF-FILE
               ADD 1 TO GRIDMAX
               MOVE LINEINPUT (1 : LINELEN)
                 TO GRID (GRIDMAX) (1 : GRIDLEN)
               MOVE 0 TO SS1
               INSPECT GRID (GRIDMAX) (1 : GRIDLEN) TALLYING SS1
                   FOR CHARACTERS BEFORE INITIAL "S"
               IF SS1 < GRIDLEN
                   COMPUTE START-ROW = LINECTR
                   COMPUTE START-COL = SS1 + 1
               END-IF
               IF NOT END-OF-FILE
                   READ INPUT-FILE
                       AT END SET END-OF-FILE TO TRUE
                       NOT AT END
                           ADD +1 TO LINECTR
DEBUG>*                    DISPLAY "LINE " LINECTR " LEN=" LINELEN
                   END-READ
               END-IF
           END-PERFORM
      D    DISPLAY "INPUT GRID TABLE:"
      D        " START=[" START-ROW "," START-COL "]"
      D    PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > GRIDMAX
      D        DISPLAY "GRID [" SS1 "]="
      D                GRID (SS1) (1 : GRIDLEN)
      D    END-PERFORM
           MOVE 1 TO CHECKMAX SEENMAX
           MOVE START-ROW TO SEEN-ROW (1) CHECK-ROW (1)
           MOVE START-COL TO SEEN-COL (1) CHECK-COL (1)
      *    MOVE TOTAL-NUM TO PART1-VALUE
           DISPLAY " ".

       200-FIND-LOOP.
           MOVE 0 TO GRIDSTEPS
           PERFORM WITH TEST BEFORE UNTIL CHECKMAX = 0
               ADD 1 TO GRIDSTEPS
               PERFORM POP-CHECK-PIPES
               MOVE GRID (CURR-ROW) (CURR-COL : 1) TO CURR-PIPE
      D        DISPLAY "PIPE CURR = " CURR-PIPE
      D            " AT [" CURR-ROW "," CURR-COL "]"
               IF CURR-ROW > 1
      *
      *            IF ROW > 0 AND CURRENT_PIPE IN "S|LJ" AND
      *               GRID[ROW - 1][COLUMN] IN "|7F" AND
      *               (ROW - 1, COLUMN) NOT IN SEEN_PIPES:
      *                SEEN_PIPES.ADD((ROW - 1, COLUMN))
      *                CHECK_PIPES.APPEND((ROW - 1, COLUMN))
      *
      D            COMPUTE ADJ-ROW = CURR-ROW - 1
      D            DISPLAY "PIPE NORTH = "
      D                GRID (ADJ-ROW) (CURR-COL : 1)
      D                " AT [" ADJ-ROW "," CURR-COL "]"
                   MOVE 0 TO SS1 SS2
                   INSPECT NORTH-PIPES TALLYING SS1
                       FOR ALL CURR-PIPE
                   INSPECT SOUTH-PIPES (2 : ) TALLYING SS2
                       FOR ALL GRID (CURR-ROW - 1) (CURR-COL : 1)
                   COMPUTE SRCH-ROW = CURR-ROW - 1
                   MOVE CURR-COL TO SRCH-COL
                   PERFORM SRCH-SEEN-PIPES
      D            DISPLAY "COND#1 " CURR-PIPE
      D                ",SS1=" SS1 ",SS2=" SS2 ",SEEN=" SEEN-FOUND-SW
                   IF SS1 > 0 AND
                      SS2 > 0 AND
                      NOT-SEEN-FOUND
      D                DISPLAY "COND#1 IS TRUE"
                       MOVE SRCH-ROW TO SADD-ROW
                       MOVE SRCH-COL TO SADD-COL
                       PERFORM ADD-SEEN-NEXT
                       ADD 1 TO CHECKMAX
                       MOVE SRCH-ROW TO CHECK-ROW (CHECKMAX)
                       MOVE SRCH-COL TO CHECK-COL (CHECKMAX)
                       IF CURR-PIPE = "S"
                           MOVE NORTH-PIPES (2 : ) TO START-PIPE-SET
                           PERFORM START-PIPE-INTERSECT
                       END-IF
                   END-IF
               END-IF
               IF CURR-ROW < GRIDMAX
      *
      *            IF ROW < LEN(GRID) - 1 AND CURRENT_PIPE IN "S|7F" AND
      *               GRID[ROW + 1][COLUMN] IN "|LJ" AND
      *               (ROW + 1, COLUMN) NOT IN SEEN_PIPES:
      *                SEEN_PIPES.ADD((ROW + 1, COLUMN))
      *                CHECK_PIPES.APPEND((ROW + 1, COLUMN))
      *
      D            COMPUTE ADJ-ROW = CURR-ROW + 1
      D            DISPLAY "PIPE SOUTH = "
      D                GRID (ADJ-ROW) (CURR-COL : 1)
      D                " AT [" ADJ-ROW "," CURR-COL "]"
                   MOVE 0 TO SS1 SS2
                   INSPECT SOUTH-PIPES TALLYING SS1
                       FOR ALL CURR-PIPE
                   INSPECT NORTH-PIPES (2 : ) TALLYING SS2
                       FOR ALL GRID (CURR-ROW + 1) (CURR-COL : 1)
                   COMPUTE SRCH-ROW = CURR-ROW + 1
                   MOVE CURR-COL TO SRCH-COL
                   PERFORM SRCH-SEEN-PIPES
      D            DISPLAY "COND#2 " CURR-PIPE
      D                ",SS1=" SS1 ",SS2=" SS2 ",SEEN=" SEEN-FOUND-SW
                   IF SS1 > 0 AND
                      SS2 > 0 AND
                      NOT-SEEN-FOUND
      D                DISPLAY "COND#2 IS TRUE"
                       MOVE SRCH-ROW TO SADD-ROW
                       MOVE SRCH-COL TO SADD-COL
                       PERFORM ADD-SEEN-NEXT
                       ADD 1 TO CHECKMAX
                       MOVE SRCH-ROW TO CHECK-ROW (CHECKMAX)
                       MOVE SRCH-COL TO CHECK-COL (CHECKMAX)
                       IF CURR-PIPE = "S"
                           MOVE SOUTH-PIPES (2 : ) TO START-PIPE-SET
                           PERFORM START-PIPE-INTERSECT
                       END-IF
                   END-IF
               END-IF
               IF CURR-COL > 1
      *
      *            IF COLUMN > 0 AND CURRENT_PIPE IN "S-7J" AND
      *               GRID[ROW][COLUMN - 1] IN "-LF" AND
      *               (ROW, COLUMN - 1) NOT IN SEEN_PIPES:
      *                SEEN_PIPES.ADD((ROW, COLUMN - 1))
      *                CHECK_PIPES.APPEND((ROW, COLUMN - 1))
      *
      D            COMPUTE ADJ-COL = CURR-COL - 1
      D            DISPLAY "PIPE EAST  = "
      D                GRID (CURR-ROW) (ADJ-COL : 1)
      D                " AT [" CURR-ROW "," ADJ-COL "]"
                   MOVE 0 TO SS1 SS2
                   INSPECT EAST-PIPES TALLYING SS1
                       FOR ALL CURR-PIPE
                   INSPECT WEST-PIPES (2 : ) TALLYING SS2
                       FOR ALL GRID (CURR-ROW) (CURR-COL - 1 : 1)
                   MOVE CURR-ROW TO SRCH-ROW
                   COMPUTE SRCH-COL = CURR-COL - 1
                   PERFORM SRCH-SEEN-PIPES
      D            DISPLAY "COND#3 " CURR-PIPE
      D                ",SS1=" SS1 ",SS2=" SS2 ",SEEN=" SEEN-FOUND-SW
                   IF SS1 > 0 AND
                      SS2 > 0 AND
                      NOT-SEEN-FOUND
      D                DISPLAY "COND#3 IS TRUE"
                       MOVE SRCH-ROW TO SADD-ROW
                       MOVE SRCH-COL TO SADD-COL
                       PERFORM ADD-SEEN-NEXT
                       ADD 1 TO CHECKMAX
                       MOVE SRCH-ROW TO CHECK-ROW (CHECKMAX)
                       MOVE SRCH-COL TO CHECK-COL (CHECKMAX)
                       IF CURR-PIPE = "S"
                           MOVE  EAST-PIPES (2 : ) TO START-PIPE-SET
                           PERFORM START-PIPE-INTERSECT
                       END-IF
                   END-IF
               END-IF
               IF CURR-COL < GRIDLEN
      *
      *            IF COLUMN < LEN(GRID[ROW]) - 1 AND
      *               CURRENT_PIPE IN "S-LF" AND
      *               GRID[ROW][COLUMN + 1] IN "-J7" AND
      *               (ROW, COLUMN + 1) NOT IN SEEN_PIPES:
      *                SEEN_PIPES.ADD((ROW, COLUMN + 1))
      *                CHECK_PIPES.APPEND((ROW, COLUMN + 1))
      *
      D            COMPUTE ADJ-COL = CURR-COL + 1
      D            DISPLAY "PIPE WEST  = "
      D                GRID (CURR-ROW) (ADJ-COL : 1)
      D                " AT [" CURR-ROW "," ADJ-COL "]"
                   MOVE 0 TO SS1 SS2
                   INSPECT WEST-PIPES TALLYING SS1
                       FOR ALL CURR-PIPE
                   INSPECT EAST-PIPES (2 : ) TALLYING SS2
                       FOR ALL GRID (CURR-ROW) (CURR-COL + 1 : 1)
                   MOVE CURR-ROW TO SRCH-ROW
                   COMPUTE SRCH-COL = CURR-COL + 1
                   PERFORM SRCH-SEEN-PIPES
      D            DISPLAY "COND#4 " CURR-PIPE
      D                ",SS1=" SS1 ",SS2=" SS2 ",SEEN=" SEEN-FOUND-SW
                   IF SS1 > 0 AND
                      SS2 > 0 AND
                      NOT-SEEN-FOUND
      D                DISPLAY "COND#4 IS TRUE"
                       MOVE SRCH-ROW TO SADD-ROW
                       MOVE SRCH-COL TO SADD-COL
                       PERFORM ADD-SEEN-NEXT
                       ADD 1 TO CHECKMAX
                       MOVE SRCH-ROW TO CHECK-ROW (CHECKMAX)
                       MOVE SRCH-COL TO CHECK-COL (CHECKMAX)
                       IF CURR-PIPE = "S"
                           MOVE  WEST-PIPES (2 : ) TO START-PIPE-SET
                           PERFORM START-PIPE-INTERSECT
                       END-IF
                   END-IF
               END-IF
      D        DISPLAY "SEEN  TABLE AT STEP " GRIDSTEPS
      D            " HAS " SEENMAX " ENTRIES"
      *        PERFORM VARYING SS3 FROM 1 BY 1 UNTIL SS3 > SEENMAX
      *            DISPLAY "SEEN  " SS3
      *                " [" SEEN-ROW (SS3) "," SEEN-COL (SS3) "]"
      *        END-PERFORM
      D        DISPLAY "CHECK TABLE AT STEP " GRIDSTEPS
      D            " HAS " CHECKMAX " ENTRIES"
      *        PERFORM VARYING SS3 FROM 1 BY 1 UNTIL SS3 > CHECKMAX
      *            DISPLAY "CHECK " SS3
      *                " [" CHECK-ROW (SS3) "," CHECK-COL (SS3) "]"
      *        END-PERFORM
DEBUG>D        IF GRIDSTEPS > 15000
DEBUG>D            DISPLAY "MORE THAN 15000 STEPS, QUITTING"
DEBUG>D            EXIT PERFORM
DEBUG>D        END-IF
           END-PERFORM
           .

       300-CLEAR-UNSEEN.
           MOVE START-PIPES (1 : 1)
             TO GRID (START-ROW) (START-COL : 1)
           PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > GRIDMAX
               MOVE SS1 TO SRCH-ROW
               PERFORM VARYING SS2 FROM 1 BY 1 UNTIL SS2 > GRIDLEN
                   MOVE SS2 TO SRCH-COL
                   PERFORM SRCH-SEEN-PIPES
                   IF NOT-SEEN-FOUND
                       MOVE "." TO GRID (SS1) (SS2 : 1)
                   END-IF
               END-PERFORM
           END-PERFORM
      D    DISPLAY "CLEAN GRID TABLE:"
      D        " START=[" START-ROW "," START-COL "]"
      D    PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > GRIDMAX
      D        DISPLAY "GRID [" SS1 "]="
      D                GRID (SS1) (1 : GRIDLEN)
      D    END-PERFORM
           .

       400-FIND-INTERIOR.
           MOVE 0 TO INTERIOR
           PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > GRIDMAX
               PERFORM VARYING SS2 FROM 1 BY 1 UNTIL SS2 > GRIDLEN
                   IF GRID (SS1) (SS2 : 1) NOT = "."
      D                DISPLAY "    IGNORING PATH"
      D                    " AT [" SS1 "," SS2 "]"
                       EXIT PERFORM CYCLE
                   END-IF
      D            DISPLAY "    EXPLORING PIPE "
      D                GRID (SS1) (SS2 : 1)
      D                " AT [" SS1 "," SS2 "]"
                   MOVE 0 TO INTERSECT CORNERMAX
                   COMPUTE SS3 = SS2 + 1
                   PERFORM VARYING SS3 FROM SS3 BY 1
                       UNTIL SS3 > GRIDLEN
                       MOVE GRID (SS1) (SS3 : 1) TO TEST-PIPE
      D                DISPLAY "      TESTING PATH " TEST-PIPE
      D                    " AT [" SS1 "," SS3 "]"
                       IF TEST-PIPE = "|"
      D                    DISPLAY "      TEST PATH IS VERTICAL PIPE"
                           ADD 1 TO INTERSECT
                       END-IF
                       IF TEST-PIPE = "F" OR "L"
      D                    DISPLAY "      TEST PATH IS F OR L"
                           ADD 1 TO CORNERMAX
                           MOVE TEST-PIPE TO CORNER-PIPE (CORNERMAX)
                       END-IF
                       IF CORNERMAX > 0
      D                    DISPLAY "      CORNER (LAST)="
      D                        CORNER-PIPE (CORNERMAX)
                           IF (TEST-PIPE = "J" AND
                               CORNER-PIPE (CORNERMAX) = "F") OR
                              (TEST-PIPE = "7" AND
                               CORNER-PIPE (CORNERMAX) = "L")
      D                        DISPLAY "      FOUND F OR L AFTER "
      D                            TEST-PIPE
                               PERFORM POP-CORNER-PIPES
                               ADD 1 TO INTERSECT
                           END-IF
                       END-IF
                   END-PERFORM
      D            DISPLAY "    INTERSECT=" INTERSECT
      D                " AT [" SS1 "," SS2 "]"
                   IF FUNCTION MOD (INTERSECT, 2) = 1
                       ADD 1 TO INTERIOR
                   END-IF
      D            DISPLAY "    INTERIOR=" INTERIOR
      D                " AT [" SS1 "," SS2 "]"
               END-PERFORM
      D        DISPLAY "  AFTER ROW " SS1 " INTERIOR=" INTERIOR
           END-PERFORM
           .

       900-WRAP-UP.
           CLOSE INPUT-FILE.

       TOKENIZE.
      *    DISPLAY SPACE
      *    DISPLAY 'STRING:'
      *    DISPLAY INPUT-STRING (1 : 64)

           MOVE 'N' TO ESCAPED ERROR-FOUND
           MOVE 1 TO T-MAX
           INITIALIZE TOKEN-ENTRY(T-MAX)
           MOVE 0 TO L

           PERFORM VARYING C FROM 1 BY 1
               UNTIL C > FUNCTION LENGTH(INPUT-STRING)
                  OR INPUT-STRING(C:) = SPACES

               EVALUATE ESCAPED ALSO INPUT-STRING(C:1)
                   WHEN 'N' ALSO ESCAPE-CHAR
                       MOVE 'Y' TO ESCAPED
                   WHEN 'N' ALSO SEPARATOR-CHAR
                       PERFORM INCREMENT-T-MAX
                       IF ERROR-FOUND = 'Y'
                           EXIT PARAGRAPH
                       END-IF
                   WHEN 'N' ALSO ANY
                       PERFORM MOVE-C
                       IF ERROR-FOUND = 'Y'
                           EXIT PARAGRAPH
                       END-IF
                   WHEN 'Y' ALSO ANY
                       PERFORM MOVE-C
                       IF ERROR-FOUND = 'Y'
                           EXIT PARAGRAPH
                       END-IF
                       MOVE 'N' TO ESCAPED
               END-EVALUATE
           END-PERFORM

           IF L > 0
               MOVE L TO TOKEN-LEN(T-MAX)
           END-IF

      *    IF C = 1
      *        DISPLAY 'NO TOKENS'
      *    ELSE
      *        DISPLAY 'TOKENS:'
      *        PERFORM VARYING T FROM 1 BY 1 UNTIL T > T-MAX
      *            IF TOKEN-LEN(T) > 0
      *                DISPLAY T ': ' TOKEN-LEN(T) SPACE
      *                        TOKEN(T) (1 : TOKEN-LEN(T))
      *            ELSE
      *                DISPLAY T ': ' TOKEN-LEN(T)
      *            END-IF
      *        END-PERFORM
      *    END-IF
           .
       INCREMENT-T-MAX.
           IF T-MAX >= T-LIM
               DISPLAY 'ERROR: AT ' C ' NUMBER OF TOKENS EXCEEDS ' T-LIM
               MOVE 'Y' TO ERROR-FOUND
           ELSE
               MOVE L TO TOKEN-LEN(T-MAX)
               ADD 1 TO T-MAX
               INITIALIZE TOKEN-ENTRY(T-MAX)
               MOVE 0 TO L
               MOVE 'N' TO ERROR-FOUND
           END-IF
           .
       MOVE-C.
           IF L >= L-LIM
               DISPLAY 'ERROR: AT ' C ' TOKEN LENGTH EXCEEDS ' L-LIM
               MOVE 'Y' TO ERROR-FOUND
           ELSE
               ADD 1 TO L
               MOVE INPUT-STRING(C:1) TO TOKEN(T-MAX)(L:1)
               MOVE 'N' TO ERROR-FOUND
           END-IF
           .

       POP-CHECK-PIPES.
           MOVE CHECK-ROW (1) TO CURR-ROW
           MOVE CHECK-COL (1) TO CURR-COL
           IF CHECKMAX > 1
               PERFORM VARYING SS2 FROM 2 BY 1 UNTIL SS2 > CHECKMAX
      D            COMPUTE SS3 = SS2 - 1
      D            DISPLAY "CHECK " SS2 " MOVING TO CHECK " SS3
      D                " = "  FUNCTION HEX-OF (CHECK-DATA (SS3))
      D                " TO " FUNCTION HEX-OF (CHECK-DATA (SS2))
                   MOVE CHECK-DATA (SS2) TO CHECK-DATA (SS2 - 1)
               END-PERFORM
           END-IF
           SUBTRACT 1 FROM CHECKMAX
      D    DISPLAY "CHECK TABLE AFTER POP AT " GRIDSTEPS
      D        " HAS " CHECKMAX " ENTRIES"
      D    PERFORM VARYING SS2 FROM 1 BY 1 UNTIL SS2 > CHECKMAX
      D        DISPLAY "CHECK " SS2
      D            " [" CHECK-ROW (SS2) "," CHECK-COL (SS2) "]"
      D    END-PERFORM
           .

       SRCH-SEEN-PIPES.
           SET SEENNDX TO 1
           SET NOT-SEEN-FOUND TO TRUE
           SEARCH SEEN-DATA VARYING SEENNDX
               WHEN SEEN-ROW (SEENNDX) = SRCH-ROW AND
                    SEEN-COL (SEENNDX) = SRCH-COL
                   SET SEEN-FOUND TO TRUE
           END-SEARCH
           .

       ADD-SEEN-NEXT.
           ADD 1 TO SEENMAX
           MOVE SADD-ROW TO SEEN-ROW (SEENMAX)
           MOVE SADD-COL TO SEEN-COL (SEENMAX)
           .

       START-PIPE-INTERSECT.
           MOVE 0 TO SS3
           PERFORM VARYING SS4 FROM 1 BY 1
               UNTIL SS4 > FUNCTION LENGTH (START-PIPE-SET)
               PERFORM VARYING SS5 FROM 1 BY 1
                   UNTIL SS5 > FUNCTION LENGTH (START-PIPES)
                   IF START-PIPES (SS5 : 1) = START-PIPE-SET (SS4 : 1)
                       ADD 1 TO SS3
                       MOVE START-PIPES (SS5 : 1)
                         TO START-PIPES (SS3 : 1)
                   END-IF
               END-PERFORM
           END-PERFORM
           MOVE SPACES TO START-PIPES (SS3 + 1 : )
      D    DISPLAY "START-PIPE='" START-PIPES (1 : SS3) "'"
           .

       POP-CORNER-PIPES.
           IF CORNERMAX > 1
               PERFORM VARYING SS4 FROM 2 BY 1 UNTIL SS4 > CORNERMAX
      D            COMPUTE SS5 = SS4 - 1
      D            DISPLAY "CORNER " SS4 " MOVING TO CORNER " SS5
                   MOVE CORNER-PIPE (SS4) TO CORNER-PIPE (SS4 - 1)
               END-PERFORM
           END-IF
           SUBTRACT 1 FROM CORNERMAX
      D    DISPLAY "CORNER TABLE AFTER POP "
      D        " HAS " CORNERMAX " ENTRIES = '"
      D        CORNER-AREA (1 : CORNERMAX) "'"
           .
