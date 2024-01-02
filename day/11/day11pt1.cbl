       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY11PT1.
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
           05  SS6               PIC S9(18) BINARY.
           05  SS7               PIC S9(18) BINARY.
           05  SS8               PIC S9(18) BINARY.
           05  S91               PIC S9(09) BINARY.
           05  S92               PIC S9(09) BINARY.
           05  S93               PIC S9(09) BINARY.
           05  S94               PIC S9(09) BINARY.
           05  S95               PIC S9(09) BINARY.
           05  S96               PIC S9(09) BINARY.
           05  S97               PIC S9(09) BINARY.
           05  S98               PIC S9(09) BINARY.
           05  S41               PIC S9(04) BINARY.
           05  S42               PIC S9(04) BINARY.
           05  S43               PIC S9(04) BINARY.
           05  S44               PIC S9(04) BINARY.
           05  S45               PIC S9(04) BINARY.
           05  S46               PIC S9(04) BINARY.
           05  S47               PIC S9(04) BINARY.
           05  S48               PIC S9(04) BINARY.
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
           05  IMAGESTEPS        PIC  9(9)  VALUE ZEROES.
           05  IMAGELEN          PIC  9(9).
           05  CURR-ROW          PIC  9(9).
           05  CURR-COL          PIC  9(9).
           05  GLXY-FOUND-SW     PIC  X.
               88  NOT-GLXY-FOUND           VALUE "N".
               88  GLXY-FOUND               VALUE "Y".
           05  PART-NO           PIC  9(1)  VALUE ZEROES.
           05  COL-IMAGE         PIC  X(256).

       01  IMAGE-TABLE.
           05  IMAGEMAX              PIC  9(9) BINARY VALUE 0.
           05  IMAGE-AREA.
               10  IMAGE-DATA        OCCURS 256 TIMES.
                   15  IMAGE         PIC  X(256).

       01  GLXY-TABLE.
           05  GLXYMAX               PIC  9(9) BINARY VALUE 0.
           05  GLXY-AREA.
               10  GLXY-DATA         OCCURS 2560 TIMES
                                     INDEXED BY GLXYNDX.
                   15  GLXY-ROW      PIC  9(9) BINARY VALUE 0.
                   15  GLXY-COL      PIC  9(9) BINARY VALUE 0.

       01  ROWADD-TABLE.
           05  ROWADDS               PIC  9(9) BINARY VALUE 0.
           05  RADD-AREA.
               10  RADD-DATA         OCCURS 256 TIMES
                                     INDEXED BY RADDNDX.
                   15  RADD-ROW      PIC  9(9) BINARY VALUE 0.

       01  COLADD-TABLE.
           05  COLADDS               PIC  9(9) BINARY VALUE 0.
           05  CADD-AREA.
               10  CADD-DATA         OCCURS 256 TIMES
                                     INDEXED BY CADDNDX.
                   15  CADD-COL      PIC  9(9) BINARY VALUE 0.

       01  GALAXY-PAIR.
           05  GPAIR-AREA.
               10  GPAIR-DATA        OCCURS 2 TIMES.
                   15  GPAIR-ROW     PIC  9(9) BINARY VALUE 0.
                   15  GPAIR-COL     PIC  9(9) BINARY VALUE 0.

       01  TEMP-GLXY.
                   15  TEMP-ROW      PIC  9(9) BINARY VALUE 0.
                   15  TEMP-COL      PIC  9(9) BINARY VALUE 0.

       LINKAGE SECTION.
       01  PARM-AREA.
           05  PARM-LEN              PIC S9(4) BINARY.
           05  PARM-PART             PIC  9.

       PROCEDURE DIVISION USING PARM-AREA.
           PERFORM 000-HOUSEKEEPING
           PERFORM 100-PROCESS-INPUT-DATA
           PERFORM 200-MAP-GALAXIES
           PERFORM 300-CALC-DISTANCES
           IF PART-NO = 1
               DISPLAY "PART 1 = " PART1-VALUE
           ELSE
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
                   MOVE LINELEN TO IMAGELEN
DEBUG>*            DISPLAY "LINE " LINECTR " LEN=" LINELEN
           END-READ
           .

       100-PROCESS-INPUT-DATA.
           PERFORM UNTIL END-OF-FILE
               ADD 1 TO IMAGEMAX
               MOVE LINEINPUT (1 : LINELEN)
                 TO IMAGE (IMAGEMAX) (1 : IMAGELEN)
      *        ARE THERE ANY GALAXIES IN THIS IMAGE LINE?
               MOVE 0 TO S91
               INSPECT IMAGE (IMAGEMAX) (1 : IMAGELEN) TALLYING S91
                   FOR ALL "#"
      *        IF NOT, REMEMBER THE EMPTY ROW NUMBER
               IF S91 = 0
                   ADD 1 TO ROWADDS
                   MOVE LINECTR TO RADD-ROW (ROWADDS)
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
      D    DISPLAY "EMPTY ROWS AT LINES:"
      D    PERFORM VARYING S91 FROM 1 BY 1 UNTIL S91 > ROWADDS
      D        DISPLAY "ROW [" S91 "]=" RADD-ROW (S91)
      D    END-PERFORM
      D    DISPLAY "IMAGE AFTER INPUT COMPLETE HAS " IMAGEMAX
      D        " ROWS AND " IMAGELEN " COLUMNS"
      D        ", " ROWADDS " EMPTY ROWS"
      D    PERFORM VARYING S91 FROM 1 BY 1 UNTIL S91 > IMAGEMAX
      D        DISPLAY "IMAGE [" S91 "]="
      D                IMAGE (S91) (1 : IMAGELEN)
      D    END-PERFORM
      *    NOW LOOK FOR EMPTY COLUMNS
           MOVE 1 TO S91
           PERFORM UNTIL S91 > IMAGELEN
      *        LOOK FOR GALAXIES IN COLUMN S91
               SET NOT-GLXY-FOUND TO TRUE
               PERFORM VARYING S92 FROM 1 BY 1 UNTIL S92 > IMAGEMAX
                   IF IMAGE (S92) (S91 : 1) = "#"
                       SET GLXY-FOUND TO TRUE
      D                DISPLAY "GALAXY FOUND AT " S92 "," S91
                   END-IF
               END-PERFORM
      *        IF NO GALAXIES IN THIS IMAGE COLUMN RECORD IT AS EMPTY
               IF NOT-GLXY-FOUND
                   ADD 1 TO COLADDS
                   MOVE S91 TO CADD-COL (COLADDS)
               END-IF
               ADD 1 TO S91
      *        DISPLAY "NEXT COLUMN EXAMINED WILL BE " S91
           END-PERFORM
      D    DISPLAY "EMPTY COLUMNS AT:"
      D    PERFORM VARYING S91 FROM 1 BY 1 UNTIL S91 > COLADDS
      D        DISPLAY "COL [" S91 "]=" CADD-COL (S91)
      D    END-PERFORM
           DISPLAY " ".

       200-MAP-GALAXIES.
           PERFORM VARYING S91 FROM 1 BY 1 UNTIL S91 > IMAGEMAX
               PERFORM VARYING S92 FROM 1 BY 1 UNTIL S92 > IMAGELEN
                   IF IMAGE (S91) (S92 : 1) = "#"
                       ADD 1 TO GLXYMAX
                       MOVE S91 TO GLXY-ROW (GLXYMAX)
                       MOVE S92 TO GLXY-COL (GLXYMAX)
                   END-IF
               END-PERFORM
           END-PERFORM
      D    DISPLAY "GALAXY MAP HAS " GLXYMAX " GALAXIES"
      D    PERFORM VARYING S91 FROM 1 BY 4 UNTIL S91 > GLXYMAX
      D        COMPUTE S92 = S91 + 1
      D        COMPUTE S93 = S91 + 2
      D        COMPUTE S94 = S91 + 3
      D        DISPLAY "GALAXY [" S91 "]="
      D            "(" GLXY-ROW (S91) "," GLXY-COL (S91) "),"
      D            "[" S92 "]="
      D            "(" GLXY-ROW (S92) "," GLXY-COL (S92) "),"
      D            "[" S93 "]="
      D            "(" GLXY-ROW (S93) "," GLXY-COL (S93) "),"
      D            "[" S94 "]="
      D            "(" GLXY-ROW (S94) "," GLXY-COL (S94) ")"
      D    END-PERFORM
           .

       300-CALC-DISTANCES.
           MOVE 0 TO PART1-VALUE IMAGESTEPS
           PERFORM VARYING S91 FROM 1 BY 1 UNTIL S91 > GLXYMAX
               COMPUTE S93 = S91 + 1
               MOVE GLXY-DATA (S91) TO GPAIR-DATA (1)
               PERFORM VARYING S92 FROM S93 BY 1 UNTIL S92 > GLXYMAX
                   MOVE GLXY-DATA (S92) TO GPAIR-DATA (2)
      *            PART 1 EMPTY MULTIPLIER IS 2, PART 2 IS 1000000
      *            ACCUMULATE THE ROW DISTANCE IN SS4
                   MOVE 0 TO SS4
      *            IF NOT IN SAME ROW
                   IF GPAIR-ROW (1) NOT = GPAIR-ROW (2)
      *                CALCULATE ROW DISTANCE WITH EMPTY MULTIPLIER
                       COMPUTE S95 = 1 +
                           FUNCTION MIN (GPAIR-ROW (1), GPAIR-ROW (2))
                       COMPUTE S96 =
                           FUNCTION MAX (GPAIR-ROW (1), GPAIR-ROW (2))
                       PERFORM VARYING S94 FROM S95 BY 1 UNTIL S94 > S96
                           SET RADDNDX TO 1
                           SEARCH RADD-DATA
                               AT END
                                   ADD 1 TO SS4
                               WHEN S94 = RADD-ROW (RADDNDX)
                                   IF PART-NO = 1
                                       ADD 2 TO SS4
                                   ELSE
                                       ADD 1000000 TO SS4
                                   END-IF
                           END-SEARCH
                       END-PERFORM
                   END-IF
      *            ACCUMULATE THE COLUMN DISTANCE IN SS5
                   MOVE 0 TO SS5
      *            IF NOT IN SAME COLUMN
                   IF GPAIR-COL (1) NOT = GPAIR-COL (2)
      *                CALCULATE COLUMN DISTANCE WITH EMPTY MULTIPLIER
                       COMPUTE S97 = 1 +
                           FUNCTION MIN (GPAIR-COL (1), GPAIR-COL (2))
                       COMPUTE S98 =
                           FUNCTION MAX (GPAIR-COL (1), GPAIR-COL (2))
                       PERFORM VARYING S94 FROM S97 BY 1 UNTIL S94 > S98
                           SET CADDNDX TO 1
                           SEARCH CADD-DATA
                               AT END
                                   ADD 1 TO SS5
                               WHEN S94 = CADD-COL (CADDNDX)
                                   IF PART-NO = 1
                                       ADD 2 TO SS5
                                   ELSE
                                       ADD 1000000 TO SS5
                                   END-IF
                           END-SEARCH
                       END-PERFORM
                   END-IF
                   ADD 1 TO IMAGESTEPS
                   IF PART-NO = 1
                       COMPUTE PART1-VALUE = PART1-VALUE + SS4 + SS5
      D                DISPLAY "FROM GALAXY [" S91 "] "
      D                    "AT " GPAIR-ROW (1) "," GPAIR-COL (1)
      D                    " TO GALAXY [" S92 "] "
      D                    "AT " GPAIR-ROW (2) "," GPAIR-COL (2)
      D                    " ROWDIST=" SS4 " COLDIST=" SS5
      D                    " PART1=" PART1-VALUE ",STEPS=" IMAGESTEPS
                   ELSE
                       COMPUTE PART2-VALUE = PART2-VALUE + SS4 + SS5
      D                DISPLAY "FROM GALAXY [" S91 "] "
      D                    "AT " GPAIR-ROW (1) "," GPAIR-COL (1)
      D                    " TO GALAXY [" S92 "] "
      D                    "AT " GPAIR-ROW (2) "," GPAIR-COL (2)
      D                    " ROWDIST=" SS4 " COLDIST=" SS5
      D                    " PART2=" PART2-VALUE ",STEPS=" IMAGESTEPS
                   END-IF
               END-PERFORM
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
