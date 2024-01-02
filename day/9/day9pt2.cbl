       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY9PT2.
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
           05  PART-NO           PIC  9(1)  VALUE ZEROES.

       01  NUM-TABLE.
           05  NUMMAX                PIC  9(9) BINARY VALUE 0.
           05  NUM-AREA.
               10  NUM-DATA          OCCURS 256 TIMES.
                   15  NUM           PIC S9(18) BINARY.

       01  FINAL-NUM-TABLE.
           05  FNUMMAX               PIC  9(9) BINARY VALUE 0.
           05  FNUM-AREA.
               10  FNUM-DATA         OCCURS 256 TIMES.
                   15  FNUM          PIC S9(18) BINARY.

       01  TEMP-NUM.
                   15  TEMP-VAL      PIC S9(18) BINARY.

       LINKAGE SECTION.
       01  PARM-AREA.
           05  PARM-LEN              PIC S9(4) BINARY.
           05  PARM-PART             PIC  9.

       PROCEDURE DIVISION USING PARM-AREA.
           PERFORM 000-HOUSEKEEPING
           PERFORM 100-PROCESS-INPUT-DATA
           DISPLAY "PART " PART-NO " = " PART1-VALUE
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
DEBUG>*            DISPLAY "LINE " LINECTR " LEN=" LINELEN
           END-READ
           .

       100-PROCESS-INPUT-DATA.
           MOVE 0 TO TOTAL-NUM
           PERFORM UNTIL END-OF-FILE
               INITIALIZE NUM-TABLE FINAL-NUM-TABLE
               PERFORM GET-NUM-DATA
      D        DISPLAY "INPUT NUM TABLE:"
      D        PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > NUMMAX
      D            DISPLAY "NUM [" SS1 "]="
      D                    NUM (SS1)
      D        END-PERFORM
               PERFORM GET-NEXT-NUM
               IF NOT END-OF-FILE
                   READ INPUT-FILE
                       AT END SET END-OF-FILE TO TRUE
                       NOT AT END
                           ADD +1 TO LINECTR
DEBUG>*                    DISPLAY "LINE " LINECTR " LEN=" LINELEN
                   END-READ
               END-IF
           END-PERFORM
           MOVE TOTAL-NUM TO PART1-VALUE
           DISPLAY " ".

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

       GET-NUM-DATA.
           MOVE LINEINPUT (1 : LINELEN) TO INPUT-STRING
           INSPECT INPUT-STRING (1 : LINELEN) REPLACING ALL SPACE BY "|"
           MOVE "|" TO SEPARATOR-CHAR
           PERFORM TOKENIZE
           MOVE 0 TO NUMMAX
           PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > T-MAX
               ADD 1 TO NUMMAX
               COMPUTE NUM (NUMMAX) = FUNCTION NUMVAL (
                   TOKEN (SS1) (1 : TOKEN-LEN (SS1)) )
           END-PERFORM
           .

       GET-NEXT-NUM.
           PERFORM UNTIL NUMMAX = 1
               ADD 1 TO FNUMMAX
               IF PART-NO = 1
                   MOVE NUM (NUMMAX) TO FNUM (FNUMMAX)
               ELSE
                   MOVE NUM (1)      TO FNUM (FNUMMAX)
               END-IF
               PERFORM VARYING SS1 FROM 2 BY 1 UNTIL SS1 > NUMMAX
                   COMPUTE NUM (SS1 - 1) = NUM (SS1) - NUM (SS1 - 1)
               END-PERFORM
               SUBTRACT 1 FROM NUMMAX
      D        DISPLAY "DIFFERENCE TABLE:"
      D        PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > NUMMAX
      D            DISPLAY "NUM [" SS1 "]="
      D                    NUM (SS1)
      D        END-PERFORM
           END-PERFORM
           MOVE 0 TO SUM-FNUM
      D    DISPLAY "NEXT NUM TABLE:"
           PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > FNUMMAX
      D        DISPLAY "FNUM [" SS1 "]="
      D                FNUM (SS1)
               IF FUNCTION MOD(SS1, 2) = 0
                   COMPUTE SUM-FNUM = SUM-FNUM + FNUM (SS1)
               ELSE
                   COMPUTE SUM-FNUM = SUM-FNUM - FNUM (SS1)
               END-IF
           END-PERFORM
      D    DISPLAY "NEXT NUM = " SUM-FNUM
           COMPUTE TOTAL-NUM = TOTAL-NUM + SUM-FNUM
           .
