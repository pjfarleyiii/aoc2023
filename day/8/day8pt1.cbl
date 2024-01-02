       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY8PT1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * UNCOMMENT WITH DEBUGGING CLAUSE FOR DEBUG LINES TO EXECUTE.
       SOURCE-COMPUTER.
           Z-SYSTEM
      *        WITH DEBUGGING MODE
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
           05  FILLER            REDEFINES LINEINPUT.
               10  INPT-NODE     PIC X(3).
               10  FILLER        PIC X(4).
               10  INPT-LEFT     PIC X(3).
               10  FILLER        PIC X(2).
               10  INPT-RIGHT    PIC X(3).
               10  FILLER        PIC X(32741).

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
           05  DIRCLEN           PIC  9(9) BINARY.
           05  DIRCNDX           PIC  9(9) BINARY.
           05  STEPS             PIC  9(9) BINARY.
           05  PART-NO           PIC  9(1)  VALUE ZEROES.
           05  DIRECTIONS        PIC  X(512).

       01  INDEX-FIELDS.
           05  START-NDX         PIC 9(9) BINARY.
           05  END-NDX           PIC 9(9) BINARY.
           05  CURR-NDX          PIC 9(9) BINARY.
           05  PREV-NDX          PIC 9(9) BINARY.
           05  ORD-OUT           PIC 9(9) BINARY.
           05  NDX-OUT           PIC 9(9) BINARY.
           05  ORD-IN            PIC X.
           05  NDX-IN            PIC X(3).

       01  NODE-TABLE.
           05  NODENDX               PIC  9(9) BINARY VALUE 0.
           05  NODE-AREA.
               10  NODE-DATA         OCCURS 20000 TIMES.
                   15  NODE-LEFT     PIC 9(9) BINARY.
                   15  NODE-RIGHT    PIC 9(9) BINARY.
                   15  NODE-ID       PIC X(3).
                   15  NODE-LEFT-ID  PIC X(3).
                   15  NODE-RIGHT-ID PIC X(3).

       01  TEMP-NODE.
                   15  TEMP-LEFT     PIC 9(9) BINARY.
                   15  TEMP-RIGHT    PIC 9(9) BINARY.
                   15  TEMP-ID       PIC X(3).
                   15  TEMP-LEFT-ID  PIC X(3).
                   15  TEMP-RIGHT-ID PIC X(3).

       LINKAGE SECTION.
       01  PARM-AREA.
           05  PARM-LEN              PIC S9(4) BINARY.
           05  PARM-PART             PIC  9.

       PROCEDURE DIVISION USING PARM-AREA.
           PERFORM 000-HOUSEKEEPING
           PERFORM 100-PROCESS-INPUT-DATA
           PERFORM 200-TRAVEL-MAP
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
           PERFORM 3 TIMES
               READ INPUT-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD +1 TO LINECTR
DEBUG>*                DISPLAY "LINE " LINECTR " LEN=" LINELEN
                       IF LINECTR = 1
                           MOVE LINELEN TO DIRCLEN
                           MOVE LINEINPUT (1 : DIRCLEN) TO DIRECTIONS
DEBUG>D                    DISPLAY "DIRECTIONS="
                               DIRECTIONS (1 : DIRCLEN)
                       END-IF
               END-READ
           END-PERFORM
           .

       100-PROCESS-INPUT-DATA.
           INITIALIZE NODE-TABLE
           PERFORM UNTIL END-OF-FILE
               PERFORM GEN-MAP-DATA
               IF NOT END-OF-FILE
                   READ INPUT-FILE
                       AT END SET END-OF-FILE TO TRUE
                       NOT AT END
                           ADD +1 TO LINECTR
DEBUG>*                    DISPLAY "LINE " LINECTR " LEN=" LINELEN
                   END-READ
               END-IF
           END-PERFORM
      D    DISPLAY "INPUT NODE TABLE:"
      D    PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > 20000
      D        IF NODE-LEFT (SS1) > 0
      D            DISPLAY "NODEID " NODE-ID (SS1) "[" SS1 "]="
      D                "(" NODE-LEFT-ID  (SS1)
      D                "[" NODE-LEFT  (SS1) "]"
      D                "," NODE-RIGHT-ID (SS1)
      D                "[" NODE-RIGHT (SS1) "])"
      D        END-IF
      D    END-PERFORM
           DISPLAY " ".

       200-TRAVEL-MAP.
           MOVE "AAA" TO NDX-IN
           PERFORM GEN-MAP-NDX
           MOVE NDX-OUT TO START-NDX
           MOVE "ZZZ" TO NDX-IN
           PERFORM GEN-MAP-NDX
           MOVE NDX-OUT TO END-NDX
           MOVE START-NDX TO CURR-NDX
           MOVE 1 TO DIRCNDX
           MOVE 0 TO STEPS
           PERFORM UNTIL CURR-NDX = END-NDX
               IF DIRCNDX > DIRCLEN
                   MOVE 1 TO DIRCNDX
               END-IF
               ADD 1 TO STEPS
               MOVE CURR-NDX TO PREV-NDX
               EVALUATE DIRECTIONS (DIRCNDX : 1)
                   WHEN 'L'
                       MOVE NODE-LEFT  (CURR-NDX) TO CURR-NDX
                   WHEN 'R'
                       MOVE NODE-RIGHT (CURR-NDX) TO CURR-NDX
               END-EVALUATE
      D        DISPLAY "STEP " STEPS " MOVED " DIRECTIONS (DIRCNDX : 1)
      D            " FROM NODE " NODE-ID (PREV-NDX) "[" PREV-NDX "]"
      D            " TO NODE "   NODE-ID (CURR-NDX) "[" CURR-NDX "]"
               ADD 1 TO DIRCNDX
           END-PERFORM
           MOVE STEPS TO PART1-VALUE
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

       GEN-MAP-DATA.
           MOVE INPT-NODE TO NDX-IN
           PERFORM GEN-MAP-NDX
           MOVE NDX-OUT TO NODENDX
           MOVE INPT-NODE  TO NODE-ID       (NODENDX)
           MOVE INPT-LEFT  TO NODE-LEFT-ID  (NODENDX)
           MOVE INPT-RIGHT TO NODE-RIGHT-ID (NODENDX)
           MOVE INPT-LEFT  TO NDX-IN
           PERFORM GEN-MAP-NDX
           MOVE NDX-OUT TO NODE-LEFT  (NODENDX)
           MOVE INPT-RIGHT TO NDX-IN
           PERFORM GEN-MAP-NDX
           MOVE NDX-OUT TO NODE-RIGHT (NODENDX)
      D    DISPLAY "NODEID " NODE-ID (NODENDX) "[" NODENDX "]="
      D        "(" NODE-LEFT-ID  (NODENDX) "[" NODE-LEFT  (NODENDX) "]"
      D        "," NODE-RIGHT-ID (NODENDX) "[" NODE-RIGHT (NODENDX) "])"
           .

       GEN-MAP-NDX.
           MOVE 0 TO NDX-OUT
           MOVE NDX-IN (1 : 1) TO ORD-IN
           PERFORM GEN-MAP-ORD
           COMPUTE NDX-OUT = NDX-OUT * 27 + ORD-OUT
           MOVE NDX-IN (2 : 1) TO ORD-IN
           PERFORM GEN-MAP-ORD
           COMPUTE NDX-OUT = NDX-OUT * 27 + ORD-OUT
           MOVE NDX-IN (3 : 1) TO ORD-IN
           PERFORM GEN-MAP-ORD
           COMPUTE NDX-OUT = NDX-OUT * 27 + ORD-OUT
           .

       GEN-MAP-ORD.
           IF ORD-IN = "0"
               MOVE 0 TO ORD-OUT
           ELSE
               COMPUTE ORD-OUT = FUNCTION ORD (ORD-IN) - 193
               IF ORD-OUT > 16
                   COMPUTE ORD-OUT = ORD-OUT - 7
               END-IF
               IF ORD-OUT > 18
                   COMPUTE ORD-OUT = ORD-OUT - 8
               END-IF
           END-IF
           .
