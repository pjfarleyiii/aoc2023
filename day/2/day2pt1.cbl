       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY2PT1.
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
           05  LINEINPUT  PIC X(32756).

       WORKING-STORAGE SECTION.
       01  INPUT-FILE-VARIABLES.
           05  LINELEN           PIC  9(8) BINARY.
           05  LINECTR           PIC S9(8) BINARY VALUE +0.
           05  SW-END-OF-FILE    PIC X(01) VALUE SPACES.
               88 END-OF-FILE              VALUE "Y".

       01  SUBSCRIPT-VARIABLES.
           05  SS1               PIC  9(8) BINARY.
           05  SS2               PIC  9(8) BINARY.
           05  SS3               PIC  9(8) BINARY.
           05  SS4               PIC  9(8) BINARY.

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
           05  GAMENO            PIC  9(8) BINARY.
           05  GAMELEN           PIC  9(8) BINARY.
           05  GAMESUM           PIC  9(8) BINARY VALUE 0.
           05  PART1-VALUE       PIC  9(8) VALUE ZEROES.
           05  GAME-STRING       PIC  X(256).

       01  DRAW-VARIABLES.
           05  DRAWSMAX          PIC  9(8) BINARY.
           05  DRAWSLEN          PIC  9(8) BINARY.
           05  GAME-POSSIBLE-SW  PIC  X.
               88  GAME-POSSIBLE VALUE "Y".
               88  NOT-POSSIBLE  VALUE "N".
           05  DRAWS-STRING      PIC  X(256).
           05  DRAW-TABLE        OCCURS 32 TIMES.
               10  DRAWLEN       PIC  9(8) BINARY.
               10  DRAWSTR       PIC  X(256).
               10  REVLSMAX      PIC  9(8) BINARY.
               10  REVL-TABLE    OCCURS 3 TIMES.
                   15  REVLLEN   PIC  9(8) BINARY.
                   15  REVLSTR   PIC  X(256).
                   15  COLORVAL  PIC 99.
                   15  COLORNAM  PIC  X(5).
                       88 REVLNONE   VALUE SPACES, LOW-VALUES.
                       88 REVLRED    VALUE "RED  ".
                       88 REVLGREEN  VALUE "GREEN".
                       88 REVLBLUE   VALUE "BLUE ".

       01  COLOR-VARIABLES.
           05  COLORMAX          PIC  9.
           05  COLOR-TABLE       OCCURS 2 TIMES.
               10  COLORLEN      PIC  9.
               10  COLORSTR      PIC  X(5).

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING
           PERFORM 100-PROCESS-INPUT-DATA
           PERFORM 900-WRAP-UP
           GOBACK.

       000-HOUSEKEEPING.
      D    MOVE REFERENCE-STRING TO INPUT-STRING
      D    PERFORM TOKENIZE
      D    MOVE "," TO SEPARATOR-CHAR
      D    MOVE " 1 RED, 2 GREEN, 6 BLUE" TO INPUT-STRING
      D    PERFORM TOKENIZE

           OPEN INPUT INPUT-FILE
           READ INPUT-FILE
               AT END SET END-OF-FILE TO TRUE
               NOT AT END
                   ADD +1 TO LINECTR
DEBUG>D            DISPLAY "LINE " LINECTR " LEN=" LINELEN
           END-READ.

       100-PROCESS-INPUT-DATA.
           PERFORM UNTIL END-OF-FILE
               DISPLAY LINEINPUT (1 : LINELEN)
               MOVE ":" TO SEPARATOR-CHAR
               MOVE LINEINPUT (1 : LINELEN) TO INPUT-STRING
               PERFORM TOKENIZE
               MOVE TOKEN (1) (1 : TOKEN-LEN (1)) TO GAME-STRING
               MOVE TOKEN-LEN (1) TO GAMELEN
               MOVE TOKEN (2) (1 : TOKEN-LEN (2)) TO DRAWS-STRING
               COMPUTE DRAWSLEN = TOKEN-LEN (2)
               MOVE "," TO SEPARATOR-CHAR
               MOVE GAME-STRING (1 : GAMELEN) TO INPUT-STRING
               MOVE "," TO INPUT-STRING (5 : 1)
               PERFORM TOKENIZE
               MOVE TOKEN (2) (1 : TOKEN-LEN (2)) TO GAMENO
      D        DISPLAY "GAME=" GAMENO
               MOVE ";" TO SEPARATOR-CHAR
               MOVE DRAWS-STRING (1 : DRAWSLEN) TO INPUT-STRING
               PERFORM TOKENIZE
               MOVE +0 TO DRAWSMAX
               PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > T-MAX
                   IF TOKEN-LEN(SS1) > 0
                       ADD +1 TO DRAWSMAX
                       INITIALIZE DRAW-TABLE (DRAWSMAX)
                       MOVE TOKEN(SS1) (1 : TOKEN-LEN(SS1))
                         TO DRAWSTR (DRAWSMAX)
                       COMPUTE DRAWLEN (DRAWSMAX) = TOKEN-LEN(SS1)
      D                DISPLAY "DRAW[" DRAWSMAX "]="
      D                    DRAWSTR (DRAWSMAX) (1 : DRAWLEN (DRAWSMAX))
                   END-IF
               END-PERFORM
               MOVE "," TO SEPARATOR-CHAR
               PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > DRAWSMAX
                   MOVE DRAWSTR (SS1) (1 : DRAWLEN (SS1))
                     TO INPUT-STRING
                   PERFORM TOKENIZE
                   MOVE +0 TO REVLSMAX (SS1)
                   INITIALIZE REVL-TABLE (SS1, 1)
                              REVL-TABLE (SS1, 2)
                              REVL-TABLE (SS1, 3)
                   PERFORM VARYING SS2 FROM 1 BY 1 UNTIL SS2 > T-MAX
                       IF TOKEN-LEN(SS2) > 0
                           ADD +1 TO REVLSMAX (SS1)
                           MOVE REVLSMAX (SS1) TO SS3
                           INITIALIZE REVL-TABLE (SS1, SS3)
                           MOVE TOKEN(SS2) (1 : TOKEN-LEN(SS2))
                             TO REVLSTR (SS1, SS3)
                           MOVE TOKEN-LEN(SS2)
                             TO REVLLEN (SS1, SS3)
      D                    DISPLAY "REVL[" SS1 "," REVLSMAX (SS1) "]="
      D                        "(" REVLLEN (SS1, SS3) ")'"
      D                        REVLSTR (SS1, SS3)
      D                                (1 : REVLLEN (SS1, SS3)) "'"
                       END-IF
                   END-PERFORM
      D            DISPLAY REVLSMAX (SS1) " REVEALS IN DRAW " SS1
      D                " GAME " GAMENO
      D            PERFORM VARYING SS2 FROM 1 BY 1
      D                UNTIL SS2 > REVLSMAX (SS1)
      D                MOVE REVLLEN (SS1, SS2) TO SS3
      D                DISPLAY "BEFORE REVEAL SPLIT:(" SS3 ")'"
      D                        REVLSTR  (SS1, SS2) (1 : SS3) "'"
      D                    "," COLORVAL (SS1, SS2)
      D                   ",'" COLORNAM (SS1, SS2) "'"
      D            END-PERFORM
                   PERFORM VARYING SS2 FROM 1 BY 1
                       UNTIL SS2 > REVLSMAX (SS1)
                       MOVE REVLSTR (SS1, SS2)
                                    (2 : REVLLEN (SS1, SS2) - 1)
                         TO INPUT-STRING
                       IF INPUT-STRING (2 : 1) = SPACE
                           MOVE "," TO INPUT-STRING (2 : 1)
                       ELSE
                           MOVE "," TO INPUT-STRING (3 : 1)
                       END-IF
                       PERFORM TOKENIZE
                       IF TOKEN-LEN (1) > 0
      D                   DISPLAY "TOKEN(1)='"
      D                       TOKEN (1) (1 : TOKEN-LEN (1)) "'"
                          COMPUTE COLORVAL (SS1, SS2) =
                              FUNCTION INTEGER (
                              FUNCTION NUMVAL (
                              TOKEN (1) (1 : TOKEN-LEN (1)) ) )
                       END-IF
                       IF TOKEN-LEN (2) > 0
      D                   DISPLAY "TOKEN(2)='"
      D                       TOKEN (2) (1 : TOKEN-LEN (2)) "'"
                          MOVE FUNCTION UPPER-CASE (
                              TOKEN (2) (1 : TOKEN-LEN (2)) )
                            TO COLORNAM (SS1, SS2)
                       END-IF
      D                DISPLAY "REVL[" SS1 "," REVLSMAX (SS1) "]="
      D                    "(" REVLLEN (SS1, SS2) ")'"
      D                    REVLSTR (SS1, SS2)
      D                            (1 : REVLLEN (SS1, SS2)) "'"
                   END-PERFORM
      D            PERFORM VARYING SS2 FROM 1 BY 1
      D                UNTIL SS2 > REVLSMAX (SS1)
      D                MOVE REVLLEN (SS1, SS2) TO SS3
      D                DISPLAY "AFTER  REVEAL SPLIT:(" SS3 ")'"
      D                        REVLSTR  (SS1, SS2) (1 : SS3) "'"
      D                    "," COLORVAL (SS1, SS2)
      D                   ",'" COLORNAM (SS1, SS2) "'"
      D            END-PERFORM
               END-PERFORM
               SET GAME-POSSIBLE TO TRUE
               PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > DRAWSMAX
                   DISPLAY "ANALYSIS OF GAME " GAMENO " DRAW " SS1 " ='"
                       DRAWSTR (SS1) (1 : DRAWLEN (SS1)) "'"
                   PERFORM VARYING SS2 FROM 1 BY 1
                       UNTIL SS2 > REVLSMAX (SS1)
                       MOVE REVLLEN (SS1, SS2) TO SS3
                       DISPLAY "REVEAL[" SS1 "," SS2 "]='"
                           REVLSTR (SS1, SS2)
                               (1 : REVLLEN (SS1, SS2)) "'"
                           "," COLORVAL (SS1, SS2) ","
                           ",'" COLORNAM (SS1, SS2) "'"
                       EVALUATE TRUE ALSO TRUE
                           WHEN REVLRED   (SS1, SS2) ALSO
                                COLORVAL  (SS1, SS2) > 12
                                SET NOT-POSSIBLE TO TRUE
                           WHEN REVLGREEN (SS1, SS2) ALSO
                                COLORVAL  (SS1, SS2) > 13
                                SET NOT-POSSIBLE TO TRUE
                           WHEN REVLBLUE  (SS1, SS2) ALSO
                                COLORVAL  (SS1, SS2) > 14
                                SET NOT-POSSIBLE TO TRUE
                       END-EVALUATE
                   END-PERFORM
               END-PERFORM
               IF GAME-POSSIBLE
                   DISPLAY "GAME " GAMENO " POSSIBLE"
                   ADD GAMENO TO GAMESUM
               ELSE
                   DISPLAY "GAME " GAMENO " NOT POSSIBLE"
               END-IF
               READ INPUT-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD +1 TO LINECTR
DEBUG>D                DISPLAY "LINE " LINECTR " LEN=" LINELEN
               END-READ
           END-PERFORM
           DISPLAY "PART1=" GAMESUM
           DISPLAY " ".

       900-WRAP-UP.
           CLOSE INPUT-FILE.

       TOKENIZE.
      D    DISPLAY SPACE
      D    DISPLAY 'STRING:'
      D    DISPLAY INPUT-STRING (1 : 64)

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

      D    IF C = 1
      D        DISPLAY 'NO TOKENS'
      D    ELSE
      D        DISPLAY 'TOKENS:'
      D        PERFORM VARYING T FROM 1 BY 1 UNTIL T > T-MAX
      D            IF TOKEN-LEN(T) > 0
      D                DISPLAY T ': ' TOKEN-LEN(T) SPACE
      D                        TOKEN(T) (1 : TOKEN-LEN(T))
      D            ELSE
      D                DISPLAY T ': ' TOKEN-LEN(T)
      D            END-IF
      D        END-PERFORM
      D    END-IF
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