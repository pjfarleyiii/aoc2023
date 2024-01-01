       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY4PT1.
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
           05  CARDNO            PIC  9(8) BINARY.
           05  WINRCNT           PIC  9(8) BINARY.
           05  WINRLEN           PIC  9(8) BINARY.
           05  GAMECNT           PIC  9(8) BINARY.
           05  GAMELEN           PIC  9(8) BINARY.
           05  PART1-VALUE       PIC  9(8) VALUE ZEROES.
           05  PART2-VALUE       PIC  9(8) VALUE ZEROES.
           05  WINR-STRING       PIC  X(256).
           05  GAME-STRING       PIC  X(256).

       01  CARD-STACK-AREA.
           05  CARDSTACK         OCCURS 2560 TIMES
                                 PIC  9(8) BINARY VALUE 1.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING
           PERFORM 100-PROCESS-INPUT-DATA
           PERFORM 900-WRAP-UP
           GOBACK.

       000-HOUSEKEEPING.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE
               AT END SET END-OF-FILE TO TRUE
               NOT AT END
                   ADD +1 TO LINECTR
DEBUG>D            DISPLAY "LINE " LINECTR " LEN=" LINELEN
           END-READ.

       100-PROCESS-INPUT-DATA.
           PERFORM UNTIL END-OF-FILE
      D        DISPLAY "CARD='" LINEINPUT (1 : LINELEN) "'"
               MOVE ":" TO SEPARATOR-CHAR
               MOVE LINEINPUT (1 : LINELEN) TO INPUT-STRING
               PERFORM TOKENIZE
               COMPUTE CARDNO =
                   FUNCTION NUMVAL (TOKEN (1) (5 : TOKEN-LEN (1) - 4))
               MOVE "|" TO SEPARATOR-CHAR
               COMPUTE SS1 = TOKEN-LEN (1) + 2
               COMPUTE SS2 = LINELEN - SS1 + 1
               MOVE LINEINPUT (SS1 : SS2) TO INPUT-STRING
      D        DISPLAY "SS1=" SS1 ",SS2=" SS2
      D            ",INSTR='" INPUT-STRING (1 : SS2) "'"
               PERFORM TOKENIZE
               MOVE TOKEN (1) (1 : TOKEN-LEN (1)) TO WINR-STRING
               MOVE TOKEN (2) (1 : TOKEN-LEN (2)) TO GAME-STRING
               COMPUTE WINRLEN = TOKEN-LEN (1) - 1
               COMPUTE GAMELEN = TOKEN-LEN (2)
      D        DISPLAY "WLEN=" WINRLEN ",WSTR='"
      D            WINR-STRING (1 : WINRLEN) "'"
      D        DISPLAY "GLEN=" GAMELEN ",GSTR='"
      D            GAME-STRING (1 : GAMELEN) "'"
               MOVE +0 TO WINRCNT
               PERFORM VARYING SS1 FROM 1 BY 3 UNTIL SS1 > GAMELEN
      D            COMPUTE SS3 = FUNCTION NUMVAL (GAME-STRING (SS1 : 3))
      D            DISPLAY "GAME " SS1 "='" GAME-STRING (SS1 : 3) "'="
      D                SS3
                   PERFORM VARYING SS2 FROM 1 BY 3 UNTIL SS2 > WINRLEN
      *                COMPUTE SS4 =
      *                    FUNCTION NUMVAL (WINR-STRING (SS2 : 3))
      *                DISPLAY "WINR " SS1 "='" WINR-STRING (SS2 : 3)
      *                    "'=" SS4
                       IF FUNCTION NUMVAL (GAME-STRING (SS1 : 3)) =
                          FUNCTION NUMVAL (WINR-STRING (SS2 : 3))
                           ADD +1 TO WINRCNT
                       END-IF
                   END-PERFORM
               END-PERFORM
      D        DISPLAY "WINRCNT=" WINRCNT
               IF WINRCNT > +0
      *            PART 1 CALCULATIONS
                   COMPUTE GAMECNT =
                       FUNCTION INTEGER(2 ** (WINRCNT - 1))
                   ADD GAMECNT TO PART1-VALUE
      D            DISPLAY "CARD " CARDNO " GAMECNT=" GAMECNT
      D                " PART1=" PART1-VALUE
      *            PART 2 CALCULATIONS
                   PERFORM VARYING SS1 FROM +0 BY +1
                       UNTIL SS1 > WINRCNT - 1
                       COMPUTE SS2 = CARDNO + SS1 + 1
      D                DISPLAY "FRNDX " CARDNO " TONDX=" SS2
      D                    " ADD=" CARDSTACK (CARDNO)
      D                    " TO=" CARDSTACK (SS2)
                       ADD CARDSTACK (CARDNO) TO CARDSTACK (SS2)
                   END-PERFORM
               END-IF
               READ INPUT-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD +1 TO LINECTR
DEBUG>D                DISPLAY "LINE " LINECTR " LEN=" LINELEN
               END-READ
           END-PERFORM
           DISPLAY "PART1=" PART1-VALUE
           DISPLAY " ".

       900-WRAP-UP.
           CLOSE INPUT-FILE.
           PERFORM VARYING SS1 FROM +1 BY +1
               UNTIL SS1 > CARDNO
      D        DISPLAY "STACK " SS1 "=" CARDSTACK (SS1)
               ADD CARDSTACK (SS1) TO PART2-VALUE
           END-PERFORM
           DISPLAY "PART2=" PART2-VALUE
           DISPLAY " ".

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