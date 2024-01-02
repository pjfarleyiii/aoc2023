       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY6PT1.
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
           05  LINEINPUT  PIC X(32756).

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
           05  TIMECOUNT         PIC S9(18) BINARY.
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
           05  TIMELEN           PIC  9(18) BINARY.
           05  DISTLEN           PIC  9(18) BINARY.
           05  PART1-VALUE       PIC  9(18) VALUE ZEROES.
           05  PART2-VALUE       PIC  9(18) VALUE ZEROES.
           05  NAME-STRING       PIC  X(256).
           05  TIME-STRING       PIC  X(256).
           05  DIST-STRING       PIC  X(256).

       01  RACE-TIMES.
           05  TIMEMAX           PIC  9(9) BINARY VALUE 0.
           05  TIME-AREA.
               10  TIME-RANGE        OCCURS 1 TO 256 TIMES
                                     DEPENDING ON TIMEMAX.
                   15  TIMEBEG       PIC  9(18) BINARY.
                   15  TIMEEND       PIC  9(18) BINARY.

       01  RACE-DIST.
           05  DISTMAX           PIC  9(9) BINARY VALUE 0.
           05  DIST-AREA.
               10  DIST-RANGE        OCCURS 1 TO 256 TIMES
                                     DEPENDING ON DISTMAX.
                   15  DISTBEG       PIC  9(18) BINARY.
                   15  DISTEND       PIC  9(18) BINARY.

       01 TEMP-TIME.
                   15  TMPTIMEBEG    PIC  9(18) BINARY.
                   15  TMPTIMEEND    PIC  9(18) BINARY.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING
           PERFORM 100-PROCESS-INPUT-DATA
           PERFORM 200-TIME-SECTIONS
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
           MOVE  0 TO TIMELEN
           MOVE  0 TO DISTLEN
           PERFORM UNTIL END-OF-FILE
      *        CHECK FOR TABLE TYPE
               MOVE ":" TO SEPARATOR-CHAR
               MOVE LINEINPUT (1 : LINELEN) TO INPUT-STRING
               MOVE FUNCTION UPPER-CASE (INPUT-STRING) TO INPUT-STRING
               PERFORM TOKENIZE
               IF TOKEN-LEN (2) > 0
                   MOVE TOKEN (1) (1 : TOKEN-LEN (1)) TO NAME-STRING
      *            CONCATENATE TOKENS INTO SINGLE VALUE
      *            CHANGE SPACES IN INPUT TO "|" FOR TOKENIZE TO PROCESS
                   MOVE TOKEN (2)    (2 : TOKEN-LEN(2) - 1)
                     TO INPUT-STRING
                   INSPECT INPUT-STRING (1 : TOKEN-LEN (2))
                       REPLACING ALL SPACE BY "|"
      D            DISPLAY INPUT-STRING (1 : LINELEN)
                   MOVE "|" TO SEPARATOR-CHAR
                   PERFORM TOKENIZE
      D            PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > T-MAX
      D                DISPLAY "TOKEN[" SS1 "=L'" TOKEN-LEN (SS1) "='"
      D                    TOKEN (SS1) (1 : TOKEN-LEN (SS1)) "'"
      D            END-PERFORM
                   PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > T-MAX
                       IF TOKEN-LEN (SS1) > 0
                           IF NAME-STRING (1 : 4) = "TIME"
                               MOVE TOKEN (SS1) (1 : TOKEN-LEN (SS1))
                                 TO TIME-STRING (TIMELEN + 1 :
                                                 TOKEN-LEN (SS1))
                               ADD TOKEN-LEN (SS1) TO TIMELEN
                           ELSE
                               MOVE TOKEN (SS1) (1 : TOKEN-LEN (SS1))
                                 TO DIST-STRING (DISTLEN + 1 :
                                                 TOKEN-LEN (SS1))
                               ADD TOKEN-LEN (SS1) TO DISTLEN
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
               IF NOT END-OF-FILE
                   READ INPUT-FILE
                       AT END SET END-OF-FILE TO TRUE
                       NOT AT END
                           ADD +1 TO LINECTR
DEBUG>D                    DISPLAY "LINE " LINECTR " LEN=" LINELEN
                   END-READ
               END-IF
           END-PERFORM
      D    DISPLAY "TIME:[" TIMELEN "],VALUE:"
      D        TIME-STRING ( 1 : TIMELEN)
      D    DISPLAY "DISTANCE:[" DISTLEN "],VALUE:"
      D        DIST-STRING ( 1 : DISTLEN)
           COMPUTE TIMEBEG (1) =
               FUNCTION NUMVAL (TIME-STRING ( 1 : TIMELEN))
           COMPUTE DISTBEG (1) =
               FUNCTION NUMVAL (DIST-STRING ( 1 : DISTLEN))
           MOVE 1 TO TIMEMAX DISTMAX
           DISPLAY " ".

       200-TIME-SECTIONS.
           MOVE 1 TO PART2-VALUE
           PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > TIMEMAX
               MOVE 0 TO TIMECOUNT
               PERFORM VARYING SS2 FROM 1 BY 1
                   UNTIL SS2 > TIMEBEG (SS1)
      D            IF FUNCTION MOD (SS2, 100000) = 0
      D                DISPLAY "TRIED " SS2 " TIME VALUES"
      D            END-IF
                   COMPUTE SS3 = TIMEBEG (SS1) - SS2
                   COMPUTE SS4 = SS3 * SS2
                   IF SS4 > DISTBEG (SS1)
                       ADD 1 TO TIMECOUNT
      D                IF FUNCTION MOD (SS2, 100000) = 0
      D                    DISPLAY "S1=" TIMEBEG (SS1) ",S2=" SS2
      D                        ",(S1 - S2)=" SS3
      D                        ",(S1 - S2) * S2=" SS4
      D                        ",DIST=" DISTBEG (SS1)
      D                        ",CNT=" TIMECOUNT
      D                END-IF
                   END-IF
               END-PERFORM
      D        DISPLAY "FOR TIME=" TIMEBEG (SS1) ",COUNT=" TIMECOUNT
               COMPUTE PART2-VALUE = PART2-VALUE * TIMECOUNT
           END-PERFORM
           DISPLAY "PART2=" PART2-VALUE
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

       SORT-TIMES.
           PERFORM WITH TEST AFTER UNTIL HASNOTCHANGED
              SET HASNOTCHANGED TO TRUE
              SUBTRACT 1 FROM ITEMCOUNT
              PERFORM VARYING ITEMINDEX FROM 1 BY 1
                 UNTIL ITEMINDEX > ITEMCOUNT
                 IF TIMEBEG (ITEMINDEX) > TIMEBEG (ITEMINDEX + 1)
                    MOVE TIME-RANGE (ITEMINDEX) TO TEMP-TIME
                    MOVE TIME-RANGE (ITEMINDEX + 1)
                      TO TIME-RANGE (ITEMINDEX)
                    MOVE TEMP-TIME TO TIME-RANGE (ITEMINDEX + 1)
                    SET HASCHANGED TO TRUE
                 END-IF
              END-PERFORM
           END-PERFORM
           .
