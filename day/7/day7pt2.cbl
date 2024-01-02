       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY7PT2.
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
           05  FILLER            REDEFINES LINEINPUT.
               10  INPT-HAND     PIC X(5).
               10  FILLER        PIC X.
               10  INPT-BID      PIC X(4).
               10  FILLER        PIC X(32746).

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
           05  MAPNDX            PIC  9(18) BINARY.
           05  WINRCNT           PIC  9(18) BINARY.
           05  WINRLEN           PIC  9(18) BINARY.
           05  JOKRCNT           PIC  9(18) BINARY.
           05  GAMELEN           PIC  9(18) BINARY.
           05  PART1-VALUE       PIC  9(18) VALUE ZEROES.
           05  PART2-VALUE       PIC  9(18) VALUE ZEROES.
           05  PART-NO           PIC  9(1)  VALUE ZEROES.
           05  GOT-CARD-FLAG     PIC  X.
               88  NOT-GOT-CARD            VALUE 'N'.
               88  GOT-CARD                VALUE 'Y'.
           05  NAME-STRING       PIC  X(256).
           05  CARD-STRINGS.
               10  FILLER        PIC  X(13) VALUE '23456789TJQKA'.
               10  FILLER        PIC  X(13) VALUE 'J23456789TQKA'.
           05  CARD-STRING-TABLE REDEFINES CARD-STRINGS
                                 OCCURS 2 TIMES.
               10  CARD-STRING   PIC  X(13).

       01  HAND-TABLE.
           05  HANDMAX               PIC  9(9) BINARY VALUE 0.
           05  HAND-AREA.
               10  HAND-DATA         OCCURS 1 TO 2000 TIMES
                                     DEPENDING ON HANDMAX.
                   15  HAND-KEY.
                       20  HANDRANK      PIC  99.
                       20  CARD-STRENGTH OCCURS 5 TIMES.
                           25  CARDSTREN PIC  99.
                   15  HANDBID           PIC  9(4).
                   15  HAND              PIC  X(05).

       01  TEMP-HAND.
                   15  TEMPRANK      PIC  99.
                   15  TEMP-STRENGTH OCCURS 5 TIMES.
                       20  TEMPSTREN PIC  99.
                   15  TEMPBID       PIC  9(4).
                   15  TEMP          PIC  X(05).

       01  CARD-COUNT-TABLE.
           05  CDCTMAX               PIC  9(5) BINARY VALUE 0.
           05  CDCT-AREA.
               10  CDMAXCNT          PIC  9(5) BINARY VALUE 0.
               10  CDCT-DATA         OCCURS 1 TO 5 TIMES
                                     DEPENDING ON CDCTMAX.
                   15  CARDVAL       PIC  X.
                   15  CARDCNT       PIC  9(5) BINARY.

       LINKAGE SECTION.
       01  PARM-AREA.
           05  PARM-LEN              PIC S9(4) BINARY.
           05  PARM-PART             PIC  9.

       PROCEDURE DIVISION USING PARM-AREA.
           PERFORM 000-HOUSEKEEPING
           PERFORM 100-PROCESS-INPUT-DATA
      *    PERFORM 200-TIME-SECTIONS
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
DEBUG>D            DISPLAY "LINE " LINECTR " LEN=" LINELEN
           END-READ.

       100-PROCESS-INPUT-DATA.
           MOVE  0 TO HANDMAX
           PERFORM UNTIL END-OF-FILE
      *        PARSE INPUT INTO HAND-TABLE
               ADD 1 TO HANDMAX
      *        COPY HAND STRING AND BID AMOUNT
               MOVE INPT-HAND TO HAND (HANDMAX)
               COMPUTE SS1 = LINELEN - 6
               COMPUTE HANDBID (HANDMAX) =
                   FUNCTION NUMVAL (INPT-BID ( 1 : SS1))
      *        INITIALIZE CARD COUNT DATA
               MOVE 0 TO CDCTMAX
               MOVE 0 TO CDMAXCNT
               PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > 5
                   MOVE SPACE TO CARDVAL (SS1)
                   MOVE 0     TO CARDCNT (SS1)
               END-PERFORM
      *        PARSE DATA FOR EACH CARD IN THE HAND
               MOVE 0 TO JOKRCNT
               PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > 5
      *            FIND THE STRENGTH OF THE CARD
                   PERFORM VARYING SS2 FROM 1 BY 1 UNTIL SS2 > 13
                       IF INPT-HAND (SS1 : 1) =
                          CARD-STRING (PART-NO) (SS2 : 1)
                           MOVE SS2 TO CARDSTREN (HANDMAX, SS1)
                       END-IF
                   END-PERFORM
      *            COUNT JOKERS SEPARATELY
                   IF INPT-HAND (SS1 : 1) = 'J'
                       ADD 1 TO JOKRCNT
                   ELSE
      *                SEE IF WE ALREADY HAVE THIS CARD COUNTED
                       SET NOT-GOT-CARD TO TRUE
                       PERFORM VARYING SS2 FROM 1 BY 1
                           UNTIL SS2 > CDCTMAX OR GOT-CARD
                           IF INPT-HAND (SS1 : 1) = CARDVAL (SS2)
      *                        WE ALREADY HAVE THIS CARD, ADD TO COUNT
                               ADD 1 TO CARDCNT (SS2)
                               SET GOT-CARD TO TRUE
                           END-IF
                       END-PERFORM
                       IF NOT-GOT-CARD
      *                   WE DO NOT HAVE THIS CARD, ADD TO COUNT TABLE
                          ADD 1                    TO CDCTMAX
                          MOVE INPT-HAND (SS1 : 1) TO CARDVAL (CDCTMAX)
                          MOVE 1                   TO CARDCNT (CDCTMAX)
                       END-IF
                   END-IF
               END-PERFORM
      *        FIND THE MAXIMUM COUNT VALUE
               PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > CDCTMAX
                   IF CARDCNT (SS1) > CDMAXCNT
                       MOVE CARDCNT (SS1) TO CDMAXCNT
                   END-IF
               END-PERFORM
      *        COMPUTE HAND RANK FROM MAX CARD COUNT AND COUNT OF CARDS
      *        INCLUDE JOKER COUNT FOR HIGHEST-COUNT CARD
               IF JOKRCNT < 5
                   COMPUTE HANDRANK (HANDMAX) =
                       CDMAXCNT + JOKRCNT - CDCTMAX + 4
               ELSE
                   MOVE 08 TO HANDRANK (HANDMAX)
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
      D    DISPLAY "INPUT  HANDS: " HANDMAX " ENTRIES:"
      D    PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > HANDMAX
      D        DISPLAY "HAND[" SS1 "],RANK=" HANDRANK (SS1)
      D            ',STRENGTHS=' CARDSTREN (SS1, 1)
      D                      ',' CARDSTREN (SS1, 2)
      D                      ',' CARDSTREN (SS1, 3)
      D                      ',' CARDSTREN (SS1, 4)
      D                      ',' CARDSTREN (SS1, 5)
      D            ',BID=' HANDBID (SS1)
      D            ',HAND='   HAND (SS1)
      D            ',DATA=' HAND-DATA (SS1)
      D    END-PERFORM
      *    SORT THE HANDS BY RANK, CARD STRENGTHS, AND BID
           MOVE HANDMAX TO ITEMCOUNT
           PERFORM SORT-HANDS
      D    DISPLAY "SORTED HANDS: " HANDMAX " ENTRIES:"
      D    PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > HANDMAX
      D        DISPLAY "HAND[" SS1 "]"
      D            ",RANK=" HANDRANK (SS1)
      D            ",STRENGTHS=" CARDSTREN (SS1, 1)
      D                      "," CARDSTREN (SS1, 2)
      D                      "," CARDSTREN (SS1, 3)
      D                      "," CARDSTREN (SS1, 4)
      D                      "," CARDSTREN (SS1, 5)
      D            ",BID="       HANDBID   (SS1)
      D            ",HAND="      HAND      (SS1)
      D            ",DATA="      HAND-DATA (SS1)
      D    END-PERFORM
      *    COMPUTE THE TOTAL WINNINGS += RANK * BID
      D    DISPLAY "CALC WINNINGS " HANDMAX " ENTRIES:"
           PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > HANDMAX
               COMPUTE SS2 = SS1 * HANDBID (SS1)
               COMPUTE PART1-VALUE = PART1-VALUE + SS2
      D        DISPLAY "HAND[" SS1 "],RANK=" SS1 "*" HANDBID (SS1)
      D            ",WINNINGS=" SS2
           END-PERFORM
           DISPLAY "PART1=" PART1-VALUE
           DISPLAY " ".

      *200-TIME-SECTIONS.
      *    MOVE 1 TO PART1-VALUE
      *    PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > TIMEMAX
      *        MOVE 0 TO TIMECOUNT
      *        PERFORM VARYING SS2 FROM 1 BY 1
      *            UNTIL SS2 > TIMEBEG (SS1)
      *            COMPUTE SS3 = TIMEBEG (SS1) - SS2
      *            COMPUTE SS4 = SS3 * SS2
      *D           DISPLAY "S1=" TIMEBEG (SS1) ",S2=" SS2
      *D               ",(S1 - S2)=" SS3
      *D               ",(S1 - S2) * S2=" SS4
      *D               ",DIST=" DISTBEG (SS1)
      *            IF SS4 > DISTBEG (SS1)
      *                ADD 1 TO TIMECOUNT
      *            END-IF
      *        END-PERFORM
      *D       DISPLAY "FOR TIME=" TIMEBEG (SS1) ",COUNT=" TIMECOUNT
      *        COMPUTE PART1-VALUE = PART1-VALUE * TIMECOUNT
      *    END-PERFORM
      *    DISPLAY "PART1=" PART1-VALUE
      *    DISPLAY " ".

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

       SORT-HANDS.
           PERFORM WITH TEST AFTER UNTIL HASNOTCHANGED
              SET HASNOTCHANGED TO TRUE
              SUBTRACT 1 FROM ITEMCOUNT
              PERFORM VARYING ITEMINDEX FROM 1 BY 1
                 UNTIL ITEMINDEX > ITEMCOUNT
                 IF HAND-KEY (ITEMINDEX) > HAND-KEY (ITEMINDEX + 1)
                    MOVE HAND-DATA (ITEMINDEX) TO TEMP-HAND
                    MOVE HAND-DATA (ITEMINDEX + 1)
                      TO HAND-DATA (ITEMINDEX)
                    MOVE TEMP-HAND TO HAND-DATA (ITEMINDEX + 1)
                    SET HASCHANGED TO TRUE
                 END-IF
              END-PERFORM
           END-PERFORM
      D    DISPLAY "HANDS SORTED AT " ITEMCOUNT " ENTRIES"
           .
