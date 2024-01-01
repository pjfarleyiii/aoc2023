       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY3PT1.
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
           05  LINESTART         PIC S9(8) BINARY VALUE +1.
           05  LINEEND           PIC S9(8) BINARY VALUE +99999999.
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
       01  T-LIM PIC 999 VALUE 256.
       01  TOKEN-TABLE.
           03  TOKEN-ENTRY OCCURS 256.
               05  TOKEN-LEN PIC 999.
               05  TOKEN PIC X(256).

       01  L PIC 999.
       01  L-LIM PIC 999 VALUE 256.

       01  ERROR-FOUND PIC X.

      * SORTING VARIABLES
       01  TEMPITEM              PIC  9(4).
       01  ITEMINDEX             PIC  9(8) BINARY.
       01  ITEMCOUNT             PIC  9(8) BINARY.
       01  CHANGED-FLAG          PIC  X VALUE "N".
           88 HASCHANGED         VALUE "Y".
           88 HASNOTCHANGED      VALUE "N".

       01  GAME-VARIABLES.
           05  PART1-VALUE       PIC  9(8) VALUE ZEROES.
           05  PART2-VALUE       PIC  9(8) VALUE ZEROES.

       01  MAP-ARRAY-TABLE.
           05  MAPMAX            PIC  9(8) BINARY.
           05  MAP-AREA.
               10  MAPLINE       OCCURS 1 TO 2560 TIMES
                                 DEPENDING ON MAPMAX.
                   15  MAPCHAR   OCCURS 256 TIMES
                                 PIC X.

       01  MAP-SEARCH-TABLE.
           05  SRCHMAX           PIC  9(8) BINARY.
           05  SRCHTOKEN         PIC X(256).
           05  SRCHAREA.
               10  MAPSRCH       OCCURS 1 TO 256 TIMES
                                 DEPENDING ON SRCHMAX
                                 INDEXED BY CHARNDX.
                   15  FILLER    PIC X.

       01  PARTS-TABLE.
           05  PARTMAX           PIC  9(8) BINARY VALUE 0.
           05  PART-AREA.
               10  PART-TABLE    OCCURS 128 TIMES.
                   15  PARTLEN   PIC  9.
                   15  PARTLOC   PIC  9(4).
                   15  PARTNO    PIC  X(4).
                   15  PARTVALSW PIC  X.
                       88  PARTVALID VALUE "Y".

       01  VALID-PARTS-TABLE.
           05  VALPARTMAX        PIC  9(8) BINARY VALUE 0.
           05  VAL-PART-AREA.
               10  VALPART-TABLE OCCURS 1 TO 2560 TIMES
                                 DEPENDING ON VALPARTMAX
                                 INDEXED BY VALNDX.
                   15  VALPARTNO    PIC  9(4).

       LINKAGE SECTION.
       01  PARM-AREA.
           05  PARM-LEN          PIC S9(4) BINARY.
           05  PARM-START        PIC  9(4).
           05  FILLER            PIC  X.
           05  PARM-END          PIC  9(4).

       PROCEDURE DIVISION USING PARM-AREA.
           PERFORM 000-HOUSEKEEPING
           PERFORM 100-PROCESS-INPUT-DATA
           PERFORM 200-PROCESS-MAP-DATA
           PERFORM 900-WRAP-UP
           GOBACK.

       000-HOUSEKEEPING.
           IF PARM-LEN = +9
               IF PARM-START IS NUMERIC
      D            DISPLAY "LINE START=" PARM-START
                   COMPUTE LINESTART = PARM-START
               END-IF
               IF PARM-END   IS NUMERIC
      D            DISPLAY "LINE END  =" PARM-END
                   COMPUTE LINEEND   = PARM-END
               END-IF
           END-IF
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE
               AT END SET END-OF-FILE TO TRUE
               NOT AT END
                   ADD +1 TO LINECTR
DEBUG>*            DISPLAY "LINEINP " LINECTR " LEN=" LINELEN
           END-READ
      *    INITIALIZE MAP AREA
           MOVE 2560 TO MAPMAX
           MOVE SPACES TO MAP-AREA
      *    ESTABLISH ALL PERIOD LINE BEFORE FIRST ACTUAL LINE.
           MOVE +1 TO MAPMAX
           PERFORM VARYING SS2 FROM 1 BY 1
               UNTIL SS2 > LINELEN + 2
               MOVE "." TO MAPCHAR (MAPMAX, SS2)
           END-PERFORM
DEBUG>D    DISPLAY "LINELEN=" LINELEN
DEBUG>D    DISPLAY "MAPLINE " MAPMAX "='"
DEBUG>D            MAPLINE (MAPMAX) (1 : LINELEN + 2) "'"
           CONTINUE.

       100-PROCESS-INPUT-DATA.
           PERFORM UNTIL END-OF-FILE
               ADD +1 TO MAPMAX
DEBUG>D        DISPLAY "LINECTR " LINECTR
DEBUG>D                " MAPMAX " MAPMAX " LINESTART=" LINESTART
               IF LINECTR < LINESTART
                   SUBTRACT +1 FROM MAPMAX
                   READ INPUT-FILE
                       AT END
                           SET END-OF-FILE TO TRUE
                       NOT AT END
                           ADD +1 TO LINECTR
DEBUG>D                    DISPLAY "LINEINP " LINECTR " LEN=" LINELEN
                   END-READ
                   EXIT PERFORM CYCLE
               ELSE
                   MOVE "." TO MAPCHAR (MAPMAX, 1)
                   MOVE LINEINPUT (1 : LINELEN)
                     TO MAPLINE (MAPMAX) (2 : LINELEN)
                   MOVE "." TO MAPCHAR (MAPMAX, LINELEN + 2)
DEBUG>D            DISPLAY "MAPLINE " MAPMAX "='"
DEBUG>D                    MAPLINE (MAPMAX) (1 : LINELEN + 2) "'"
               END-IF
DEBUG>D        DISPLAY "LINECTR " LINECTR
DEBUG>D                " MAPMAX " MAPMAX " LINEEND  =" LINEEND
               IF LINECTR > LINEEND
                   SUBTRACT +1 FROM MAPMAX
                   SET END-OF-FILE TO TRUE
               ELSE
                   READ INPUT-FILE
                       AT END
                           SET END-OF-FILE TO TRUE
                       NOT AT END
                           ADD +1 TO LINECTR
DEBUG>D                    DISPLAY "LINEINP " LINECTR " LEN=" LINELEN
                   END-READ
               END-IF
               IF END-OF-FILE
      *            ESTABLISH ALL PERIOD LINE AFTER LAST ACTUAL LINE.
                   ADD +1 TO MAPMAX
                   PERFORM VARYING SS2 FROM 1 BY 1
                       UNTIL SS2 > LINELEN + 2
                       MOVE "." TO MAPCHAR (MAPMAX, SS2)
                   END-PERFORM
DEBUG>D            DISPLAY "MAPLINE " MAPMAX "='"
DEBUG>D                    MAPLINE (MAPMAX) (1 : LINELEN + 2) "'"
               END-IF
           END-PERFORM
           DISPLAY " ".

       200-PROCESS-MAP-DATA.
           MOVE 2560 TO VALPARTMAX
           MOVE SPACES TO VAL-PART-AREA
           MOVE 0 TO VALPARTMAX
           PERFORM VARYING SS1 FROM 2 BY 1 UNTIL SS1 > MAPMAX - 1
               MOVE "." TO SEPARATOR-CHAR
               MOVE MAPLINE (SS1) (1 : LINELEN + 2)
                 TO INPUT-STRING
      *        CLEAR OUT ANY SYMBOLS IN THE LINE TO BE TOKENIZED
               PERFORM VARYING SS2 FROM 1 BY 1 UNTIL SS2 > LINELEN + 2
                   IF INPUT-STRING (SS2 : 1) NOT = "."   AND
                      INPUT-STRING (SS2 : 1) NOT NUMERIC
                       MOVE "." TO INPUT-STRING (SS2 : 1)
                   END-IF
               END-PERFORM
      D        DISPLAY " "
      D        DISPLAY "LINES :"
      D        DISPLAY MAPLINE (SS1 - 1) (1 : LINELEN + 2)
      D        DISPLAY MAPLINE (SS1)     (1 : LINELEN + 2)
      D        DISPLAY MAPLINE (SS1 + 1) (1 : LINELEN + 2)
               PERFORM TOKENIZE
               INITIALIZE PART-AREA
               MOVE 0 TO PARTMAX
               PERFORM VARYING SS2 FROM 1 BY 1 UNTIL SS2 > T-MAX
                   IF TOKEN-LEN(SS2) > 0
      *                ADD PART TO PART TABLE
                       ADD +1 TO PARTMAX
                       MOVE SPACES TO PARTNO (PARTMAX)
                       MOVE TOKEN(SS2) (1 : TOKEN-LEN(SS2))
                         TO PARTNO (PARTMAX) (1 : TOKEN-LEN(SS2))
                       MOVE TOKEN-LEN(SS2) TO PARTLEN (PARTMAX)
      *                SET PARTLOC TO POSITION IN LINE
                       SET CHARNDX TO 1
                       MOVE +1 TO SS3
                       COMPUTE SRCHMAX = LINELEN + 2
                       MOVE INPUT-STRING (1 : SRCHMAX) TO SRCHAREA
                       MOVE "." TO SRCHTOKEN (1 : 1)
                                   SRCHTOKEN (TOKEN-LEN(SS2) + 2 : 1)
                       MOVE TOKEN(SS2) (1 : TOKEN-LEN(SS2))
                         TO SRCHTOKEN (2 : TOKEN-LEN(SS2))
      D                DISPLAY "SRCHTOKEN='"
      D                    SRCHTOKEN (1 : TOKEN-LEN(SS2) + 2) "'"
                       SEARCH MAPSRCH VARYING SS3
                           AT END
                               MOVE ZEROES TO PARTLOC (PARTMAX)
                           WHEN SRCHAREA
                                         (SS3 : TOKEN-LEN(SS2) + 2) =
                                SRCHTOKEN  (1 : TOKEN-LEN(SS2) + 2)
      *                        SET PARTLOC (PARTMAX) TO POS IN LINE
                               COMPUTE PARTLOC (PARTMAX) = SS3 + 1
                       END-SEARCH
                       MOVE SPACE TO PARTVALSW (PARTMAX)
      *                CHECK PART FOR ADJACENCY TO A SYMBOL ON THIS LINE
      D                COMPUTE SS3 = PARTLOC (PARTMAX) - 1
      D                DISPLAY "PART '"
      D                   PARTNO (PARTMAX) (1 : TOKEN-LEN(SS2)) "' "
      D                   "PARTLOC[" PARTMAX "] - 1 = " SS3 " ='"
      D                   MAPLINE (SS1)
      D                           (PARTLOC (PARTMAX) - 1 : 1) "'"
      D                COMPUTE SS3 = PARTLOC (PARTMAX) +
      D                              PARTLEN (PARTMAX)
      D                DISPLAY "PART '"
      D                   PARTNO (PARTMAX) (1 : TOKEN-LEN(SS2)) "' "
      D                        "PARTLOC[" PARTMAX "] + "
      D                        "PARTLEN[" PARTMAX "] = " SS3 " ='"
      D                   MAPLINE (SS1)
      D                           (PARTLOC (PARTMAX) +
      D                            PARTLEN (PARTMAX) : 1) "'"
                       IF MAPLINE (SS1)
                                  (PARTLOC (PARTMAX) - 1 : 1)
                          NOT = "."
                               SET PARTVALID (PARTMAX) TO TRUE
                       END-IF
                       IF NOT PARTVALID (PARTMAX) AND
                          MAPLINE (SS1)
                                  (PARTLOC (PARTMAX) +
                                   PARTLEN (PARTMAX) : 1)
                          NOT = "."
                               SET PARTVALID (PARTMAX) TO TRUE
                       END-IF
      *                DISPLAY "PARTNO[" PARTMAX "] @ "
      *                    PARTLOC (PARTMAX) " = '" PARTNO (PARTMAX)
      *                    "' FOR LEN " PARTLEN (PARTMAX)
      *                    " VALID='" PARTVALSW (PARTMAX) "'"
                   END-IF
               END-PERFORM
               IF PARTMAX = 0
      D            DISPLAY "NO PARTS IN MAPLINE " SS1
                   CONTINUE
               ELSE
      *            CHECK EACH PART FOR ADJACENCY TO A SYMBOL ON THE
      *            PREVIOUS OR NEXT LINE
                   PERFORM VARYING SS2 FROM 1 BY 1 UNTIL SS2 > PARTMAX
                       IF NOT PARTVALID (SS2)
                           COMPUTE SRCHMAX = PARTLEN (SS2) + 2
                           MOVE MAPLINE (SS1 - 1)
                                (PARTLOC (SS2) - 1 : PARTLEN (SS2) + 2)
                             TO SRCHAREA (1 : SRCHMAX)
      D                    DISPLAY "PART '" PARTNO (SS2) "' "
      D                       "SRCHAREA(-1)='" SRCHAREA "'"
                           SET CHARNDX TO 1
                           MOVE +1 TO SS3
                           SEARCH MAPSRCH VARYING SS3
                               AT END
                                   CONTINUE
                               WHEN MAPSRCH (SS3) NOT = "." AND
                                    MAPSRCH (SS3) NOT NUMERIC
                                   SET PARTVALID (SS2) TO TRUE
                           END-SEARCH
                       END-IF
                       IF NOT PARTVALID (SS2)
                           COMPUTE SRCHMAX = PARTLEN (SS2) + 2
                           MOVE MAPLINE (SS1 + 1)
                                (PARTLOC (SS2) - 1 : PARTLEN (SS2) + 2)
                             TO SRCHAREA (1 : SRCHMAX)
      D                    DISPLAY "PART '" PARTNO (SS2) "' "
      D                       "SRCHAREA(+1)='" SRCHAREA "'"
                           SET CHARNDX TO 1
                           MOVE +1 TO SS3
                           SEARCH MAPSRCH VARYING SS3
                               AT END
                                   CONTINUE
                               WHEN MAPSRCH (SS3) NOT = "." AND
                                    MAPSRCH (SS3) NOT NUMERIC
                                   SET PARTVALID (SS2) TO TRUE
                           END-SEARCH
                       END-IF
      *                IF PARTVALID (SS2)
      **                   DUPLICATE NUMBERS ARE NOT VALID
      *                    SET VALNDX TO 1
      *                    MOVE +1 TO SS3
      *                    SEARCH VALPART-TABLE VARYING SS3
      *                        AT END
      *                            CONTINUE
      *                        WHEN VALPARTNO (SS3) = TEMPITEM
      *                             FUNCTION NUMVAL ( PARTNO (SS2) )
      *                             MOVE SPACE TO PARTVALSW (SS2)
      *                             DISPLAY "DUPLICATE PART "
      *                                     VALPARTNO (SS3)
      *                                     " NOT STORED"
      *                    END-SEARCH
      *                END-IF
                       IF PARTVALID (SS2)
                           ADD +1 TO VALPARTMAX
                           COMPUTE VALPARTNO (VALPARTMAX) =
                               FUNCTION NUMVAL ( PARTNO (SS2) )
      D                    DISPLAY "PARTNO[" SS2 "] @ "
      D                        PARTLOC (SS2) " = '" PARTNO (SS2)
      D                        "' FOR LEN " PARTLEN (SS2)
      D                        " VALID='" PARTVALSW (SS2) "'"
                       END-IF
                   END-PERFORM
                   PERFORM SORT-VALID-PARTS
      D            DISPLAY " "
      D            DISPLAY "VALID PARTS: " VALPARTMAX
      D            PERFORM VARYING SS2 FROM +1 BY +24
      D                UNTIL SS2 > VALPARTMAX
      D                DISPLAY VALPARTNO (SS2)
      D                    "," VALPARTNO (SS2 + 01)
      D                    "," VALPARTNO (SS2 + 02)
      D                    "," VALPARTNO (SS2 + 03)
      D                    "," VALPARTNO (SS2 + 04)
      D                    "," VALPARTNO (SS2 + 05)
      D                    "," VALPARTNO (SS2 + 06)
      D                    "," VALPARTNO (SS2 + 07)
      D                    "," VALPARTNO (SS2 + 08)
      D                    "," VALPARTNO (SS2 + 09)
      D                    "," VALPARTNO (SS2 + 10)
      D                    "," VALPARTNO (SS2 + 11)
      D                    "," VALPARTNO (SS2 + 12)
      D                    "," VALPARTNO (SS2 + 13)
      D                    "," VALPARTNO (SS2 + 14)
      D                    "," VALPARTNO (SS2 + 15)
      D                    "," VALPARTNO (SS2 + 16)
      D                    "," VALPARTNO (SS2 + 17)
      D                    "," VALPARTNO (SS2 + 18)
      D                    "," VALPARTNO (SS2 + 19)
      D                    "," VALPARTNO (SS2 + 20)
      D                    "," VALPARTNO (SS2 + 21)
      D                    "," VALPARTNO (SS2 + 22)
      D                    "," VALPARTNO (SS2 + 23)
      D            END-PERFORM
                   CONTINUE
               END-IF
           END-PERFORM
           PERFORM VARYING SS1 FROM +1 BY +1 UNTIL SS1 > VALPARTMAX
      D        DISPLAY "VALID PART[" SS1 "]=" VALPARTNO (SS1)
               ADD VALPARTNO (SS1) TO PART1-VALUE
           END-PERFORM
           DISPLAY "PART1=" PART1-VALUE
           CONTINUE.

       900-WRAP-UP.
           CLOSE INPUT-FILE.

       TOKENIZE.
      D    DISPLAY SPACE
      D    DISPLAY 'STRING:'
      D    DISPLAY INPUT-STRING (1 : LINELEN + 2)

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
      *            ELSE
      *                DISPLAY T ': ' TOKEN-LEN(T)
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

       SORT-VALID-PARTS.
           MOVE VALPARTMAX TO ITEMCOUNT
           PERFORM WITH TEST AFTER UNTIL HASNOTCHANGED
              SET HASNOTCHANGED TO TRUE
              SUBTRACT 1 FROM ITEMCOUNT
              PERFORM VARYING ITEMINDEX FROM 1 BY 1
                 UNTIL ITEMINDEX > ITEMCOUNT
                 IF VALPARTNO (ITEMINDEX) > VALPARTNO (ITEMINDEX + 1)
                    MOVE VALPARTNO (ITEMINDEX) TO TEMPITEM
                    MOVE VALPARTNO (ITEMINDEX + 1)
                      TO VALPARTNO (ITEMINDEX)
                    MOVE TEMPITEM TO VALPARTNO (ITEMINDEX + 1)
                    SET HASCHANGED TO TRUE
                 END-IF
              END-PERFORM
           END-PERFORM
           .
