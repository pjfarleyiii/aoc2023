       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY5PT1.
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
           05  GAMECNT           PIC  9(18) BINARY.
           05  GAMELEN           PIC  9(18) BINARY.
           05  PART1-VALUE       PIC  9(18) VALUE ZEROES.

           05  PART2-VALUE       PIC  9(18) VALUE ZEROES.

           05  MAPPED-BOUND-FLAG PIC  X.
               88  NOT-MAPPED-BOUND        VALUE 'N'.
               88  MAPPED-BOUND            VALUE 'Y'.
           05  GAME-STRING       PIC  X(256).

       01  SEED-RANGES.
           05  SEEDMAX           PIC  9(9) BINARY VALUE 0.
           05  SEED-AREA.
               10  SEED-RANGE        OCCURS 1 TO 256 TIMES
                                     DEPENDING ON SEEDMAX.
                   15  SEEDBEG       PIC  9(18) BINARY.
                   15  SEEDEND       PIC  9(18) BINARY.

       01  NEXT-RANGES.
           05  NEXTMAX           PIC  9(9) BINARY VALUE 0.
           05  NEXT-AREA.
               10  NEXT-RANGE        OCCURS 1 TO 256 TIMES
                                     DEPENDING ON NEXTMAX.
                   15  NEXTBEG       PIC  9(18) BINARY.
                   15  NEXTEND       PIC  9(18) BINARY.

       01  BOUNDS-TABLE.
           05  BOUNDMAX          PIC  9(9) BINARY VALUE 0.
           05  BOUND-AREA.
               10  BOUNDS            OCCURS 1 TO 256 TIMES
                                     DEPENDING ON BOUNDMAX.
                   15  BNDBEG        PIC  9(18) BINARY.
                   15  BNDEND        PIC  9(18) BINARY.

       01  MAP-TABLES.
           05  MAPATABLE         OCCURS 7 TIMES.
               10  MAPAMAX       PIC  9(9) BINARY VALUE 0.
               10  MAPANAME      PIC  X(4) VALUE SPACES.

               10  MAPAELEMS.
                   15  MAPADATA  OCCURS 256 TIMES
                                 PIC  X(32) VALUE LOW-VALUES.

       01  MAP-CURR.
           05  MAPMAX            PIC  9(9) BINARY VALUE 0.
           05  MAPNAME           PIC  X(4) VALUE SPACES.

           05  MAP-ELEMS.
               10  MAP           OCCURS 1 TO 256 TIMES
                                 DEPENDING ON MAPMAX.
                   15  MAPSRCBEG PIC  9(18) BINARY.
                   15  MAPSRCEND PIC  9(18) BINARY.
                   15  MAPDSTBEG PIC  9(18) BINARY.
                   15  MAPDSTEND PIC  9(18) BINARY.

       01 TEMP-MAP.
                   15  TMPSRCBEG PIC  9(18) BINARY.
                   15  TMPSRCEND PIC  9(18) BINARY.
                   15  TMPDSTBEG PIC  9(18) BINARY.
                   15  TMPDSTEND PIC  9(18) BINARY.

       01 TEMP-SEED.
                   15  TMPSEEDBEG    PIC  9(18) BINARY.
                   15  TMPSEEDEND    PIC  9(18) BINARY.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING
           PERFORM 100-PROCESS-INPUT-DATA
           PERFORM 200-MAP-SECTIONS
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
      *        CHECK FOR TABLE TYPE
               MOVE ":" TO SEPARATOR-CHAR
               MOVE LINEINPUT (1 : LINELEN) TO INPUT-STRING
               PERFORM TOKENIZE
               IF TOKEN-LEN (2) > 0
      *            ONLY THE SEEDS TABLE HAS DATA ON THE TITLE LINE
      *            CHANGE SPACES IN INPUT TH "|" FOR TOKENIZE TO PROCESS
                   MOVE TOKEN (2)    (2 : TOKEN-LEN(2) - 1)
                     TO INPUT-STRING
                   INSPECT INPUT-STRING REPLACING ALL SPACE BY "|"
                   MOVE SPACES
                     TO INPUT-STRING (TOKEN-LEN (2) : )
      D            DISPLAY INPUT-STRING (1 : LINELEN)
                   MOVE "|" TO SEPARATOR-CHAR
                   PERFORM TOKENIZE
                   MOVE  0 TO SEEDMAX
                   PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > T-MAX
                       ADD  1 TO SEEDMAX
                       COMPUTE SEEDBEG (SEEDMAX) = FUNCTION NUMVAL (
                           TOKEN (SS1) (1 : TOKEN-LEN (SS1)) )
                       COMPUTE SEEDEND (SS1) = SEEDBEG (SS1) + 1
      *                THIS CODE WILL BE USED FOR PART 2
      *                IF FUNCTION MOD (SS1, 2) = 0
      *                    COMPUTE SEEDEND (SEEDMAX) =
      *                        SEED (SS1 - 1) + SEED (SS1)
      *                END-IF
                   END-PERFORM
      *            SORT SEED RANGES BY SEEDBEG
                   MOVE SEEDMAX  TO ITEMCOUNT
                   PERFORM SORT-SEEDS
      D            DISPLAY "SEEDS:"
      D            PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > SEEDMAX
      D                DISPLAY "[" SS1 "]SEEDBEG=" SEEDBEG (SS1)
      D                    " SEEDEND=" SEEDEND (SS1)
      D            END-PERFORM
      *            SKIP BLANK LINE AFTER SEEDS
                   READ INPUT-FILE
                       AT END
                           SET END-OF-FILE TO TRUE
                           EXIT PERFORM
                       NOT AT END
                           ADD +1 TO LINECTR
DEBUG>D                    DISPLAY "LINE " LINECTR " LEN=" LINELEN
                   END-READ
               ELSE
      *            ALL OTHER TABLES HAVE STANDALONE TITLE LINE
      *            DETERMINE MAP NAME AND INDEX FROM TITLE LINE
                   EVALUATE FUNCTION UPPER-CASE ( LINEINPUT (1 : 5) )
                       WHEN "SEED-"
                           MOVE +1 TO MAPNDX
                           MOVE "S2S " TO MAPNAME
                       WHEN "SOIL-"
                           MOVE +2 TO MAPNDX
                           MOVE "S2F " TO MAPNAME
                       WHEN "FERTI"
                           MOVE +3 TO MAPNDX
                           MOVE "F2W " TO MAPNAME
                       WHEN "WATER"
                           MOVE +4 TO MAPNDX
                           MOVE "W2L " TO MAPNAME
                       WHEN "LIGHT"
                           MOVE +5 TO MAPNDX
                           MOVE "L2T " TO MAPNAME
                       WHEN "TEMPE"
                           MOVE +6 TO MAPNDX
                           MOVE "T2H " TO MAPNAME
                       WHEN "HUMID"
                           MOVE +7 TO MAPNDX
                           MOVE "H2L " TO MAPNAME
                   END-EVALUATE
      *            PROCESS MAP RANGE SPECIFICATIONS
                   MOVE 0 TO MAPMAX
                   PERFORM UNTIL END-OF-FILE OR LINELEN = +0
                       READ INPUT-FILE
                           AT END
                               SET END-OF-FILE TO TRUE
                               EXIT PERFORM
                           NOT AT END
                               ADD +1 TO LINECTR
DEBUG>D                        DISPLAY "LINE " LINECTR " LEN=" LINELEN
                               IF LINELEN = +0
                                   EXIT PERFORM
                               END-IF
                       END-READ
      *                EACH MAP TABLE LINE HAS EXACTLY THREE NUMBERS
                       ADD  1 TO MAPMAX
                       MOVE LINEINPUT    (1 : LINELEN) TO INPUT-STRING
                       INSPECT INPUT-STRING REPLACING ALL SPACE BY "|"
                       MOVE SPACES TO INPUT-STRING (LINELEN + 1 : )
      D                DISPLAY INPUT-STRING (1 : LINELEN)
                       MOVE "|" TO SEPARATOR-CHAR
                       PERFORM TOKENIZE
                       COMPUTE SS1                = FUNCTION NUMVAL (
                           TOKEN (3) (1 : TOKEN-LEN (3)) )
                       COMPUTE MAPDSTBEG (MAPMAX) = FUNCTION NUMVAL (
                           TOKEN (1) (1 : TOKEN-LEN (1)) )
                       COMPUTE MAPDSTEND (MAPMAX) = SS1 +
                           MAPDSTBEG (MAPMAX)
                       COMPUTE MAPSRCBEG (MAPMAX) = FUNCTION NUMVAL (
                           TOKEN (2) (1 : TOKEN-LEN (2)) )
                       COMPUTE MAPSRCEND (MAPMAX) = SS1 +
                           MAPSRCBEG (MAPMAX)
                   END-PERFORM
      *            SORT MAP RANGES BY SRCBEG BEFORE STORING
                   MOVE MAPMAX  TO ITEMCOUNT
                   PERFORM SORT-MAP
      *            STORE MAP RANGES FOR LATER USE
                   MOVE MAPMAX  TO MAPAMAX  (MAPNDX)
                   MOVE MAPNAME TO MAPANAME (MAPNDX)
                   COMPUTE SS1 = MAPMAX * FUNCTION LENGTH (MAP (1))
                   MOVE MAP-ELEMS          (1 : SS1)
                     TO MAPAELEMS (MAPNDX) (1 : SS1)
      D            DISPLAY "MAP " MAPNDX " NAME " MAPNAME
      D            PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > MAPMAX
      D                DISPLAY "[" SS1 "]SRCBEG=" MAPSRCBEG (SS1)
      D                    " SRCEND=" MAPSRCEND (SS1)
      D                    " DSTBEG=" MAPDSTBEG (SS1)
      D                    " DSTEND=" MAPDSTEND (SS1)
      D            END-PERFORM
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
      D    DISPLAY "MAPS DONE"
      D    PERFORM VARYING MAPNDX FROM 1 BY 1 UNTIL MAPNDX > 7
      D        COMPUTE SS1 = MAPAMAX (MAPNDX) *
      D            FUNCTION LENGTH ( MAPADATA (1, 1) )
      D        DISPLAY "MAP[" MAPNDX "]="
      D             "[" MAPAMAX (MAPNDX) "," MAPANAME (MAPNDX) "]="
      D             FUNCTION HEX-OF (
      D                 MAPAELEMS (MAPNDX) (1 : SS1) )
      D    END-PERFORM
           DISPLAY " ".

       200-MAP-SECTIONS.
           MOVE SEEDMAX TO BOUNDMAX
           MOVE SEED-AREA TO BOUND-AREA
           MOVE 0       TO NEXTMAX
           PERFORM VARYING MAPNDX FROM 1 BY 1 UNTIL MAPNDX > 7
               MOVE MAPAMAX (MAPNDX) TO MAPMAX
               MOVE MAPANAME (MAPNDX) TO MAPNAME
               MOVE MAPAELEMS (MAPNDX) TO MAP-ELEMS
               MOVE 0 TO NEXTMAX
      D        DISPLAY "MAP[" MAPNDX "],"
      D            "MAPMAX=" MAPMAX ",NAME=" MAPNAME
      D            ",BOUNDMAX=" BOUNDMAX
               PERFORM VARYING SS2 FROM 1 BY 1 UNTIL SS2 > BOUNDMAX
      D            DISPLAY "BOUND[" SS2 "]=["
      D                BNDBEG (SS2) "," BNDEND (SS2) "]"
                   SET NOT-MAPPED-BOUND TO TRUE
      D            PERFORM VARYING SS1 FROM 1 BY 1 UNTIL SS1 > MAPMAX
      D                DISPLAY "MAP[" MAPNDX "][" SS1 "]=["
      D                    "[" MAPSRCBEG (SS1) ","
      D                        MAPSRCEND (SS1) "],"
      D                    "[" MAPDSTBEG (SS1) ","
      D                        MAPDSTEND (SS1) "]]"
                       COMPUTE SS3 = 0 - MAPSRCBEG (SS1) +
                                     MAPDSTBEG (SS1)
      D                DISPLAY "DEST FACTOR=0 - MAPSRCBEG"
      D                    " + MAPDSTBEG=" SS3
                       PERFORM 300-EVALUATE-BOUND
                       IF MAPPED-BOUND
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
               END-PERFORM
      *        DISPLAY BOUNDS AT END OF PROCESSING EACH MAP
      D        DISPLAY "BOUNDS AFTER MAP " MAPNDX
      D            "(" MAPNAME ") DONE"
      D        PERFORM VARYING SS2 FROM 1 BY 1 UNTIL SS2 > BOUNDMAX
      D            DISPLAY "BOUND[" SS2 "]=["
      D                BNDBEG (SS2) "," BNDEND (SS2) "]"
      D        END-PERFORM
      *        DISPLAY NEXT-RANGES AT END OF PROCESSING EACH MAP
      D        IF NEXTMAX > 0
      D            DISPLAY "NEXT-RANGES AFTER MAP " MAPNDX
      D                    "(" MAPNAME ") DONE"
      D            PERFORM VARYING SS2 FROM 1 BY 1 UNTIL SS2 > NEXTMAX
      D                DISPLAY "NEXT[" SS2 "]=["
      D                    NEXTBEG (SS2) "," NEXTEND (SS2) "]"
      D            END-PERFORM
      D        ELSE
      D            DISPLAY "NEXT-RANGES EMPTY AFTER MAP " MAPNDX
      D                    "(" MAPNAME ") DONE"
      D        END-IF
      *        COPY UN-MAPPED BOUNDS TO NEXT-RANGES
               PERFORM VARYING SS2 FROM 1 BY 1 UNTIL SS2 > BOUNDMAX
                   IF BNDBEG (SS2) NOT = BNDEND (SS2)
                       ADD 1 TO NEXTMAX
                       COMPUTE NEXTBEG (NEXTMAX) = BNDBEG (SS2)
                       COMPUTE NEXTEND (NEXTMAX) = BNDEND (SS2)
      D                DISPLAY "CASE 5:BOUND NOT IN MAP RANGE,"
      D                    "BOUND[" SS2 "]=["
      D                    BNDBEG (SS2) "," BNDEND (SS2) "]"
      D                    ",NEXT[" NEXTMAX "]=["
      D                    NEXTBEG (NEXTMAX) "," NEXTEND (NEXTMAX) "]"
                   END-IF
               END-PERFORM
      *        COPY NEXT-RANGES TO BOUNDS AT END OF PROCESSING EACH MAP
               IF NEXTMAX > 0
                   MOVE NEXTMAX TO BOUNDMAX
                   PERFORM VARYING SS2 FROM 1 BY 1 UNTIL SS2 > NEXTMAX
                       MOVE NEXT-RANGE (SS2) TO BOUNDS (SS2)
                   END-PERFORM
               END-IF
      *        SORT BOUNDS BY BNDBEG
               MOVE BOUNDMAX TO ITEMCOUNT
               PERFORM SORT-BOUNDS
      *        DISPLAY NEW BOUNDS AT END OF PROCESSING EACH MAP
      D        DISPLAY "BOUNDS AFTER MAP " MAPNDX
      D            "(" MAPNAME ") DONE"
      D        PERFORM VARYING SS2 FROM 1 BY 1 UNTIL SS2 > BOUNDMAX
      D            DISPLAY "BOUND[" SS2 "]=["
      D                BNDBEG (SS2) "," BNDEND (SS2) "]"
      D        END-PERFORM
           END-PERFORM
           DISPLAY " ".

       300-EVALUATE-BOUND.
           EVALUATE TRUE
               WHEN BNDBEG (SS2) >= MAPSRCBEG (SS1) AND
                    BNDEND (SS2) <= MAPSRCEND (SS1)
                   ADD 1 TO NEXTMAX
                   COMPUTE NEXTBEG (NEXTMAX) = BNDBEG (SS2) + SS3
                   COMPUTE NEXTEND (NEXTMAX) = BNDEND (SS2) + SS3
      D            DISPLAY "CASE 1:BOUND FULLY INSIDE "
      D                "MAP[" MAPNDX "][" SS1 "]=["
      D                "[" MAPSRCBEG (SS1) ","
      D                    MAPSRCEND (SS1) "],"
      D                "[" MAPDSTBEG (SS1) ","
      D                    MAPDSTEND (SS1) "]]"
      D                ",NEXT[" NEXTMAX "]=["
      D                    NEXTBEG (NEXTMAX) "," NEXTEND (NEXTMAX) "]"
                   MOVE BNDEND (SS2) TO BNDBEG (SS2)
                   SET MAPPED-BOUND TO TRUE
                   CONTINUE
               WHEN BNDBEG (SS2) <  MAPSRCBEG (SS1) AND
                    BNDEND (SS2) >  MAPSRCEND (SS1)
                   ADD 1 TO NEXTMAX
                   COMPUTE NEXTBEG (NEXTMAX) = MAPDSTBEG (SS1)
                   COMPUTE NEXTEND (NEXTMAX) = MAPSRCEND (SS2) + SS3
      D            DISPLAY "CASE 2:MAP FULLY INSIDE BOUND,"
      D                "MAP[" MAPNDX "][" SS1 "]=["
      D                "[" MAPSRCBEG (SS1) ","
      D                    MAPSRCEND (SS1) "],"
      D                "[" MAPDSTBEG (SS1) ","
      D                    MAPDSTEND (SS1) "]]"
      D                ",NEXT[" NEXTMAX "]=["
      D                    NEXTBEG (NEXTMAX) "," NEXTEND (NEXTMAX) "]"
      *            DELETE BOUNDS (SS2) BY MOVING REST UP ONE SLOT
                   PERFORM VARYING SS4 FROM SS2 BY +1
                       UNTIL SS4 > BOUNDMAX
                       MOVE BOUNDS (SS4 + 1) TO BOUNDS (SS4)
                   END-PERFORM
      *            NOW ADD NEW RANGE AT END OF BOUNDS LIST
                   MOVE BNDBEG (SS2)    TO BNDBEG (BOUNDMAX)
                   MOVE MAPSRCBEG (SS1) TO BNDEND (BOUNDMAX)
      *            AND ADD ANOTHER NEW RANGE AT END OF BOUNDS LIST
                   ADD 1 TO BOUNDMAX
                   MOVE MAPDSTBEG (SS1) TO BNDBEG (BOUNDMAX)
                   MOVE BNDEND (SS2)    TO BNDEND (BOUNDMAX)
                   SET MAPPED-BOUND TO TRUE
                   CONTINUE
               WHEN BNDBEG (SS2) >= MAPSRCBEG (SS1) AND
                    BNDBEG (SS2) <  MAPSRCEND (SS1)
                   ADD 1 TO NEXTMAX
                   COMPUTE NEXTBEG (NEXTMAX) = BNDBEG    (SS2) + SS3
                   COMPUTE NEXTEND (NEXTMAX) = MAPSRCEND (SS1) + SS3
      D            DISPLAY "CASE 3:ONLY LOW BOUND INSIDE MAP RANGE,"
      D                "MAP[" MAPNDX "][" SS1 "]=["
      D                "[" MAPSRCBEG (SS1) ","
      D                    MAPSRCEND (SS1) "],"
      D                "[" MAPDSTBEG (SS1) ","
      D                    MAPDSTEND (SS1) "]]"
      D                ",NEXT[" NEXTMAX "]=["
      D                    NEXTBEG (NEXTMAX) "," NEXTEND (NEXTMAX) "]"
                   MOVE MAPSRCEND (SS1) TO BNDBEG (SS2)
                   SET MAPPED-BOUND TO TRUE
                   CONTINUE
               WHEN BNDEND (SS2) >= MAPSRCBEG (SS1) AND
                    BNDEND (SS2) <  MAPSRCEND (SS1)
                   ADD 1 TO NEXTMAX
                   COMPUTE NEXTBEG (NEXTMAX) = MAPDSTBEG (SS1)
                   COMPUTE NEXTEND (NEXTMAX) = BNDEND    (SS2) + SS3
      D            DISPLAY "CASE 4:ONLY HIGH BOUND INSIDE MAP RANGE,"
      D                "MAP[" MAPNDX "][" SS1 "]=["
      D                "[" MAPSRCBEG (SS1) ","
      D                    MAPSRCEND (SS1) "],"
      D                "[" MAPDSTBEG (SS1) ","
      D                    MAPDSTEND (SS1) "]]"
      D                ",NEXT[" NEXTMAX "]=["
      D                    NEXTBEG (NEXTMAX) "," NEXTEND (NEXTMAX) "]"
                   MOVE MAPSRCBEG (SS1) TO BNDBEG (SS2)
                   SET MAPPED-BOUND TO TRUE
                   CONTINUE
               WHEN OTHER
      *            DISPLAY "CASE 5:BOUND NOT IN MAP RANGE,"
      *                "MAP[" MAPNDX "][" SS1 "]=["
      *                "[" MAPSRCBEG (SS1) ","
      *                    MAPSRCEND (SS1) "],"
      *                "[" MAPDSTBEG (SS1) ","
      *                    MAPDSTEND (SS1) "]]"
                   CONTINUE
           END-EVALUATE.

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

       SORT-MAP.
           PERFORM WITH TEST AFTER UNTIL HASNOTCHANGED
              SET HASNOTCHANGED TO TRUE
              SUBTRACT 1 FROM ITEMCOUNT
              PERFORM VARYING ITEMINDEX FROM 1 BY 1
                 UNTIL ITEMINDEX > ITEMCOUNT
                 IF MAPSRCBEG (ITEMINDEX) > MAPSRCBEG (ITEMINDEX + 1)
                    MOVE MAP (ITEMINDEX) TO TEMP-MAP
                    MOVE MAP (ITEMINDEX + 1) TO MAP (ITEMINDEX)
                    MOVE TEMP-MAP TO MAP (ITEMINDEX + 1)
                    SET HASCHANGED TO TRUE
                 END-IF
              END-PERFORM
           END-PERFORM
           .

       SORT-SEEDS.
           PERFORM WITH TEST AFTER UNTIL HASNOTCHANGED
              SET HASNOTCHANGED TO TRUE
              SUBTRACT 1 FROM ITEMCOUNT
              PERFORM VARYING ITEMINDEX FROM 1 BY 1
                 UNTIL ITEMINDEX > ITEMCOUNT
                 IF SEEDBEG (ITEMINDEX) > SEEDBEG (ITEMINDEX + 1)
                    MOVE SEED-RANGE (ITEMINDEX) TO TEMP-SEED
                    MOVE SEED-RANGE (ITEMINDEX + 1)
                      TO SEED-RANGE (ITEMINDEX)
                    MOVE TEMP-SEED TO SEED-RANGE (ITEMINDEX + 1)
                    SET HASCHANGED TO TRUE
                 END-IF
              END-PERFORM
           END-PERFORM
           .

       SORT-BOUNDS.
           PERFORM WITH TEST AFTER UNTIL HASNOTCHANGED
              SET HASNOTCHANGED TO TRUE
              SUBTRACT 1 FROM ITEMCOUNT
              PERFORM VARYING ITEMINDEX FROM 1 BY 1
                 UNTIL ITEMINDEX > ITEMCOUNT
                 IF BNDBEG (ITEMINDEX) > BNDBEG (ITEMINDEX + 1)
                    MOVE BOUNDS (ITEMINDEX) TO TEMP-SEED
                    MOVE BOUNDS (ITEMINDEX + 1)
                      TO BOUNDS (ITEMINDEX)
                    MOVE TEMP-SEED TO BOUNDS (ITEMINDEX + 1)
                    SET HASCHANGED TO TRUE
                 END-IF
              END-PERFORM
           END-PERFORM
           .
