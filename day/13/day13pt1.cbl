       PROCESS NOSEQ,DS(S),AR(E),TEST(SO),CP(1047)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY13PT1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * UNCOMMENT "WITH DEBUGGING" CLAUSE FOR DEBUG LINES TO EXECUTE.
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

       WORKING-STORAGE SECTION.
       01  INPUT-FILE-VARIABLES.
           05  LINELEN           PIC  9(18) COMP-5.
           05  LINECTR           PIC S9(18) COMP-5 VALUE +0.
           05  SW-END-OF-FILE    PIC  X(01) VALUE SPACES.
               88 END-OF-FILE               VALUE "Y".

       01  SUBSCRIPT-VARIABLES.
           05  SS1               PIC S9(18) COMP-5.
           05  SS2               PIC S9(18) COMP-5.
           05  SS3               PIC S9(18) COMP-5.
           05  SS4               PIC S9(18) COMP-5.
           05  SS5               PIC S9(18) COMP-5.
           05  SS6               PIC S9(18) COMP-5.
           05  SS7               PIC S9(18) COMP-5.
           05  SS8               PIC S9(18) COMP-5.
           05  S91               PIC S9(09) COMP-5.
           05  S92               PIC S9(09) COMP-5.
           05  S93               PIC S9(09) COMP-5.
           05  S94               PIC S9(09) COMP-5.
           05  S95               PIC S9(09) COMP-5.
           05  S96               PIC S9(09) COMP-5.
           05  S97               PIC S9(09) COMP-5.
           05  S98               PIC S9(09) COMP-5.
           05  U91               PIC  9(09) COMP-5.
           05  U92               PIC  9(09) COMP-5.
           05  U93               PIC  9(09) COMP-5.
           05  U94               PIC  9(09) COMP-5.
           05  U95               PIC  9(09) COMP-5.
           05  U96               PIC  9(09) COMP-5.
           05  U97               PIC  9(09) COMP-5.
           05  U98               PIC  9(09) COMP-5.
           05  S41               PIC S9(04) COMP-5.
           05  S42               PIC S9(04) COMP-5.
           05  S43               PIC S9(04) COMP-5.
           05  S44               PIC S9(04) COMP-5.
           05  S45               PIC S9(04) COMP-5.
           05  S46               PIC S9(04) COMP-5.
           05  S47               PIC S9(04) COMP-5.
           05  S48               PIC S9(04) COMP-5.
           05  REF-COUNT         PIC S9(09) COMP-5.
           05  REF-TOTAL         PIC S9(09) COMP-5.
           05  NONE-TOTAL        PIC S9(09) COMP-5.
           05  MAP-NO            PIC S9(04) COMP-5 VALUE +0.
           05  EVEN-COL-COUNT    PIC S9(04) COMP-5 VALUE +0.
           05  ODD-COL-COUNT     PIC S9(04) COMP-5 VALUE +0.
           05  EVEN-ROW-COUNT    PIC S9(04) COMP-5 VALUE +0.
           05  ODD-ROW-COUNT     PIC S9(04) COMP-5 VALUE +0.

       01 WORK-FLAGS.
          05 CHANGED-FLAG        PIC X.
             88 HASCHANGED             VALUE 'Y'.
             88 HASNOTCHANGED          VALUE 'N'.
          05 FOUND-REFLECT-FLAG  PIC X.
             88 FOUND-REFLECT          VALUE 'Y'.
             88 NOT-FOUND-REF          VALUE 'N'.

       01 WORK-AREAS.
          05  SPR-LEN            PIC S9(04) COMP-5.
          05  GRP-LEN            PIC S9(04) COMP-5.
          05  SCAN-LEN           PIC S9(04) COMP-5.
          05  SPR-TOT            PIC S9(04) COMP-5.
          05  GRP-TOT            PIC S9(04) COMP-5.
          05  UNASGN-TOT         PIC S9(04) COMP-5.
          05  FILE-MIRLEN-MAX    PIC S9(04) COMP-5 VALUE +0.
          05  FILE-MIRMAX-MAX    PIC S9(04) COMP-5 VALUE +0.
          05  SPR-TEXT           PIC X(64).
          05  GRP-TEXT           PIC X(64).
          05  SCAN-TEXT          PIC X(64).
          05  SCAN-TEMP          PIC X(64).

       01 MIRMAP-AREA.
          05  MIRMMAX            PIC S9(04) COMP-5 VALUE +0.
          05  MIRMLEN            PIC S9(04) COMP-5 VALUE +0.
          05  MIRMAP-DATA        OCCURS 64 TIMES
                                 INDEXED BY MIRMNDX.
              10  MIRMAP-TEXT    PIC  X(64) VALUE SPACES.

       01 MIRTRN-AREA.
          05  MIRTMAX            PIC S9(04) COMP-5 VALUE +0.
          05  MIRTLEN            PIC S9(04) COMP-5 VALUE +0.
          05  MIRTRN-DATA        OCCURS 64 TIMES
                                 INDEXED BY MIRTNDX.
              10  MIRTRN-TEXT    PIC  X(64) VALUE SPACES.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING
           PERFORM 100-PROCESS-INPUT-DATA
           PERFORM 900-WRAP-UP
           GOBACK.

       000-HOUSEKEEPING.
           MOVE +0 TO REF-TOTAL NONE-TOTAL
           OPEN INPUT INPUT-FILE
           PERFORM 050-READ-INPUT-DATA
           CONTINUE
           .

       050-READ-INPUT-DATA.
           READ INPUT-FILE
               AT END SET END-OF-FILE TO TRUE
               NOT AT END
                   ADD +1 TO LINECTR
DEBUG>*            DISPLAY "LINE " LINECTR " LEN=" LINELEN
DEBUG>*                ", LINE='" LINEINPUT ( 1 : LINELEN) "'"
           END-READ
           .

       100-PROCESS-INPUT-DATA.
           PERFORM UNTIL END-OF-FILE
               IF LINELEN < 1
                   ADD +1 TO MAP-NO
      D            IF FUNCTION MOD (MIRMMAX, 2) = 0
      D                ADD +1 TO EVEN-ROW-COUNT
      D            ELSE
      D                ADD +1 TO ODD-ROW-COUNT
      D            END-IF
      D            IF FUNCTION MOD (MIRMLEN, 2) = 0
      D                ADD +1 TO EVEN-COL-COUNT
      D            ELSE
      D                ADD +1 TO ODD-COL-COUNT
      D            END-IF
                   IF MIRMMAX > FILE-MIRMAX-MAX
                       MOVE MIRMMAX TO FILE-MIRMAX-MAX
                   END-IF
                   PERFORM 150-PROCESS-MAP
                   MOVE +0 TO MIRMMAX MIRTMAX
                   IF NOT END-OF-FILE
                       PERFORM 050-READ-INPUT-DATA
                   END-IF
               END-IF
               IF LINELEN > FILE-MIRLEN-MAX
                   MOVE LINELEN TO FILE-MIRLEN-MAX
               END-IF
               MOVE LINELEN TO MIRMLEN MIRTMAX
               ADD +1 TO MIRMMAX
               MOVE MIRMMAX TO MIRTLEN
               SET MIRMNDX TO MIRMMAX
               MOVE LINEINPUT (1 : LINELEN) TO MIRMAP-TEXT (MIRMNDX)
               IF NOT END-OF-FILE
                   PERFORM 050-READ-INPUT-DATA
               END-IF
           END-PERFORM
           PERFORM 150-PROCESS-MAP
           .

       150-PROCESS-MAP.
      D    DISPLAY " "
           ADD +1 TO MAP-NO
           PERFORM 200-TRANSPOSE-MAP
      D    PERFORM 250-DISPLAY-MAPS
           PERFORM 300-FIND-ROW-REFLECT
           MOVE +0 TO REF-COUNT
           IF FOUND-REFLECT
               COMPUTE REF-COUNT = S44 * 100
           END-IF
           PERFORM 400-FIND-COL-REFLECT
           IF FOUND-REFLECT
               COMPUTE REF-COUNT = S44
           END-IF
           IF REF-COUNT = +0
               ADD +1 TO NONE-TOTAL
           END-IF
      D    DISPLAY " "
           ADD REF-COUNT TO REF-TOTAL
      D    DISPLAY "MAPNO=" MAP-NO
      D        ",ADDED " REF-COUNT " TO TOTAL"
           .

       200-TRANSPOSE-MAP.
           SET MIRTNDX TO 1
           PERFORM VARYING MIRMNDX FROM 1 BY 1
               UNTIL MIRMNDX > MIRMMAX
               SET S41 TO MIRMNDX
               PERFORM VARYING S42 FROM 1 BY 1
                   UNTIL S42 > MIRTMAX
                   SET MIRTNDX TO S42
                   MOVE MIRMAP-TEXT (MIRMNDX) (S42 : 1)
                     TO MIRTRN-TEXT (MIRTNDX) (S41 : 1)
               END-PERFORM
           END-PERFORM
           .

      D250-DISPLAY-MAPS.
      D    DISPLAY "MAPNO=" MAP-NO
      D    PERFORM VARYING MIRMNDX FROM 1 BY 1
      D        UNTIL MIRMNDX > MIRMMAX
      D        SET S41 TO MIRMNDX
      D        DISPLAY "MAP[" S41 "]='"
      D            MIRMAP-TEXT (MIRMNDX) (1 : MIRMLEN) "'"
      D    END-PERFORM
      D    DISPLAY " "
      D    PERFORM VARYING MIRTNDX FROM 1 BY 1
      D        UNTIL MIRTNDX > MIRTMAX
      D        SET S41 TO MIRTNDX
      D        DISPLAY "TRN[" S41 "]='"
      D            MIRTRN-TEXT (MIRTNDX) (1 : MIRTLEN) "'"
      D    END-PERFORM
      D    DISPLAY " "
      D    .

       300-FIND-ROW-REFLECT.
           SET NOT-FOUND-REF TO TRUE
           PERFORM VARYING MIRMNDX FROM 1 BY 1
               UNTIL MIRMNDX >= MIRMMAX OR FOUND-REFLECT
               SET S41 TO MIRMNDX
               IF MIRMAP-TEXT (MIRMNDX)     (1 : MIRMLEN) =
                  MIRMAP-TEXT (MIRMNDX + 1) (1 : MIRMLEN)
                   COMPUTE S42 = S41 + 1
                   MOVE S41 TO S44
                   SET FOUND-REFLECT TO TRUE
      D            DISPLAY "MAPNO=" MAP-NO
      D                ",FOUND REFLECTED ROWS AT " S41 "," S42
                   PERFORM VARYING MIRMNDX FROM S42 BY 1
                       UNTIL MIRMNDX >= MIRMMAX OR S41 < 1
                       SET S43 TO MIRMNDX
      D                DISPLAY "ROWLEN=" MIRMLEN
      D                    ",ROW[" S41 "]='"
      D                    MIRMAP-TEXT (S41)     (1 : MIRMLEN) "'"
      D                DISPLAY "ROWLEN=" MIRMLEN
      D                    ",ROW[" S43 "]='"
      D                    MIRMAP-TEXT (MIRMNDX) (1 : MIRMLEN) "'"
                       IF MIRMAP-TEXT (S41)     (1 : MIRMLEN) NOT =
                          MIRMAP-TEXT (MIRMNDX) (1 : MIRMLEN)
      D                    DISPLAY "UN-REFLECTED ROWS AT " S41 "," S43
      D                        ",CONTINUING AT " S44
                           SET NOT-FOUND-REF TO TRUE
                           SET MIRMNDX TO S44
                           EXIT PERFORM
                       END-IF
                       SUBTRACT +1 FROM S41
                   END-PERFORM
               END-IF
           END-PERFORM
           .

       400-FIND-COL-REFLECT.
           SET NOT-FOUND-REF TO TRUE
           PERFORM VARYING MIRTNDX FROM 1 BY 1
               UNTIL MIRTNDX >= MIRTMAX OR FOUND-REFLECT
               SET S41 TO MIRTNDX
      D        COMPUTE S42 = S41 + 1
      D        DISPLAY "MAPNO=" MAP-NO
      D            ",COLLEN=" MIRTLEN
      D            ",COL[" S41 "]='"
      D            MIRTRN-TEXT (MIRTNDX)     (1 : MIRTLEN) "'"
      D        DISPLAY "             COLLEN=" MIRTLEN
      D            ",COL[" S42 "]='"
      D            MIRTRN-TEXT (MIRTNDX + 1) (1 : MIRTLEN) "'"
               IF MIRTRN-TEXT (MIRTNDX)     (1 : MIRTLEN) =
                  MIRTRN-TEXT (MIRTNDX + 1) (1 : MIRTLEN)
                   COMPUTE S42 = S41 + 1
                   MOVE S41 TO S44
                   SET FOUND-REFLECT TO TRUE
      D            DISPLAY "MAPNO=" MAP-NO
      D                ",FOUND REFLECTED COLS AT " S41 "," S42
                   SUBTRACT +1 FROM S41
                   ADD +1 TO S42
                   PERFORM VARYING MIRTNDX FROM S42 BY 1
                       UNTIL MIRTNDX >  MIRTMAX OR S41 < 1
                       SET S43 TO MIRTNDX
      D                DISPLAY "COLLEN=" MIRTLEN
      D                    ",COL[" S41 "]='"
      D                    MIRTRN-TEXT (S41)     (1 : MIRTLEN) "'"
      D                DISPLAY "COLLEN=" MIRTLEN
      D                    ",COL[" S43 "]='"
      D                    MIRTRN-TEXT (MIRTNDX) (1 : MIRTLEN) "'"
                       IF MIRTRN-TEXT (S41)     (1 : MIRTLEN) NOT =
                          MIRTRN-TEXT (MIRTNDX) (1 : MIRTLEN)
      D                    DISPLAY "UN-REFLECTED COLS AT " S41 "," S43
      D                        ",CONTINUING AT " S44
                           SET NOT-FOUND-REF TO TRUE
                           SET MIRTNDX TO S44
                           EXIT PERFORM
                       END-IF
                       SUBTRACT +1 FROM S41
                   END-PERFORM
               END-IF
           END-PERFORM
           .

       900-WRAP-UP.
           CLOSE INPUT-FILE.
      D    DISPLAY "FILE LINELEN MAX=" FILE-MIRLEN-MAX
      D        ",FILE MAP LEN MAX=" FILE-MIRMAX-MAX
      D    DISPLAY "EVEN ROW COUNT=" EVEN-ROW-COUNT
      D        ",ODD ROW COUNT=" ODD-ROW-COUNT
      D        ",EVEN COL COUNT=" EVEN-COL-COUNT
      D        ",ODD COL COUNT=" ODD-COL-COUNT
           DISPLAY "REFLECTION TOTAL=" REF-TOTAL
           CONTINUE.

       END PROGRAM DAY13PT1.
