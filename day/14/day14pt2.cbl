       PROCESS NOSEQ,DS(S),AR(E),TEST(SO),CP(1047)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY14PT2.
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
           05  LINEINPUT                 PIC  X(32756).

       WORKING-STORAGE SECTION.
       01  INPUT-FILE-VARIABLES.
           05  LINELEN                   PIC  9(18) COMP-5.
           05  LINECTR                   PIC S9(18) COMP-5 VALUE +0.
           05  SW-END-OF-FILE            PIC  X(01) VALUE SPACES.
               88 END-OF-FILE                       VALUE "Y".

       01  SUBSCRIPT-VARIABLES.
           05  SS1                       PIC S9(18) COMP-5.
           05  SS2                       PIC S9(18) COMP-5.
           05  SS3                       PIC S9(18) COMP-5.
           05  SS4                       PIC S9(18) COMP-5.
           05  SS5                       PIC S9(18) COMP-5.
           05  SS6                       PIC S9(18) COMP-5.
           05  SS7                       PIC S9(18) COMP-5.
           05  SS8                       PIC S9(18) COMP-5.
           05  S91                       PIC S9(09) COMP-5.
           05  S92                       PIC S9(09) COMP-5.
           05  S93                       PIC S9(09) COMP-5.
           05  S94                       PIC S9(09) COMP-5.
           05  S95                       PIC S9(09) COMP-5.
           05  S96                       PIC S9(09) COMP-5.
           05  S97                       PIC S9(09) COMP-5.
           05  S98                       PIC S9(09) COMP-5.
           05  U91                       PIC  9(09) COMP-5.
           05  U92                       PIC  9(09) COMP-5.
           05  U93                       PIC  9(09) COMP-5.
           05  U94                       PIC  9(09) COMP-5.
           05  U95                       PIC  9(09) COMP-5.
           05  U96                       PIC  9(09) COMP-5.
           05  U97                       PIC  9(09) COMP-5.
           05  U98                       PIC  9(09) COMP-5.
           05  S41                       PIC S9(04) COMP-5.
           05  S42                       PIC S9(04) COMP-5.
           05  S43                       PIC S9(04) COMP-5.
           05  S44                       PIC S9(04) COMP-5.
           05  S45                       PIC S9(04) COMP-5.
           05  S46                       PIC S9(04) COMP-5.
           05  S47                       PIC S9(04) COMP-5.
           05  S48                       PIC S9(04) COMP-5.

       01 WORK-FLAGS.
          05 CHANGED-FLAG                PIC X.
             88 HASCHANGED                     VALUE 'Y'.
             88 HASNOTCHANGED                  VALUE 'N'.
          05 FOUND-CYCLE-FLAG            PIC X.
             88 FOUND-CYCLE                    VALUE 'Y'.
             88 NOT-FOUND-CYC                  VALUE 'N'.

       01 WORK-AREAS.
          05  CYCLE-LIM                  PIC S9(18) COMP-5 VALUE +0.
          05  CYCLE                      PIC S9(18) COMP-5 VALUE +0.
          05  ROCK-WEIGHT                PIC S9(09) COMP-5 VALUE +0.
          05  ROCK-WGHT                  PIC S9(09) COMP-5 VALUE +0.

       01 ROCKMAP-AREA.
          05  ROCKMAX                    PIC S9(04) COMP-5 VALUE +0.
          05  ROCKLEN                    PIC S9(04) COMP-5 VALUE +0.
          05  ROCKMAP-DATA.
              10  FILLER                 OCCURS 128 TIMES
                                         INDEXED BY ROCKNDX.
                  15  ROCKMAP-TEXT       PIC  X(128) VALUE SPACES.

       01 NEW-MAP-AREA.
          05  NEW-MAX                    PIC S9(04) COMP-5 VALUE +0.
          05  NEW-LEN                    PIC S9(04) COMP-5 VALUE +0.
          05  NEW-MAP-DATA.
              10  FILLER                 OCCURS 128 TIMES
                                         INDEXED BY NEWMNDX.
                  15  NEW-MAP-TEXT       PIC  X(128) VALUE SPACES.

       01 HISTORY-AREA.
          05  HISTMAX                    PIC S9(04) COMP-5 VALUE +0.
          05  HISTLEN                    PIC S9(04) COMP-5 VALUE +0.
          05  HISTORY-DATA.
              10  FILLER                 OCCURS 256 TIMES
                                         INDEXED BY HISTNDX.
                  15  HISTORY-WGHT       PIC S9(09) COMP-5 VALUE +0.

       01 CYCLE-AREA.
          05  CYCLMAX                    PIC S9(04) COMP-5 VALUE +0.
          05  CYCLLEN                    PIC S9(04) COMP-5 VALUE +0.
          05  CYCLE-DATA.
              10  FILLER                 OCCURS 256 TIMES
                                         INDEXED BY CYCLNDX.
                  15  CYCLE-WGHT         PIC S9(09) COMP-5 VALUE +0.

       LINKAGE SECTION.
       01  PARM-AREA.
           05  PARM-LEN                  PIC S9(4) COMP-5.
           05  PARM-LIMIT                PIC  X(18).

       PROCEDURE DIVISION USING PARM-AREA.
           PERFORM 000-HOUSEKEEPING
           PERFORM 100-PROCESS-INPUT-DATA
           PERFORM 900-WRAP-UP
           GOBACK.

       000-HOUSEKEEPING.
           OPEN INPUT INPUT-FILE
           PERFORM 050-READ-INPUT-DATA
           IF PARM-LEN > +0 AND PARM-LIMIT (1 : PARM-LEN) NUMERIC
               COMPUTE CYCLE-LIM = FUNCTION NUMVAL (
                   PARM-LIMIT (1 : PARM-LEN) )
           ELSE
               COMPUTE CYCLE-LIM = LINELEN * +10
           END-IF
           DISPLAY "DAY 14 PART 2 - TRYING " CYCLE-LIM " CYCLES"
           DISPLAY "BUT WILL EXIT ON FINDING REPEATING CYCLE OF WEIGHTS"
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
           MOVE LINELEN TO ROCKLEN NEW-LEN
           PERFORM UNTIL END-OF-FILE
               ADD +1 TO ROCKMAX NEW-MAX
               SET ROCKNDX TO ROCKMAX
               MOVE LINEINPUT (1 : LINELEN) TO ROCKMAP-TEXT (ROCKNDX)
               IF NOT END-OF-FILE
                   PERFORM 050-READ-INPUT-DATA
               END-IF
           END-PERFORM

           PERFORM VARYING CYCLE FROM 1 BY 1 UNTIL CYCLE > CYCLE-LIM
               DISPLAY "CYCLE " CYCLE
               PERFORM 4 TIMES
                   PERFORM 150-ROLL-ROCKS
                   PERFORM 300-ROTATE-RIGHT-90
               END-PERFORM
     D*        PERFORM 200-SHOW-MAP
     D*        DISPLAY " "
               MOVE +0 TO ROCK-WEIGHT
               PERFORM 175-CALC-LOAD
     D*        DISPLAY "WEIGHT=" ROCK-WEIGHT
     D*        DISPLAY " "
               ADD +1 TO HISTMAX
               MOVE ROCK-WEIGHT TO HISTORY-WGHT (HISTMAX)
      *        FOR SPIN-CYCLES 3 AND UP SCAN HISTORY TO FIND THE CYCLE
      *        OF WEIGHT VALUES
               IF CYCLE > +2
                   COMPUTE S48 = HISTMAX - 1
      D            DISPLAY "LOOKING FOR CYCLES IN SIZES FROM 2"
      D                " TO " S48 " IN " HISTMAX " HISTORIES"
                   PERFORM VARYING S41 FROM +2 BY +1
                       UNTIL S41 >= HISTMAX
      *                S41 IS THE CYCLE SIZE
      *                CANNOT COMPARE MORE HISTORY THAN WE HAVE
                       IF S41 * 2 > HISTMAX
     D*                    DISPLAY "EXITING CYCLE LOOP AT " S41
                           EXIT PERFORM
                       END-IF
      *                POPULATE CYCLE-DATA FROM HISTORY
                       COMPUTE S43 = HISTMAX - S41 + 1
     D*                DISPLAY "SIZE=" S41
     D*                    " COPYING HIST FROM " S43
     D*                    " TO " HISTMAX
                       MOVE +0 TO CYCLMAX
                       PERFORM VARYING S42 FROM S43 BY 1
                           UNTIL S42 > HISTMAX
                           ADD +1 TO CYCLMAX
                           MOVE HISTORY-WGHT (S42)
                             TO CYCLE-WGHT (CYCLMAX)
                       END-PERFORM
      *                COMPARE CYCLE-DATA TO SLICE OF HISTORY-DATA
      *                COMPUTE INITIAL HISTORY ELEMENT TO START AT
      *                HISTORY SLICE ENDS AT THE ELEMENT BEFORE THE
      *                FIRST ONE COPIED INTO CYCLE-DATA
                       COMPUTE S43 = HISTMAX - (S41 * 2) + 1
      *                COMPUTE LENGTH OF COMPARE
                       COMPUTE S44 = S43 + CYCLMAX - 1
      D                DISPLAY "COMPARE " CYCLMAX " WEIGHTS TO "
      D                    "HISTORY[" S43 ":" S44 "]"
                       IF S43 < +1
                           EXIT PERFORM CYCLE
                       END-IF
      *                THIS IS THE ACTUAL COMPARE LOOP
                       SET FOUND-CYCLE TO TRUE
                       SET HISTNDX TO S43
                       PERFORM VARYING CYCLNDX FROM 1 BY 1
                           UNTIL CYCLNDX > CYCLMAX OR NOT-FOUND-CYC
                           IF CYCLE-WGHT (CYCLNDX) NOT =
                              HISTORY-WGHT (HISTNDX)
                               SET NOT-FOUND-CYC TO TRUE
                           END-IF
                           SET HISTNDX UP BY 1
                       END-PERFORM
                       IF FOUND-CYCLE
                           DISPLAY " "
                           DISPLAY "FOUND CYCLE:"
                           PERFORM VARYING CYCLNDX FROM 1 BY 1
                               UNTIL CYCLNDX > CYCLMAX
                               SET S47 TO CYCLNDX
                               DISPLAY "CYCLE[" S47 "]="
                                   CYCLE-WGHT (CYCLNDX)
                           END-PERFORM
                           DISPLAY " "
      D                    PERFORM VARYING HISTNDX FROM 1 BY 1
      D                        UNTIL HISTNDX > HISTMAX
      D                        SET S47 TO HISTNDX
      D                        DISPLAY "HIST [" S47 "]="
      D                            HISTORY-WGHT (HISTNDX)
      D                    END-PERFORM
                           DISPLAY " "
                           COMPUTE SS1 = +1000000000 - CYCLE
                           COMPUTE S47 = FUNCTION MOD (SS1, CYCLMAX)
                           DISPLAY "FINAL WEIGHT=CYCLE[" S47 "]="
                               CYCLE-WGHT (S47)
                           MOVE CYCLE-LIM TO CYCLE
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .

       150-ROLL-ROCKS.
           PERFORM VARYING S42 FROM 1 BY 1
               UNTIL S42 > ROCKLEN
               PERFORM VARYING ROCKNDX FROM 1 BY 1
                   UNTIL ROCKNDX > ROCKMAX
                   SET S41 TO ROCKNDX
     D*            DISPLAY "CHAR AT " S41 "," S42 " IS '"
     D*                ROCKMAP-TEXT (ROCKNDX) (S42 : 1) "'"
                   IF ROCKMAP-TEXT (ROCKNDX) (S42 : 1) = 'O'
                       COMPUTE S44 = S41 - 1
                       PERFORM WITH TEST BEFORE
                           VARYING S43 FROM S44 BY -1
                           UNTIL S43 = 0 OR
                                 ROCKMAP-TEXT (S43) (S42 : 1) NOT = "."
     D*                    DISPLAY "CHAR '"
     D*                        ROCKMAP-TEXT (S43) (S42 : 1)
     D*                        "' AT " S43 "," S42
                       END-PERFORM
                       ADD +1 TO S43
     D*                DISPLAY "AFTER SCAN CHAR '"
     D*                    ROCKMAP-TEXT (S43) (S42 : 1)
     D*                    "' AT " S43 "," S42
                       IF ROCKMAP-TEXT (S43) (S42 : 1) = "."
                           MOVE 'O' TO ROCKMAP-TEXT (S43) (S42 : 1)
                           MOVE '.' TO ROCKMAP-TEXT (ROCKNDX) (S42 : 1)
      *                    COMPUTE ROCK-WGHT = (ROCKMAX - S43 + 1)
     D*                    DISPLAY "ROCK AT " S41 "," S42
     D*                        " IS MOVED TO " S43 "," S42
      *                        " WEIGHT=" ROCK-WGHT
      *                ELSE
      *                    COMPUTE ROCK-WGHT = (ROCKMAX - S41 + 1)
     D*                    DISPLAY "ROCK AT " S41 "," S42
     D*                        " STAYS AT    " S41 "," S42
      *                        " WEIGHT=" ROCK-WGHT
                       END-IF
      *                COMPUTE ROCK-WEIGHT = ROCK-WEIGHT + ROCK-WGHT
                   END-IF
               END-PERFORM
      *        PERFORM 200-SHOW-MAP
           END-PERFORM
           .

       175-CALC-LOAD.
           MOVE +0 TO ROCK-WEIGHT
           PERFORM VARYING ROCKNDX FROM 1 BY 1
               UNTIL ROCKNDX > ROCKMAX
               MOVE +0 TO S48
               INSPECT ROCKMAP-TEXT (ROCKNDX) TALLYING S48
                   FOR ALL "O"
               SET S47 TO ROCKNDX
               COMPUTE ROCK-WGHT = (ROCKMAX - S47 + 1) * S48
               ADD ROCK-WGHT TO ROCK-WEIGHT
           END-PERFORM
           .

     D 200-SHOW-MAP.
     D     PERFORM VARYING ROCKNDX FROM 1 BY 1
     D         UNTIL ROCKNDX > ROCKMAX
     D         SET S41 TO ROCKNDX
     D         DISPLAY "MAP[" S41 "]='"
     D             ROCKMAP-TEXT (ROCKNDX) (1 : ROCKLEN) "'"
     D     END-PERFORM
     D     .

       300-ROTATE-RIGHT-90.
      *    ROTATE MAP 90 DEGREES CLOCKWISE
      *    I.E., NORTH BECOMES EAST, EAST BECOMES SOUTH, ETC.
           MOVE ROCKMAX TO S43
           PERFORM VARYING ROCKNDX FROM 1 BY 1
               UNTIL ROCKNDX > ROCKMAX
               PERFORM VARYING NEWMNDX FROM 1 BY 1
                   UNTIL NEWMNDX > ROCKMAX
                   SET S42 TO NEWMNDX
                   MOVE ROCKMAP-TEXT (ROCKNDX) (S42 : 1)
                     TO NEW-MAP-TEXT (NEWMNDX) (S43 : 1)
               END-PERFORM
               SUBTRACT +1 FROM S43
           END-PERFORM
           MOVE NEW-MAP-DATA TO ROCKMAP-DATA
      *    PERFORM 200-SHOW-MAP
           .

       900-WRAP-UP.
           CLOSE INPUT-FILE.
      *    DISPLAY "WEIGHT=" ROCK-WEIGHT
           CONTINUE.

       END PROGRAM DAY14PT2.
