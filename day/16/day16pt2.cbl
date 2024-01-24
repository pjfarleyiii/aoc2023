       PROCESS NOSEQ,DS(S),AR(E),TEST(SO),CP(1047)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY16PT2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * UNCOMMENT "WITH DEBUGGING" CLAUSE FOR DEBUG LINES TO EXECUTE.
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
          05 FOUND-ENERGIZ-FLAG            PIC X.
             88 FOUND-ENERGIZ                  VALUE 'Y'.
             88 NOT-FOUND-NRGZ                 VALUE 'N'.
          05 FOUND-NEW-BEAM-FLAG           PIC X.
             88 FOUND-NEW-BEAM                 VALUE 'Y'.
             88 NOT-FOUND-NEWB                 VALUE 'N'.

       01 WORK-AREAS.
          05  LOOP-COUNT                 PIC S9(09) COMP-5 VALUE +0.
          05  ENERGIZED                  PIC S9(04) COMP-5 VALUE +0.
          05  MAX-ENERGIZED              PIC S9(04) COMP-5 VALUE +0.
          05  MOVE-T                     PIC S9(04) COMP-5 VALUE +0.
          05  START-DATA.
              10  START-X                PIC S9(04) COMP-5 VALUE +0.
              10  START-Y                PIC S9(04) COMP-5 VALUE +0.
              10  START-DX               PIC S9(04) COMP-5 VALUE +0.
              10  START-DY               PIC S9(04) COMP-5 VALUE +0.
          05  MOVE-DATA.
              10  MOVE-X                 PIC S9(04) COMP-5 VALUE +0.
              10  MOVE-Y                 PIC S9(04) COMP-5 VALUE +0.
              10  MOVE-DX                PIC S9(04) COMP-5 VALUE +0.
              10  MOVE-DY                PIC S9(04) COMP-5 VALUE +0.
          05  MOVED-AREA.
              10  MOVDMAX                PIC S9(04) COMP-5 VALUE +0.
              10  MOVED-DATA             OCCURS 2 TIMES.
                  15  MOVED-X            PIC S9(04) COMP-5 VALUE +0.
                  15  MOVED-Y            PIC S9(04) COMP-5 VALUE +0.
                  15  MOVED-DX           PIC S9(04) COMP-5 VALUE +0.
                  15  MOVED-DY           PIC S9(04) COMP-5 VALUE +0.
          05  MIRROR                     PIC  X.

       01 MIRRORS-AREA.
          05  MIROMAX                    PIC S9(04) COMP-5 VALUE +0.
          05  MIROLEN                    PIC S9(04) COMP-5 VALUE +0.
          05  MIRRORS-DATA.
              10  FILLER                 OCCURS 128 TIMES
                                         INDEXED BY MIRONDX.
                  15  MIRRORS            PIC  X(128) VALUE SPACES.

       01 NEW-MAP-AREA.
          05  NEWMMAX                    PIC S9(04) COMP-5 VALUE +0.
          05  NEWMLEN                    PIC S9(04) COMP-5 VALUE +0.
          05  NEW-MAP-DATA.
              10  FILLER                 OCCURS 128 TIMES
                                         INDEXED BY NEWMNDX.
                  15  NEW-MAP            PIC  X(128) VALUE SPACES.

       01 BEAM-AREA.
          05  BEAMMAX                    PIC S9(04) COMP-5 VALUE +0.
          05  BEAMLEN                    PIC S9(04) COMP-5 VALUE +0.
          05  BEAM-DATA.
              10  BEAM                   OCCURS 2560 TIMES
                                         INDEXED BY BEAMNDX.
                  15  BEAM-X             PIC S9(04) COMP-5 VALUE +0.
                  15  BEAM-Y             PIC S9(04) COMP-5 VALUE +0.
                  15  BEAM-DX            PIC S9(04) COMP-5 VALUE +0.
                  15  BEAM-DY            PIC S9(04) COMP-5 VALUE +0.

       01 NEW-BEAM-AREA.
          05  NEWBMAX                    PIC S9(04) COMP-5 VALUE +0.
          05  NEWBLEN                    PIC S9(04) COMP-5 VALUE +0.
          05  NEW-BEAM-DATA.
              10  NEW-BEAM               OCCURS 2560 TIMES
                                         INDEXED BY NEWBNDX.
                  15  NEWB-X             PIC S9(04) COMP-5 VALUE +0.
                  15  NEWB-Y             PIC S9(04) COMP-5 VALUE +0.
                  15  NEWB-DX            PIC S9(04) COMP-5 VALUE +0.
                  15  NEWB-DY            PIC S9(04) COMP-5 VALUE +0.

       01 ENERGIZ-AREA.
          05  NRGZMAX                    PIC S9(04) COMP-5 VALUE +0.
          05  NRGZLEN                    PIC S9(04) COMP-5 VALUE +0.
          05  ENERGIZ-DATA.
              10  ENERGIZ                OCCURS 20480 TIMES
                                         INDEXED BY NRGZNDX.
                  15  NRGZ-X             PIC S9(04) COMP-5 VALUE +0.
                  15  NRGZ-Y             PIC S9(04) COMP-5 VALUE +0.
                  15  NRGZ-DX            PIC S9(04) COMP-5 VALUE +0.
                  15  NRGZ-DY            PIC S9(04) COMP-5 VALUE +0.

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
           MOVE LINELEN TO MIROLEN BEAMLEN
           PERFORM UNTIL END-OF-FILE
               ADD +1 TO MIROMAX BEAMMAX
               SET MIRONDX TO MIROMAX
               MOVE LINEINPUT (1 : LINELEN) TO MIRRORS (MIRONDX)
               IF NOT END-OF-FILE
                   PERFORM 050-READ-INPUT-DATA
               END-IF
           END-PERFORM
     D*    PERFORM 200-SHOW-MAP

      *    FIND MAX ENERGIZED STARTING AT LEFT AND RIGHT OF EACH ROW
           PERFORM VARYING START-Y FROM 1 BY 1
               UNTIL START-Y > MIROMAX
      *        MOVING LEFT TO RIGHT
               MOVE MIRRORS-AREA TO NEW-MAP-AREA
               MOVE +0 TO BEAMMAX NEWBMAX
               MOVE +1 TO START-X START-DX
               MOVE +0 TO START-DY
               PERFORM 300-GET-ENERGIZED
               PERFORM 300-COUNT-ENERGIZED
               IF ENERGIZED > MAX-ENERGIZED
                   MOVE ENERGIZED TO MAX-ENERGIZED
               END-IF
      *        MOVING RIGHT TO LEFT
               MOVE MIRRORS-AREA TO NEW-MAP-AREA
               MOVE +0 TO BEAMMAX NEWBMAX
               COMPUTE START-X = MIROLEN - START-X + 1
               MOVE -1 TO START-DX
               MOVE +0 TO START-DY
               PERFORM 300-GET-ENERGIZED
               PERFORM 300-COUNT-ENERGIZED
               IF ENERGIZED > MAX-ENERGIZED
                   MOVE ENERGIZED TO MAX-ENERGIZED
               END-IF
           END-PERFORM
      D    DISPLAY "PART 2A - MAXIMUM ENERGIZED:" MAX-ENERGIZED

      *    FIND MAX ENERGIZED STARTING AT TOP AND BOTTOM OF EACH COLUMN
           PERFORM VARYING START-X FROM 1 BY 1
               UNTIL START-X > MIROLEN
      *        MOVING TOP TO BOTTOM
               MOVE MIRRORS-AREA TO NEW-MAP-AREA
               MOVE +0 TO BEAMMAX NEWBMAX
               MOVE +1 TO START-Y START-DY
               MOVE +0 TO START-DX
               PERFORM 300-GET-ENERGIZED
               PERFORM 300-COUNT-ENERGIZED
               IF ENERGIZED > MAX-ENERGIZED
                   MOVE ENERGIZED TO MAX-ENERGIZED
               END-IF
      *        MOVING BOTTOM TO TOP
               MOVE MIRRORS-AREA TO NEW-MAP-AREA
               MOVE +0 TO BEAMMAX NEWBMAX
               COMPUTE START-Y = MIROLEN - START-X + 1
               MOVE -1 TO START-DY
               MOVE +0 TO START-DX
               PERFORM 300-GET-ENERGIZED
               PERFORM 300-COUNT-ENERGIZED
               IF ENERGIZED > MAX-ENERGIZED
                   MOVE ENERGIZED TO MAX-ENERGIZED
               END-IF
           END-PERFORM
           DISPLAY "PART 2 - MAXIMUM ENERGIZED:" MAX-ENERGIZED
           .

       150-MOVE-BEAM.
           MOVE MIRRORS (MOVE-Y) (MOVE-X : 1) TO MIRROR
     D*    DISPLAY "MOVING FROM MIRROR '" MIRROR "'"
           MOVE +1 TO MOVDMAX
           EVALUATE TRUE
               WHEN MIRROR = "-" AND MOVE-DY NOT = +0
     D*            DISPLAY "MOVE CASE #1"
                   MOVE +2 TO MOVDMAX
                   MOVE MOVE-X TO MOVED-X  (1) MOVED-X  (2)
                   MOVE MOVE-Y TO MOVED-Y  (1) MOVED-Y  (2)
                   MOVE -1     TO MOVED-DX (1)
                   MOVE +1     TO MOVED-DX (2)
                   MOVE +0     TO MOVED-DY (1) MOVED-DY (2)
               WHEN MIRROR = "|" AND MOVE-DX NOT = +0
     D*            DISPLAY "MOVE CASE #2"
                   MOVE +2 TO MOVDMAX
                   MOVE MOVE-X TO MOVED-X  (1) MOVED-X  (2)
                   MOVE MOVE-Y TO MOVED-Y  (1) MOVED-Y  (2)
                   MOVE +0     TO MOVED-DX (1) MOVED-DX (2)
                   MOVE -1     TO MOVED-DY (1)
                   MOVE +1     TO MOVED-DY (2)
               WHEN MIRROR = "/"
     D*            DISPLAY "MOVE CASE #3"
                   MOVE MOVE-DX TO MOVE-T
                   COMPUTE MOVE-DX = 0 - MOVE-DY
                   COMPUTE MOVE-DY = 0 - MOVE-T
                   COMPUTE MOVED-X  (1) = MOVE-X + MOVE-DX
                   COMPUTE MOVED-Y  (1) = MOVE-Y + MOVE-DY
                   COMPUTE MOVED-DX (1) = MOVE-DX
                   COMPUTE MOVED-DY (1) = MOVE-DY
               WHEN MIRROR = "\"
     D*            DISPLAY "MOVE CASE #4"
                   MOVE MOVE-DX TO MOVE-T
                   MOVE MOVE-DY TO MOVE-DX
                   MOVE MOVE-T  TO MOVE-DY
                   COMPUTE MOVED-X  (1) = MOVE-X + MOVE-DX
                   COMPUTE MOVED-Y  (1) = MOVE-Y + MOVE-DY
                   COMPUTE MOVED-DX (1) = MOVE-DX
                   COMPUTE MOVED-DY (1) = MOVE-DY
               WHEN OTHER
     D*            DISPLAY "MOVE CASE #5"
                   COMPUTE MOVED-X  (1) = MOVE-X + MOVE-DX
                   COMPUTE MOVED-Y  (1) = MOVE-Y + MOVE-DY
                   COMPUTE MOVED-DX (1) = MOVE-DX
                   COMPUTE MOVED-DY (1) = MOVE-DY
           END-EVALUATE
           .

       160-ADD-ENERGIZED.
           PERFORM 165-SRCH-ENERGIZED
           IF NOT-FOUND-NRGZ
               ADD +1 TO NRGZMAX
               MOVE MOVE-DATA TO ENERGIZ (NRGZMAX)
           END-IF
           .

       165-SRCH-ENERGIZED.
           SET NOT-FOUND-NRGZ TO TRUE
           PERFORM VARYING NRGZNDX FROM 1 BY 1
               UNTIL NRGZNDX > NRGZMAX
               IF ENERGIZ (NRGZNDX) = MOVE-DATA
                   SET FOUND-ENERGIZ TO TRUE
               END-IF
           END-PERFORM
           .

       170-ADD-NEW-BEAM.
           SET NOT-FOUND-NEWB TO TRUE
           PERFORM VARYING NEWBNDX FROM 1 BY 1
               UNTIL NEWBNDX > NEWBMAX
               IF NEW-BEAM (NEWBNDX) = MOVE-DATA
                   SET FOUND-NEW-BEAM TO TRUE
               END-IF
           END-PERFORM
           IF NOT-FOUND-NEWB
               ADD +1 TO NEWBMAX
               MOVE MOVE-DATA TO NEW-BEAM (NEWBMAX)
           END-IF
           .

     D 200-SHOW-MAP.
     D     PERFORM VARYING MIRONDX FROM 1 BY 1
     D         UNTIL MIRONDX > MIROMAX
     D         SET S41 TO MIRONDX
     D         DISPLAY "MAP[" S41 "]='"
     D             MIRRORS (MIRONDX) (1 : MIROLEN) "'"
     D     END-PERFORM
     D     .

     D 250-SHOW-NEW.
     D     PERFORM VARYING NEWMNDX FROM 1 BY 1
     D         UNTIL NEWMNDX > NEWMMAX
     D         SET S41 TO NEWMNDX
     D         DISPLAY "NEW[" S41 "]='"
     D             NEW-MAP (NEWMNDX) (1 : NEWMLEN) "'"
     D     END-PERFORM
     D     .

       300-GET-ENERGIZED.
           MOVE +0 TO LOOP-COUNT
      *    INITIALIZE BEAMS AND ENERGIZED LISTS
           MOVE +1 TO BEAMMAX NRGZMAX
           MOVE START-DATA TO BEAM (1) ENERGIZ (1)
           MOVE "#" TO NEW-MAP (START-Y) (START-X : 1)
      *    LOOP UNTIL NO MORE MOVES AVAILABLE
           PERFORM WITH TEST BEFORE UNTIL BEAMMAX <= +0
               ADD +1 TO LOOP-COUNT
      *        MOVE THE BEAM THROUGH THE MAP
               MOVE +0 TO NEWBMAX
               PERFORM VARYING BEAMNDX FROM 1 BY 1
                   UNTIL BEAMNDX > BEAMMAX
                   MOVE BEAM (BEAMNDX) TO MOVE-DATA
                   PERFORM 160-ADD-ENERGIZED
                   PERFORM 150-MOVE-BEAM
                   PERFORM VARYING S41 FROM 1 BY 1
                       UNTIL S41 > MOVDMAX
                       ADD +1 TO NEWBMAX
                       MOVE MOVED-DATA (S41) TO NEW-BEAM (NEWBMAX)
     D*                DISPLAY "MOVED [" S41 "]="
     D*                    MOVED-X  (S41) ","
     D*                    MOVED-Y  (S41) ","
     D*                    MOVED-DX (S41) ","
     D*                    MOVED-DY (S41)
                   END-PERFORM
               END-PERFORM
               MOVE +0 TO BEAMMAX
               PERFORM VARYING NEWBNDX FROM 1 BY 1
                   UNTIL NEWBNDX > NEWBMAX
                   MOVE NEW-BEAM (NEWBNDX) TO MOVE-DATA
     D*            SET S41 TO NEWBNDX
     D*            DISPLAY "NEW BEAM[" S41 "]="
     D*                NEWB-X  (S41) ","
     D*                NEWB-Y  (S41) ","
     D*                NEWB-DX (S41) ","
     D*                NEWB-DY (S41)
                   PERFORM 165-SRCH-ENERGIZED
                   IF NOT-FOUND-NRGZ               AND
                      (MOVE-X  > 0 AND <= MIROLEN) AND
                      (MOVE-Y  > 0 AND <= MIROMAX)
                       ADD +1 TO BEAMMAX
                       MOVE MOVE-DATA TO BEAM (BEAMMAX)
                       MOVE "#" TO NEW-MAP (MOVE-Y) (MOVE-X : 1)
                   END-IF
               END-PERFORM
     D*        PERFORM VARYING BEAMNDX FROM 1 BY 1
     D*            UNTIL BEAMNDX > BEAMMAX
     D*            SET S41 TO BEAMNDX
     D*            DISPLAY "BEAM[" S41 "]="
     D*                BEAM-X  (S41) ","
     D*                BEAM-Y  (S41) ","
     D*                BEAM-DX (S41) ","
     D*                BEAM-DY (S41)
     D*        END-PERFORM
     D*        PERFORM 250-SHOW-NEW
      D        IF FUNCTION MOD (LOOP-COUNT, 500) = 0
      D            DISPLAY "LOOPCNT=" LOOP-COUNT
      D                ",BEAMMAX=" BEAMMAX ",NRGZMAX=" NRGZMAX
      D        END-IF
           END-PERFORM
           .

       300-COUNT-ENERGIZED.
      *    COUNT ENERGIZED TILES
           MOVE +0 TO ENERGIZED
           PERFORM VARYING NEWMNDX FROM 1 BY 1
               UNTIL NEWMNDX > NEWMMAX
               MOVE +0 TO S42
               INSPECT NEW-MAP (NEWMNDX) TALLYING S42
                   FOR ALL "#"
               ADD S42 TO ENERGIZED
     D*        SET S41 TO NEWMNDX
     D*        DISPLAY "NEW[" S41 "]='"
     D*            NEW-MAP (NEWMNDX) (1 : NEWMLEN) "'"
           END-PERFORM
           .

       900-WRAP-UP.
           CLOSE INPUT-FILE.
           CONTINUE.

       END PROGRAM DAY16PT2.
