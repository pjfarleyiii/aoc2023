       PROCESS NOSEQ,DS(S),AR(E),TEST(SO),CP(1047)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY14PT1.
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

       01 WORK-FLAGS.
          05 CHANGED-FLAG        PIC X.
             88 HASCHANGED             VALUE 'Y'.
             88 HASNOTCHANGED          VALUE 'N'.
          05 FOUND-REFLECT-FLAG  PIC X.
             88 FOUND-REFLECT          VALUE 'Y'.
             88 NOT-FOUND-REF          VALUE 'N'.

       01 WORK-AREAS.
          05  ROCK-WEIGHT        PIC S9(09) COMP-5 VALUE +0.
          05  ROCK-WGHT          PIC S9(09) COMP-5 VALUE +0.

       01 ROCKMAP-AREA.
          05  ROCKMAX            PIC S9(04) COMP-5 VALUE +0.
          05  ROCKLEN            PIC S9(04) COMP-5 VALUE +0.
          05  ROCKMAP-DATA       OCCURS 128 TIMES
                                 INDEXED BY ROCKNDX.
              10  ROCKMAP-TEXT   PIC  X(128) VALUE SPACES.

       PROCEDURE DIVISION.
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
           MOVE LINELEN TO ROCKLEN
           PERFORM UNTIL END-OF-FILE
               ADD +1 TO ROCKMAX
               SET ROCKNDX TO ROCKMAX
               MOVE LINEINPUT (1 : LINELEN) TO ROCKMAP-TEXT (ROCKNDX)
               IF NOT END-OF-FILE
                   PERFORM 050-READ-INPUT-DATA
               END-IF
           END-PERFORM

      D    PERFORM 200-SHOW-MAP
           PERFORM VARYING S42 FROM 1 BY 1
               UNTIL S42 > ROCKLEN
               PERFORM VARYING ROCKNDX FROM 1 BY 1
                   UNTIL ROCKNDX > ROCKMAX
                   SET S41 TO ROCKNDX
      D            DISPLAY "CHAR AT " S41 "," S42 " IS '"
      D                ROCKMAP-TEXT (ROCKNDX) (S42 : 1) "'"
                   IF ROCKMAP-TEXT (ROCKNDX) (S42 : 1) = 'O'
                       COMPUTE S44 = S41 - 1
                       PERFORM WITH TEST BEFORE
                           VARYING S43 FROM S44 BY -1
                           UNTIL S43 = 0 OR
                                 ROCKMAP-TEXT (S43) (S42 : 1) NOT = "."
      D                    DISPLAY "CHAR '"
      D                        ROCKMAP-TEXT (S43) (S42 : 1)
      D                        "' AT " S43 "," S42
                       END-PERFORM
                       ADD +1 TO S43
      D                DISPLAY "AFTER SCAN CHAR '"
      D                    ROCKMAP-TEXT (S43) (S42 : 1)
      D                    "' AT " S43 "," S42
                       IF ROCKMAP-TEXT (S43) (S42 : 1) = "."
                           MOVE 'O' TO ROCKMAP-TEXT (S43) (S42 : 1)
                           MOVE '.' TO ROCKMAP-TEXT (ROCKNDX) (S42 : 1)
                           COMPUTE ROCK-WGHT = (ROCKMAX - S43 + 1)
      D                    DISPLAY "ROCK AT " S41 "," S42
      D                        " IS MOVED TO " S43 "," S42
      D                        " WEIGHT=" ROCK-WGHT
                       ELSE
                           COMPUTE ROCK-WGHT = (ROCKMAX - S41 + 1)
      D                    DISPLAY "ROCK AT " S41 "," S42
      D                        " STAYS AT    " S41 "," S42
      D                        " WEIGHT=" ROCK-WGHT
                       END-IF
                       COMPUTE ROCK-WEIGHT = ROCK-WEIGHT + ROCK-WGHT
                   END-IF
               END-PERFORM
      D        PERFORM 200-SHOW-MAP
           END-PERFORM
           .

       200-SHOW-MAP.
           PERFORM VARYING ROCKNDX FROM 1 BY 1
               UNTIL ROCKNDX > ROCKMAX
               SET S41 TO ROCKNDX
               DISPLAY "MAP[" S41 "]='"
                   ROCKMAP-TEXT (ROCKNDX) (1 : ROCKLEN) "'"
           END-PERFORM
           .

       900-WRAP-UP.
           CLOSE INPUT-FILE.
           DISPLAY "WEIGHT=" ROCK-WEIGHT
           CONTINUE.

       END PROGRAM DAY14PT1.
