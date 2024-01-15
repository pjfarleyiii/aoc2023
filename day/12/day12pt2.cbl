       PROCESS NOSEQ,DS(S),AR(E),MAP,TEST(SO)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY12PT2.
      * BASED ON AN IDEA FROM THE FOLLOWING URL FOUND ON THE AOC 2023
      * SOLUTIONS MEGATHREAD
      * https://github.com/clrfl/AdventOfCode2023/blob/master/12/
      *  explanation.ipynb
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
           05  ARR-COUNT         PIC S9(18) COMP-5.
           05  ARR-TOTAL         PIC S9(18) COMP-5.
           05  STATE             PIC S9(04) COMP-5.

       01 WORK-FLAGS.
          05 CHANGED-FLAG        PIC X.
             88 HASCHANGED             VALUE 'Y'.
             88 HASNOTCHANGED          VALUE 'N'.
          05 VALID-ARRANGE-FLAG  PIC X.
             88 VALID-ARR              VALUE 'Y'.
             88 NOT-VALID-ARR          VALUE 'N'.

       01 WORK-AREAS.
          05  SPR-LEN            PIC S9(04) COMP-5.
          05  GRP-LEN            PIC S9(04) COMP-5.
          05  SCAN-LEN           PIC S9(04) COMP-5.
          05  SPR-TOT            PIC S9(04) COMP-5.
          05  GRP-TOT            PIC S9(04) COMP-5.
          05  STATES-LEN         PIC S9(04) COMP-5.
          05  FILE-STATES-MAX    PIC S9(04) COMP-5.
          05  FILE-GROUP-MAX     PIC S9(04) COMP-5.
          05  SPR-TEXT           PIC X(256).
          05  GRP-TEXT           PIC X(256).
          05  SCAN-TEXT          PIC X(256).
          05  SCAN-TEMP          PIC X(256).
          05  STATES             PIC X(256).
          05  TEXT-CHAR          PIC X.

       01 GROUPS-AREA.
          05  GRP-MAX            PIC S9(04) COMP-5 VALUE +0.
          05  GROUP-DATA         OCCURS 64 TIMES
                                 INDEXED BY GRPNDX.
              10  GROUP-TEXT     PIC  X(04) VALUE SPACES.
              10  GROUP-VAL      PIC S9(04) COMP-5 VALUE +0.

       01 STATES-AREA.
          05  STATES-MAX         PIC S9(04) COMP-5 VALUE +0.
          05  STATES-DICT.
              10  STATES-VAL     OCCURS 1024 TIMES
                                 INDEXED BY STATESNDX
                                 PIC S9(18) COMP-5 VALUE +0.

       01 NEW-STATES-AREA.
          05  NEW-STATES-MAX     PIC S9(04) COMP-5 VALUE +0.
          05  NEW-STATES-DICT.
              10  NEW-STATES-VAL OCCURS 1024 TIMES
                                 INDEXED BY NEWDICNDX
                                 PIC S9(18) COMP-5 VALUE +0.

       01  UNRANK-DATA.
           05  UNRKCNT           PIC  9(9) COMP-5 VALUE 0.
           05  UNRK-ITEM         OCCURS 32 TIMES
                                 INDEXED BY UNRKNDX.
               10  UNRK-VAL      PIC S9(9) COMP-5 VALUE 0.

       01  SCAN-DATA.
           05  SCANCNT           PIC  9(9) COMP-5 VALUE 0.
           05  SCAN-ITEM         OCCURS 32 TIMES
                                 INDEXED BY SCANNDX.
               10  SCAN-VAL      PIC S9(9) COMP-5 VALUE 0.

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
DEBUG>D                ", LINE='" LINEINPUT ( 1 : LINELEN) "'"
           END-READ
           CONTINUE
           .

       100-PROCESS-INPUT-DATA.
           MOVE +0 TO FILE-GROUP-MAX ARR-TOTAL
           PERFORM UNTIL END-OF-FILE
               MOVE +0 TO ARR-COUNT
               UNSTRING LINEINPUT (1 : LINELEN)
                   DELIMITED BY SPACE
                   INTO SPR-TEXT COUNT IN SPR-LEN
               END-UNSTRING
      D        DISPLAY "DATA='" SPR-TEXT (1 : SPR-LEN) "'"
               COMPUTE GRP-LEN = LINELEN - SPR-LEN - 1
               MOVE LINEINPUT (SPR-LEN + 2 : GRP-LEN) TO GRP-TEXT
      D        DISPLAY "GROUPS='" GRP-TEXT (1 : GRP-LEN) "'"
      *        UNFOLD THE MAP LINE
               STRING
                   SPR-TEXT (1 : SPR-LEN) DELIMITED BY SIZE
                   "?"                    DELIMITED BY SIZE
                   SPR-TEXT (1 : SPR-LEN) DELIMITED BY SIZE
                   "?"                    DELIMITED BY SIZE
                   SPR-TEXT (1 : SPR-LEN) DELIMITED BY SIZE
                   "?"                    DELIMITED BY SIZE
                   SPR-TEXT (1 : SPR-LEN) DELIMITED BY SIZE
                   "?"                    DELIMITED BY SIZE
                   SPR-TEXT (1 : SPR-LEN) DELIMITED BY SIZE
                   INTO SCAN-TEXT
               END-STRING
               COMPUTE SPR-LEN = (SPR-LEN * 5) + 4
               MOVE SCAN-TEXT (1 : SPR-LEN) TO SPR-TEXT
      D        DISPLAY "UNFOLD DATA='" SPR-TEXT (1 : SPR-LEN) "'"
      *        UNFOLD THE GROUPS
               STRING
                   GRP-TEXT (1 : GRP-LEN) DELIMITED BY SIZE
                   ","                    DELIMITED BY SIZE
                   GRP-TEXT (1 : GRP-LEN) DELIMITED BY SIZE
                   ","                    DELIMITED BY SIZE
                   GRP-TEXT (1 : GRP-LEN) DELIMITED BY SIZE
                   ","                    DELIMITED BY SIZE
                   GRP-TEXT (1 : GRP-LEN) DELIMITED BY SIZE
                   ","                    DELIMITED BY SIZE
                   GRP-TEXT (1 : GRP-LEN) DELIMITED BY SIZE
                   INTO SCAN-TEXT
               END-STRING
               COMPUTE GRP-LEN = (GRP-LEN * 5) + 4
               MOVE SCAN-TEXT (1 : GRP-LEN) TO GRP-TEXT
      D        DISPLAY "UNFOLD GROUPS='" GRP-TEXT (1 : GRP-LEN) "'"
      *        COUNT UNDAMAGED SPRINGS
               MOVE ZERO TO SPR-TOT
               INSPECT SPR-TEXT TALLYING SPR-TOT FOR ALL "#"
      *        CONVERT GROUPS TEXT TO INTEGERS IN GROUPS-DATA
               MOVE ZERO TO S41
               INSPECT GRP-TEXT TALLYING S41 FOR ALL ","
               ADD  +1 TO S41
               MOVE +1 TO S42
               MOVE +0 TO GRP-TOT
               PERFORM VARYING GRPNDX FROM 1 BY 1 UNTIL GRPNDX > S41
                   MOVE SPACES TO GROUP-TEXT (GRPNDX)
                   UNSTRING GRP-TEXT (1 : GRP-LEN)
                       DELIMITED BY ","
                       INTO GROUP-TEXT (GRPNDX)
                       COUNT   S43
                       POINTER S42
                   END-UNSTRING
                   SET GRP-MAX TO GRPNDX
                   COMPUTE GROUP-VAL (GRPNDX) = FUNCTION NUMVAL (
                       GROUP-TEXT (GRPNDX) (1 : S43) )
                   ADD GROUP-VAL (GRPNDX) TO GRP-TOT
                   IF GROUP-VAL (GRPNDX) > FILE-GROUP-MAX
                       MOVE GROUP-VAL (GRPNDX) TO FILE-GROUP-MAX
                   END-IF
      D            DISPLAY "GROUP[" GRP-MAX "],LEN=" S43 ",'"
      D                GROUP-TEXT (GRPNDX) (1 : S43) "'"
      D                ",VALUE=" GROUP-VAL (GRPNDX)
      D                ",TOTAL=" GRP-TOT
               END-PERFORM
      *        CONSTRUCT NFA STRING STATES
               MOVE '.' TO STATES (1 : 1)
               MOVE +2 TO S42
               PERFORM VARYING GRPNDX FROM 1 BY 1
                   UNTIL GRPNDX > GRP-MAX
                   MOVE ALL "#" TO STATES (S42 : GROUP-VAL (GRPNDX))
                   ADD GROUP-VAL (GRPNDX) TO S42
                   MOVE '.' TO STATES (S42 : 1)
                   ADD +1 TO S42
               END-PERFORM
               COMPUTE STATES-LEN = S42 - 1
               IF STATES-LEN > FILE-STATES-MAX
                   MOVE STATES-LEN TO FILE-STATES-MAX
               END-IF
      D        DISPLAY "STATES-LEN=" STATES-LEN
      D                ",STATES='" STATES (1 : STATES-LEN) "'"
      *        SCAN TEXT CREATING NFA STATES AND COUNTS
      *        INITIALIZING TABLES WITH LOW-VALUES MAKES ALL STATE
      *        AND NEW-STATE VALUES ZERO SO THEY CAN ALWAYS BE SAFELY
      *        USED FOR ADDITION CALCULATIONS
               MOVE LOW-VALUES TO STATES-AREA NEW-STATES-AREA
               MOVE STATES-LEN TO STATES-MAX  NEW-STATES-MAX
               MOVE +1 TO STATES-VAL (1)
               MOVE +0 TO ARR-COUNT
               PERFORM VARYING S41 FROM 1 BY 1 UNTIL S41 > SPR-LEN
                   MOVE SPR-TEXT (S41 : 1) TO TEXT-CHAR
      D            DISPLAY "CHAR '" TEXT-CHAR "' AT POS=" S41
      *            ONLY NON-ZERO-VALUE ARRAY ELEMENTS ARE "IN" THE
      *            DICTONARY BECAUSE WE ARE SIMULATING DICTONARIES WITH
      *            ARRAYS.  WE PROCESS IN REVERSE ORDER TO SIMULATE THE
      *            ORIGINAL ALGORITHM'S POPULATION OF HIGHER-VALUED
      *            KEYS BEFORE LOWER-VALUED KEYS
                   PERFORM VARYING STATESNDX FROM STATES-MAX BY -1
                       UNTIL STATESNDX < 1
                       IF STATES-VAL (STATESNDX) = +0
      *                    DISPLAY "BYPASSING STATE[" STATE "]="
      *                        STATES-VAL (STATESNDX)
                           EXIT PERFORM CYCLE
                       END-IF
      *                IN THE ORIGINAL ALGOTITHM, "STATE" IS THE
      *                ZERO-BASED DICTIONARY KEY, NOT ITS VALUE
      *                COBOL ARRAYS ARE ONE-BASED, SO ADJUST AS NEEDED
                       SET STATE TO STATESNDX
                       SUBTRACT +1 FROM STATE
      *                STATE IS NOW ZERO-BASED
      D                COMPUTE S42 = STATE + 1
      D                DISPLAY "STATE=" STATE
      D                    ",STATES[" STATE "]='" STATES (S42 : 1)
      D                    "',STATES[" S42 "]='" STATES (S42 + 1 : 1)
      D                    "'"
                       EVALUATE TEXT-CHAR
                           WHEN "?"
                               IF (STATE + 1) < STATES-LEN
                                   ADD STATES-VAL (STATE + 1)
                                    TO NEW-STATES-VAL (STATE + 2)
      D                            COMPUTE S42 = STATE + 1
      D                            DISPLAY "CASE 1:"
      D                                "ADDED NEW-DICT[" S42 "]="
      D                                NEW-STATES-VAL (S42 + 1)
                               END-IF
                               IF STATES (STATE + 1 : 1) = "."
                                   ADD STATES-VAL (STATE + 1)
                                    TO NEW-STATES-VAL (STATE + 1)
      D                            COMPUTE S42 = STATE + 1
      D                            DISPLAY "CASE 2:"
      D                                "ADDED NEW-DICT[" S42 "]="
      D                                NEW-STATES-VAL (S42)
                               END-IF
                           WHEN "."
                               IF (STATE + 1) < STATES-LEN AND
                                  STATES (STATE + 2 : 1) = "."
                                   ADD STATES-VAL (STATE + 1)
                                    TO NEW-STATES-VAL (STATE + 2)
      D                            COMPUTE S42 = STATE + 1
      D                            DISPLAY "CASE 3:"
      D                                "ADDED NEW-DICT[" S42 "]="
      D                                NEW-STATES-VAL (S42 + 1)
                               END-IF
                               IF STATES (STATE + 1 : 1) = "."
                                   ADD STATES-VAL (STATE + 1)
                                    TO NEW-STATES-VAL (STATE + 1)
      D                            COMPUTE S42 = STATE + 1
      D                            DISPLAY "CASE 4:"
      D                                "ADDED NEW-DICT[" S42 "]="
      D                                NEW-STATES-VAL (S42)
                               END-IF
                           WHEN "#"
                               IF (STATE + 1) < STATES-LEN AND
                                  STATES (STATE + 2 : 1) = "#"
                                   ADD STATES-VAL (STATE + 1)
                                    TO NEW-STATES-VAL (STATE + 2)
      D                            COMPUTE S42 = STATE + 1
      D                            DISPLAY "CASE 5:"
      D                                "ADDED NEW-DICT[" S42 "]="
      D                                NEW-STATES-VAL (S42 + 1)
                               END-IF
                       END-EVALUATE
                   END-PERFORM
                   MOVE NEW-STATES-DICT TO STATES-DICT
                   MOVE LOW-VALUES      TO NEW-STATES-DICT
      D            PERFORM VARYING STATESNDX FROM STATES-MAX BY -1
      D                UNTIL STATESNDX < 1
      D                IF STATES-VAL (STATESNDX) > 0
      D                    SET S42 TO STATESNDX
      D                    DISPLAY "DICT[" S42 "]="
      D                        STATES-VAL (STATESNDX)
      D                END-IF
      D            END-PERFORM
               END-PERFORM
      D        COMPUTE S42 = STATES-MAX - 1
      D        COMPUTE S43 = STATES-MAX - 2
      D        DISPLAY "DICT[" S42 "]=" STATES-VAL (STATES-MAX)
      D                ",DICT[" S43 "]=" STATES-VAL (STATES-MAX - 1)
               ADD STATES-VAL (STATES-MAX)
                   STATES-VAL (STATES-MAX - 1)
                TO ARR-COUNT
               ADD ARR-COUNT TO ARR-TOTAL
DEBUG>*        DISPLAY "ARRANGEMENTS=" ARR-COUNT
      D        DISPLAY "ARRANGEMENTS OF LINE " LINECTR " = " ARR-COUNT
      D        DISPLAY "ARRANGEMENTS TOTAL               = " ARR-TOTAL
               IF NOT END-OF-FILE
                   READ INPUT-FILE
                       AT END SET END-OF-FILE TO TRUE
                       NOT AT END
                           ADD +1 TO LINECTR
DEBUG>D                    DISPLAY "LINE " LINECTR " LEN=" LINELEN
DEBUG>D                        ", LINE='" LINEINPUT ( 1 : LINELEN) "'"
                   END-READ
               END-IF
           END-PERFORM
           .

       900-WRAP-UP.
           CLOSE INPUT-FILE.
      D    DISPLAY "FILE GROUP  MAX=" FILE-GROUP-MAX
      D            ",FILE STATES MAX=" FILE-STATES-MAX
           DISPLAY "TOTAL ARRANGEMENTS = " ARR-TOTAL
           CONTINUE.

       END PROGRAM DAY12PT2.

