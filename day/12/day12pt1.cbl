       PROCESS NOSEQ,DS(S),AR(E),TEST(SO),CP(1047)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSTUNSTR.
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
           05  ARR-COUNT         PIC S9(09) COMP-5.
           05  ARR-TOTAL         PIC S9(09) COMP-5.

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
          05  UNASGN-TOT         PIC S9(04) COMP-5.
          05  FILE-UNASGN-MAX    PIC S9(04) COMP-5.
          05  FILE-GROUP-MAX     PIC S9(04) COMP-5.
          05  SPR-TEXT           PIC X(64).
          05  GRP-TEXT           PIC X(64).
          05  SCAN-TEXT          PIC X(64).
          05  SCAN-TEMP          PIC X(64).

       01 GROUPS-AREA.
          05  GRP-MAX            PIC S9(04) COMP-5 VALUE +0.
          05  GROUP-DATA         OCCURS 10 TIMES
                                 INDEXED BY GRPNDX.
              10  GROUP-TEXT     PIC  X(04) VALUE SPACES.
              10  GROUP-VAL      PIC S9(04) COMP-5 VALUE +0.

       01 UNASGN-AREA.
          05  UNASGN-MAX         PIC S9(04) COMP-5 VALUE +0.
          05  UNASGN-DATA        OCCURS 32 TIMES
                                 INDEXED BY UNASGNNDX.
              10  UNASGN-POS     PIC S9(04) COMP-5 VALUE +0.

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
           MOVE +0 TO FILE-UNASGN-MAX FILE-GROUP-MAX ARR-TOTAL
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
               MOVE ZERO TO SPR-TOT
               INSPECT SPR-TEXT TALLYING SPR-TOT FOR ALL "#"
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
               COMPUTE UNASGN-TOT = GRP-TOT - SPR-TOT
      D        DISPLAY "GRP-TOT=" GRP-TOT
      D                ",SPR-TOT=" SPR-TOT
      D                ",UNASGN-TOT=" UNASGN-TOT
               IF UNASGN-TOT = 0
                   ADD +1 TO ARR-TOTAL
                   READ INPUT-FILE
                       AT END SET END-OF-FILE TO TRUE
                       NOT AT END
                           ADD +1 TO LINECTR
DEBUG>D                    DISPLAY "LINE " LINECTR " LEN=" LINELEN
DEBUG>D                        ", LINE='" LINEINPUT ( 1 : LINELEN) "'"
                   END-READ
                   EXIT PERFORM CYCLE
               END-IF
               MOVE +0 TO UNASGN-MAX
               PERFORM VARYING S41 FROM 1 BY 1
                   UNTIL S41 > SPR-LEN
                   IF SPR-TEXT (S41 : 1) = "?"
                       ADD +1 TO UNASGN-MAX
                       MOVE S41 TO UNASGN-POS (UNASGN-MAX)
                   END-IF
               END-PERFORM
      D        PERFORM VARYING S41 FROM 1 BY 1
      D            UNTIL S41 > UNASGN-MAX
      D            DISPLAY "UNASGN[" S41 "]=" UNASGN-POS (S41)
      D        END-PERFORM
               MOVE UNASGN-MAX TO S91
               MOVE UNASGN-TOT TO S92
               CALL "CHOOSE"
                    USING VALUE S91, S92,
                          REFERENCE S93
      D        DISPLAY "UNRANKING (" UNASGN-MAX "," UNASGN-TOT ")"
      D            "=" S93 " COMBINATIONS"
               PERFORM VARYING U91 FROM 1 BY 1
                   UNTIL U91 > S93
                   CALL "UNRANK"
                        USING VALUE S91, S92, U91,
                              REFERENCE UNRANK-DATA
      *            ONLY UNCOMMENT THESE FOR THE TEST FILE
      D            DISPLAY "UNRANK(" UNASGN-MAX ","
      D                UNASGN-TOT "," U91 ")"
      D            DISPLAY "OUTPUT  "
      D                    " CNT " UNRKCNT
      D            PERFORM VARYING S41 FROM 1 BY 1
      D                UNTIL S41 > UNRKCNT
      D                    DISPLAY "  UNRANK " S41
      D                    " = " UNRK-VAL (S41)
      D            END-PERFORM
                   MOVE SPR-TEXT (1 : SPR-LEN) TO SCAN-TEXT
                   MOVE SPR-LEN TO SCAN-LEN
                   PERFORM VARYING UNRKNDX FROM 1 BY 1
                       UNTIL UNRKNDX > UNRKCNT
                       MOVE UNRK-VAL (UNRKNDX) TO S41
                       MOVE "#" TO SCAN-TEXT (UNASGN-POS (S41) : 1)
                   END-PERFORM
                   PERFORM 200-SCAN-SPRINGS
                   PERFORM 300-VALIDATE-SPRINGS
                   IF VALID-ARR
      D                DISPLAY "RANK " U91 " IS VALID"
      *                DISPLAY "UNRANK(" UNASGN-MAX ","
      *                    UNASGN-TOT "," U91 ")"
      *                DISPLAY "OUTPUT  "
      *                        " CNT " UNRKCNT
      *                PERFORM VARYING S41 FROM 1 BY 1
      *                    UNTIL S41 > UNRKCNT
      *                        DISPLAY "  UNRANK " S41
      *                        " = " UNRK-VAL (S41)
      *                END-PERFORM
      *                DISPLAY "SCANNED='" SCAN-TEXT (1 : SCAN-LEN) "'"
                       ADD +1 TO ARR-COUNT
      *            ELSE
      *                DISPLAY "RANK " U91 " IS NOT VALID"
                   END-IF
               END-PERFORM
               ADD ARR-COUNT TO ARR-TOTAL
DEBUG>*        DISPLAY "ARRANGEMENTS=" ARR-COUNT
      D        DISPLAY "ARRANGEMENTS OF LINE " LINECTR " = " ARR-COUNT
      D        DISPLAY "ARRANGEMENTS TOTAL               = " ARR-TOTAL
               IF UNASGN-MAX > FILE-UNASGN-MAX
                   MOVE UNASGN-MAX TO FILE-UNASGN-MAX
               END-IF
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

       200-SCAN-SPRINGS.
           MOVE +0 TO SCANCNT
           MOVE +1 TO S41
           SET SCANNDX TO 1
      D    DISPLAY "SCANNING='" SCAN-TEXT (1 : SCAN-LEN) "'"
           PERFORM UNTIL S41 > SCAN-LEN
               MOVE SPACES TO SCAN-TEMP
               UNSTRING SCAN-TEXT (1 : SCAN-LEN)
                   DELIMITED BY "." OR "?"
                   INTO SCAN-TEMP
                   COUNT   SCAN-VAL (SCANNDX)
                   POINTER S41
               END-UNSTRING
               IF SCAN-VAL (SCANNDX) > 0
      D            DISPLAY "SCAN-VAL AT " S41 " = " SCAN-VAL (SCANNDX)
      D                ",'" SCAN-TEMP (1 : SCAN-VAL (SCANNDX)) "'"
                   ADD +1 TO SCANCNT
                   SET SCANNDX UP BY 1
               END-IF
           END-PERFORM
           .

       300-VALIDATE-SPRINGS.
           SET VALID-ARR TO TRUE
           IF SCANCNT = GRP-MAX
               PERFORM VARYING SCANNDX FROM 1 BY 1
                   UNTIL SCANNDX > SCANCNT OR NOT-VALID-ARR
                   SET S41 TO SCANNDX
                   SET GRPNDX TO S41
                   IF SCAN-VAL (SCANNDX) NOT = GROUP-VAL (GRPNDX)
                       SET NOT-VALID-ARR TO TRUE
                   END-IF
               END-PERFORM
           ELSE
               SET NOT-VALID-ARR TO TRUE
           END-IF
           .

       900-WRAP-UP.
           CLOSE INPUT-FILE.
      D    DISPLAY "FILE UNASSIGNED MAX=" FILE-UNASGN-MAX
      D        ",FILE GROUP MAX=" FILE-GROUP-MAX
           DISPLAY "TOTAL ARRANGEMENTS = " ARR-TOTAL
           CONTINUE.

       END PROGRAM TSTUNSTR.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHOOSE.
       ENVIRONMENT DIVISION.
      *CONFIGURATION SECTION.
      * UNCOMMENT "WITH DEBUGGING" CLAUSE FOR DEBUG LINES TO EXECUTE.
      *SOURCE-COMPUTER.
      *    Z-SYSTEM
      *        WITH DEBUGGING MODE
      *    .
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  I       PIC S9(9) COMP-5.
       01  FACTOR            COMP-2.
       LINKAGE SECTION.
       01  N       PIC S9(9) COMP-5.
       01  K       PIC S9(9) COMP-5.
       01  RESULT  PIC S9(9) COMP-5.
       PROCEDURE DIVISION USING VALUE N, K, REFERENCE RESULT.
       COMBINATORIAL.
      *    INPUT PARAMETERS: N, K
      *    INTERNAL FIELDS : I, FACTOR
      *    RETURNS         : RESULT
           EVALUATE TRUE
               WHEN N < 0
      *            PRINT ERROR MESSAGE AND STOP PROCESSING
                   DISPLAY "COMBINATORIAL N LESS THAN ZERO=" N
               WHEN K < 0
      *            PRINT ERROR MESSAGE AND STOP PROCESSING
                   DISPLAY "COMBINATORIAL K LESS THAN ZERO=" K
               WHEN N < K
      *            SPECIAL CASE 1
      *            DISPLAY "          CHOOSE SPECIAL CASE 1"
                   MOVE  0 TO RESULT
               WHEN N = K
      *            SPECIAL CASE 2
      *            DISPLAY "          CHOOSE SPECIAL CASE 2"
                   MOVE  1 TO RESULT
               WHEN OTHER
      *            NORMAL CASES INCLUDING K = 1
                   MOVE  N TO RESULT
                   PERFORM WITH TEST BEFORE
                       VARYING I FROM 1 BY 1 UNTIL I >= K
                       COMPUTE FACTOR = (N - I) / (I + 1)
                       COMPUTE RESULT = FACTOR * RESULT
      *                DISPLAY "          CHOOSE"
      *                    " I=" I
      *                    " N=" N
      *                    " K=" K
      *                    " FACTOR=" FACTOR
      *                    " RESULT=" RESULT
                   END-PERFORM
           END-EVALUATE
      *    DISPLAY "          CHOOSE"
      *        " N=" N
      *        " K=" K
      *        " RESULT=" RESULT
           EXIT PROGRAM
           .
       END PROGRAM CHOOSE.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RANKBELO
       ENVIRONMENT DIVISION.
      *CONFIGURATION SECTION.
      * UNCOMMENT "WITH DEBUGGING" CLAUSE FOR DEBUG LINES TO EXECUTE.
      *SOURCE-COMPUTER.
      *    Z-SYSTEM
      *        WITH DEBUGGING MODE
      *    .
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  I       PIC S9(9) COMP-5.
       01  VAL     PIC S9(9) COMP-5.
       LINKAGE SECTION.
       01  N2      PIC S9(9) COMP-5.
       01  K2      PIC S9(9) COMP-5.
       01  THRESH  PIC S9(9) COMP-5.
       01  CMBVAL  PIC S9(9) COMP-5.
       PROCEDURE DIVISION USING VALUE N2, K2, THRESH, REFERENCE CMBVAL.
           COMPUTE I = N2 - 1
           CALL "CHOOSE" USING VALUE I, K2, REFERENCE CMBVAL
      *    DISPLAY "      CALCRANK " I ",K2=" K2
      *        " INIT CMBVAL=" CMBVAL
      *        " THRESH=" THRESH
           PERFORM UNTIL NOT (CMBVAL > THRESH)
               CALL "CHOOSE" USING VALUE I, K2, REFERENCE CMBVAL
               IF CMBVAL > THRESH
                   SUBTRACT 1 FROM I
               END-IF
      *        DISPLAY "        RANKLOOP " I
      *            " CMBVAL=" CMBVAL
      *            " THRESH=" THRESH
           END-PERFORM
           COMPUTE CMBVAL = I
      *    DISPLAY "      CALCRANK " I ",RESULT=" I
           EXIT PROGRAM
           .
       END PROGRAM RANKBELO.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCDUAL.
       ENVIRONMENT DIVISION.
      *CONFIGURATION SECTION.
      * UNCOMMENT "WITH DEBUGGING" CLAUSE FOR DEBUG LINES TO EXECUTE.
      *SOURCE-COMPUTER.
      *    Z-SYSTEM
      *        WITH DEBUGGING MODE
      *    .
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  REDUCEK PIC S9(9) COMP-5.
       01  DIMRANK PIC S9(9) COMP-5.
       01  I       PIC S9(9) COMP-5.
       01  SUBRANK PIC S9(9) COMP-5.
       LINKAGE SECTION.
       01  N       PIC S9(9) COMP-5.
       01  K       PIC S9(9) COMP-5.
       01  RANK    PIC S9(9) COMP-5.
       01  RESULT.
           05  RSLTCNT       PIC  9(9) COMP-5.
           05  RSLT-ITEM     OCCURS 160 TIMES.
               10  RSLT-VAL  PIC S9(9) COMP-5.
       PROCEDURE DIVISION USING VALUE N, K, RANK, REFERENCE RESULT.
      *    INPUT PARAMETERS: N, K, RANK
      *    INTERNAL FIELDS : REDUCEK, DIMRANK, SUBRANK
      *    RETURNS         : RESULT (1 : K)
      *    DISPLAY "  CALCDUAL"
      *        " N=" N
      *        " K=" K
      *        " RANK=" RANK
           MOVE K TO REDUCEK RSLTCNT
           MOVE RANK TO DIMRANK RSLT-VAL (1)
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > K
      *        DISPLAY "    CMBOLOOP1A " I
      *            " DIMRANK=" DIMRANK
      *            " REDUCEK=" REDUCEK
               CALL "RANKBELO" USING VALUE N, REDUCEK, DIMRANK,
                                          REFERENCE RSLT-VAL (I)
      *        DISPLAY "    CMBOLOOP1B " I
      *            " B4RANK=" RSLT-VAL (I)
               CALL "CHOOSE" USING VALUE RSLT-VAL (I), REDUCEK,
                                   REFERENCE SUBRANK
               COMPUTE DIMRANK = DIMRANK - SUBRANK
               SUBTRACT 1 FROM REDUCEK
      *        DISPLAY "    CMBOLOOP1C " I
      *            " AFRANK=" RSLT-VAL (I)
      *            " NEWRANK=" DIMRANK
      *            " NEWK=" REDUCEK
           END-PERFORM
           EXIT PROGRAM
           .
       END PROGRAM CALCDUAL.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNRANK.
       ENVIRONMENT DIVISION.
      *CONFIGURATION SECTION.
      * UNCOMMENT "WITH DEBUGGING" CLAUSE FOR DEBUG LINES TO EXECUTE.
      *SOURCE-COMPUTER.
      *    Z-SYSTEM
      *        WITH DEBUGGING MODE
      *    .
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  DUALOF0 PIC S9(9) COMP-5.
       01  DUAL    PIC S9(9) COMP-5.
       01  TEMP1   PIC S9(9) COMP-5.
       01  I       PIC S9(9) COMP-5.
       LINKAGE SECTION.
       01  N       PIC S9(9) COMP-5.
       01  K       PIC S9(9) COMP-5.
       01  RANK    PIC S9(9) COMP-5.
       01  RESULT.
           05  RSLTCNT       PIC  9(9) COMP-5.
           05  RSLT-ITEM     OCCURS 160 TIMES.
               10  RSLT-VAL  PIC S9(9) COMP-5.
       PROCEDURE DIVISION USING VALUE N, K, RANK, REFERENCE RESULT.
       UNRANK-PROCESS.
      *    INPUT PARAMETERS: N, K, RANK
      *    INTERNAL FIELDS : DUALOF0, DUAL, TEMP1, I
      *    RETURNS         : RESULT (1 : K)
      *    DISPLAY "UNRANK"
      *        " N=" N
      *        " K=" K
      *        " RANK=" RANK
      *        " RSLTCNT=" RSLTCNT
           COMPUTE DUALOF0 = N - 1
           COMPUTE RANK = RANK - 1
           CALL "CHOOSE" USING VALUE N, K, REFERENCE TEMP1
           COMPUTE DUAL = TEMP1 - 1 - RANK
      *    DISPLAY "UNRANK"
      *        " TMP1=" TEMP1
      *        " DUAL=" DUAL
           CALL "CALCDUAL" USING VALUE N, K, DUAL, REFERENCE RESULT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > RSLTCNT
               COMPUTE RSLT-VAL (I) = DUALOF0 - RSLT-VAL (I)
               ADD +1 TO RSLT-VAL (I)
           END-PERFORM
           GOBACK
           .
       END PROGRAM UNRANK.
