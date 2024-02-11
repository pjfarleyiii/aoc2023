       PROCESS NOSEQ,DS(S),AR(E),TEST(SO),CP(1047)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY19PT2.
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
           05  LINELEN                   PIC  9(04) COMP-5.
           05  LINECTR                   PIC S9(04) COMP-5 VALUE +0.
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

       01 CALLED-PROGRAMS.
           05  CEE3ABD                   PIC  X(08) VALUE "CEE3ABD".

       01 WORK-FLAGS.
          05 CHANGED-FLAG                PIC X.
             88 HASCHANGED                     VALUE 'Y'.
             88 HASNOTCHANGED                  VALUE 'N'.
          05 WKFL-DONE-FLAG                PIC X.
             88 WORKFLOW-DONE                  VALUE 'Y'.
             88 NOT-WORKFLOW-DONE              VALUE 'N'.

       01 WORK-AREAS.
          05  RTNG-TOTAL                 PIC S9(18) COMP-5 VALUE +0.
          05  CQUE-TOTAL                 PIC S9(18) COMP-5 VALUE +0.
          05  ABEND-CODE                 PIC S9(09) COMP-5 VALUE +1040.
          05  ABEND-FORMAT               PIC S9(09) COMP-5 VALUE +1.
          05  WKFL-STRLEN                PIC S9(04) COMP-5 VALUE +0.
          05  WKFL-STRING                PIC  X(64).
          05  WKFL-NEXTRULE              PIC  X(04).
          05  WKFL-CRITRULE              PIC  X(04).
          05  UNSTR-DELIMS.
              10  UNSTR-DELIM            OCCURS 10 TIMES
                                         PIC  X     VALUE SPACE.
          05  SCAN-TEXT                  PIC X(64).
          05  RULE-TEXT                  PIC X(64).
          05  CRIT-VALUES                PIC X(04)  VALUE "XMAS".
          05  CQUE-DMSG                  PIC X(06)  VALUE SPACES.

       01  SCAN-AREA.
           05  SCANCNT                   PIC S9(04) COMP-5 VALUE 0.
           05  SCANPTR                   PIC S9(04) COMP-5 VALUE 0.
           05  SCAN-STRLEN               PIC S9(04) COMP-5 VALUE +0.
           05  SCAN-DATA                 OCCURS 32 TIMES
                                         INDEXED BY SCANNDX.
               10  SCAN-LEN              PIC S9(04) COMP-5 VALUE 0.
               10  SCAN-TXT              PIC  X(64).

       01  RULE-AREA.
           05  RULECNT                   PIC S9(04) COMP-5 VALUE 0.
           05  RULEPTR                   PIC S9(04) COMP-5 VALUE 0.
           05  RULE-STRLEN               PIC S9(04) COMP-5 VALUE +0.
           05  RULE-DATA                 OCCURS 32 TIMES
                                         INDEXED BY RULENDX.
               10  RULE-LEN              PIC S9(04) COMP-5 VALUE 0.
               10  RULE-TXT              PIC  X(64).

       01 WORKFLOW-AREA.
          05  WKFLMAX                    PIC S9(04) COMP-5 VALUE +0.
          05  WKFLSUB                    PIC S9(04) COMP-5 VALUE +0.
          05  WKCRSUB                    PIC S9(04) COMP-5 VALUE +0.
          05  WORKFLOW-DATA.
              10  WORKFLOW-TABLE         OCCURS 1024 TIMES
                                         INDEXED BY WKFLNDX.
                  15  WKFL-COUNT         PIC S9(04) COMP-5 VALUE +0.
                  15  WKFL-NAME          PIC  X(04) VALUE SPACES.
                  15  WKFL-CRIT-TABLE    OCCURS 8 TIMES
                                         INDEXED BY WKCRNDX.
                      20  WKFL-VALUE     PIC S9(04) COMP-5 VALUE +0.
                      20  WKFL-CNDX      PIC S9(04) COMP-5 VALUE +0.
                      20  WKFL-COND      PIC  X     VALUE SPACE.
                      20  WKFL-CRIT      PIC  X     VALUE SPACE.
                      20  WKFL-RULE      PIC  X(04) VALUE SPACES.

       01  CURR-FLOW-DATA.
                      20  CFLO-VALUE     PIC S9(04) COMP-5 VALUE +0.
                      20  CFLO-CNDX      PIC S9(04) COMP-5 VALUE +0.
                      20  CFLO-COND      PIC  X     VALUE SPACE.
                      20  CFLO-CRIT      PIC  X     VALUE SPACE.
                      20  CFLO-RULE      PIC  X(04) VALUE SPACES.

       01  RANGE-QUEUE-AREA.
           05  RQUEMAX                   PIC S9(04) COMP-5 VALUE +0.
           05  RQUESUB                   PIC S9(04) COMP-5 VALUE +0.
           05  RNGQUE-DATA.
               10  RNGQUE-TABLE          OCCURS 1024 TIMES
                                         INDEXED BY RQUENDX.
                   15  RQUE-NAME         PIC  X(04) VALUE SPACES.
                   15  RANGES-TABLE      OCCURS 4 TIMES
                                         INDEXED BY RANGNDX.
                       20  RANG-LO       PIC S9(04) COMP-5 VALUE +0.
                       20  RANG-HI       PIC S9(04) COMP-5 VALUE +0.

       01  CURR-QUEUE-DATA.
                   15  CQUE-NAME         PIC  X(04) VALUE SPACES.
                   15  CURRQUE-TABLE     OCCURS 4 TIMES
                                         INDEXED BY CQUENDX.
                       20  CQUE-LO       PIC S9(04) COMP-5 VALUE +0.
                       20  CQUE-HI       PIC S9(04) COMP-5 VALUE +0.

       01  CUR2-QUEUE-DATA.
                   15  CQU2-NAME         PIC  X(04) VALUE SPACES.
                   15  CUR2QUE-TABLE     OCCURS 4 TIMES
                                         INDEXED BY CQU2NDX.
                       20  CQU2-LO       PIC S9(04) COMP-5 VALUE +0.
                       20  CQU2-HI       PIC S9(04) COMP-5 VALUE +0.

       01  INTERVALS-DATA.
                       20  NTVL-LO       PIC S9(04) COMP-5 VALUE +0.
                       20  NTVL-HI       PIC S9(04) COMP-5 VALUE +0.

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
DEBUG>D            DISPLAY "LINE " LINECTR " LEN=" LINELEN
DEBUG>D                ", LINE='" LINEINPUT ( 1 : LINELEN) "'"
           END-READ
           .

       100-PROCESS-INPUT-DATA.
           SET NOT-WORKFLOW-DONE TO TRUE
           PERFORM UNTIL END-OF-FILE
      *        FIRST DECIPHER WORK FLOW INPUT
               IF LINELEN = +0
                   SET WORKFLOW-DONE TO TRUE
                   PERFORM 050-READ-INPUT-DATA UNTIL END-OF-FILE
               END-IF
               IF NOT-WORKFLOW-DONE
                   ADD +1 TO WKFLMAX
                   SET WKFLNDX TO WKFLMAX
                   UNSTRING LINEINPUT (1 : LINELEN)
                       DELIMITED BY "{" OR "}" INTO
                       WKFL-NAME (WKFLNDX)
                       WKFL-STRING  COUNT IN WKFL-STRLEN
                       UNSTR-DELIM (1)
                   END-UNSTRING
      D            DISPLAY "WORKFLOW[" WKFLMAX "]"
      D                " NAME=" WKFL-NAME (WKFLNDX)
      D                ",CRITERIA=[" WKFL-STRLEN "]='"
      D                WKFL-STRING (1 : WKFL-STRLEN) "'"
      D                ",DELIM='" UNSTR-DELIM (1) "'"
                   MOVE WKFL-STRING (1 : WKFL-STRLEN) TO SCAN-TEXT
                   MOVE WKFL-STRLEN TO SCAN-STRLEN
                   PERFORM 800-SCAN-COMMA
      D            DISPLAY "SCANCNT=" SCANCNT
                   SET WKCRNDX TO 1
                   MOVE +0 TO WKFL-COUNT (WKFLNDX)
                   PERFORM VARYING S41 FROM 1 BY 1 UNTIL S41 > SCANCNT
      D                DISPLAY "SCANTXT[" S41 "]=L(" SCAN-LEN (S41)
      D                    ")='" SCAN-TXT (S41) (1 : SCAN-LEN (S41))
      D                    "'"
                       MOVE SCAN-TXT (S41) (1 : SCAN-LEN (S41))
                         TO RULE-TEXT
                       MOVE SCAN-LEN (S41) TO RULE-STRLEN
                       PERFORM 810-SCAN-RULE
      D                DISPLAY "RULECNT=" RULECNT
                       ADD +1 TO WKFL-COUNT (WKFLNDX)
                       MOVE SPACES TO WKFL-COND  (WKFLNDX WKCRNDX)
                                      WKFL-CRIT  (WKFLNDX WKCRNDX)
                                      WKFL-RULE  (WKFLNDX WKCRNDX)
                       MOVE +0     TO WKFL-VALUE (WKFLNDX WKCRNDX)
                       SET S44 TO WKCRNDX
                       PERFORM VARYING S42 FROM 1 BY 1
                           UNTIL S42 > RULECNT
                           MOVE RULE-LEN (S42) TO S43
      D                    DISPLAY "RULETXT[" S42 "]=L(" S43
      D                        ")='" RULE-TXT (S42) (1 : S43)
      D                        "'"
                           EVALUATE UNSTR-DELIM (S42)
                               WHEN "<"
                               WHEN ">"
                                   MOVE RULE-TXT (S42) (1 : S43)
                                     TO WKFL-CRIT (WKFLNDX WKCRNDX)
                                   MOVE +1
                                     TO WKFL-CNDX (WKFLNDX WKCRNDX)
                                   INSPECT CRIT-VALUES TALLYING
                                       WKFL-CNDX (WKFLNDX WKCRNDX)
                                       FOR CHARACTERS BEFORE
                                       FUNCTION UPPER-CASE (
                                       RULE-TXT (S42) (1 : S43) )
                                   MOVE UNSTR-DELIM (S42)
                                     TO WKFL-COND     (WKFLNDX WKCRNDX)
                               WHEN ":"
                                   COMPUTE
                                       WKFL-VALUE (WKFLNDX WKCRNDX) =
                                       FUNCTION NUMVAL (
                                       RULE-TXT (S42) (1 : S43) )
                               WHEN OTHER
                                   MOVE RULE-TXT (S42) (1 : S43)
                                     TO WKFL-RULE (WKFLNDX WKCRNDX)
                           END-EVALUATE
                       END-PERFORM
                       SET WKCRNDX UP BY 1
                   END-PERFORM
      D            DISPLAY "WORKFLOW[" WKFLMAX "]"
      D                " NAME=" WKFL-NAME (WKFLNDX)
      D                ",CRITCNT=" WKFL-COUNT (WKFLNDX)
      D            PERFORM VARYING WKCRNDX FROM 1 BY 1
      D                UNTIL WKCRNDX > WKFL-COUNT (WKFLNDX)
      D                SET S41 TO WKCRNDX
      D                DISPLAY "    CRIT[" WKFLMAX "," S41 "],CRIT="
      D                    WKFL-CRIT  (WKFLNDX WKCRNDX) ",CNDX="
      D                    WKFL-CNDX  (WKFLNDX WKCRNDX) ",COND="
      D                    WKFL-COND  (WKFLNDX WKCRNDX) ",VAL="
      D                    WKFL-VALUE (WKFLNDX WKCRNDX) ",RULE='"
      D                    WKFL-RULE  (WKFLNDX WKCRNDX) "'"
      D            END-PERFORM
               ELSE
      *            FOR PART 2, PART RATINGS ARE IGNORED
                   CONTINUE
               END-IF
               IF NOT END-OF-FILE
                   PERFORM 050-READ-INPUT-DATA
               END-IF
           END-PERFORM
      D    PERFORM 200-SHOW-WKFL
      *    SET UP INITIAL RANGE QUEUE
           MOVE +1 TO RQUEMAX
           MOVE FUNCTION LOWER-CASE ("IN  ") TO RQUE-NAME (1)
           PERFORM VARYING RANGNDX FROM 1 BY 1 UNTIL RANGNDX > 4
               MOVE +1    TO RANG-LO (1 RANGNDX)
               MOVE +4000 TO RANG-HI (1 RANGNDX)
           END-PERFORM
      D    PERFORM 210-SHOW-RNGQ
      *    LOOP UNTIL THE QUEUE IS EMPTY
           PERFORM UNTIL RQUEMAX = 0
      *        POP-RNGQ POPULATES CURR-QUEUE-DATA
               PERFORM 220-POP-RNGQ
               IF CQUE-NAME = "A   " OR "R   "
      D            DISPLAY "    CASE 1:CURR='" CQUE-NAME "'"
                   IF CQUE-NAME = "A   "
      D                MOVE "PRODNG" TO CQUE-DMSG
      D                PERFORM 240-SHOW-CQUE
                       MOVE +1 TO CQUE-TOTAL
                       PERFORM VARYING CQUENDX FROM 1 BY 1
                           UNTIL CQUENDX > 4
                           COMPUTE CQUE-TOTAL = CQUE-TOTAL *
                             (CQUE-HI (CQUENDX) - CQUE-LO (CQUENDX) + 1)
                       END-PERFORM
                       ADD CQUE-TOTAL TO RTNG-TOTAL
      D                DISPLAY "TOTAL +=" CQUE-TOTAL "=" RTNG-TOTAL
                   END-IF
                   EXIT PERFORM CYCLE
               END-IF
               SET NOT-WORKFLOW-DONE TO TRUE
               MOVE CQUE-NAME TO WKFL-NEXTRULE
               PERFORM 110-SRCH-WKFL
      *        PERFORM VARYING WKFLNDX FROM 1 BY 1
      *            UNTIL WKFLNDX >= WKFLMAX OR WORKFLOW-DONE
      *            SET WKFLSUB TO WKFLNDX
                   PERFORM VARYING WKCRNDX FROM 1 BY 1
                       UNTIL WKCRNDX >= WKFL-COUNT (WKFLNDX)
                       MOVE WKFL-CRIT-TABLE (WKFLNDX WKCRNDX)
                         TO CURR-FLOW-DATA
                       MOVE CURRQUE-TABLE (CFLO-CNDX)
                         TO INTERVALS-DATA
      D                MOVE "CHECKG" TO CQUE-DMSG
      D                PERFORM 240-SHOW-CQUE
      D                DISPLAY "NTVL-LO=" NTVL-LO ",NTVL-HI=" NTVL-HI
      *                ALL PASSTHROUGH, NO TRANSFER
                       IF (CFLO-COND = ">" AND CFLO-VALUE >= NTVL-HI)
                       OR (CFLO-COND = "<" AND CFLO-VALUE <= NTVL-LO)
      D                    MOVE +2 TO S48
      D                    PERFORM 230-SHOW-CFLO
                           EXIT PERFORM CYCLE
                       END-IF
      *                ALL TRANSFER, NO PASSTHROUGH
                       IF (CFLO-COND = ">" AND CFLO-VALUE < NTVL-LO)
                       OR (CFLO-COND = "<" AND CFLO-VALUE > NTVL-HI)
      D                    MOVE +3 TO S48
      D                    PERFORM 230-SHOW-CFLO
                           MOVE CFLO-RULE TO CQUE-NAME
      D                    MOVE "ADDING" TO CQUE-DMSG
      D                    PERFORM 240-SHOW-CQUE
                           ADD +1 TO RQUEMAX
                           IF RQUEMAX > +64
                               DISPLAY "QUEUE DEPTH EXCEEDED #1"
      D                            ",NAME=" CQUE-NAME
      D                            ",(" CQUE-LO (1)
      D                            ","  CQUE-HI (1) ")"
      D                            ",(" CQUE-LO (2)
      D                            ","  CQUE-HI (2) ")"
      D                            ",(" CQUE-LO (3)
      D                            ","  CQUE-HI (3) ")"
      D                            ",(" CQUE-LO (4)
      D                            ","  CQUE-HI (4) ")"
                               CALL CEE3ABD USING ABEND-CODE,
                                                  ABEND-FORMAT
                           END-IF
                           SET RQUENDX TO RQUEMAX
                           MOVE CURR-QUEUE-DATA
                             TO RNGQUE-TABLE (RQUENDX)
      D                    PERFORM 210-SHOW-RNGQ
                           SET WORKFLOW-DONE TO TRUE
                           EXIT PERFORM
                       END-IF
      *                SOME TRANSFER, SOME PASSTHROUGH
                       MOVE CURR-QUEUE-DATA TO CUR2-QUEUE-DATA
      D                MOVE "BEFOR2" TO CQUE-DMSG
      D                PERFORM 250-SHOW-CQU2
                       IF CFLO-COND = ">"
                           COMPUTE CQU2-LO (CFLO-CNDX) = CFLO-VALUE + 1
                           COMPUTE CQUE-HI (CFLO-CNDX) = CFLO-VALUE
                       ELSE
                           COMPUTE CQU2-HI (CFLO-CNDX) = CFLO-VALUE - 1
                           COMPUTE CQUE-LO (CFLO-CNDX) = CFLO-VALUE
                       END-IF
                       MOVE CFLO-RULE TO CQU2-NAME
      D                MOVE +4 TO S48
      D                PERFORM 230-SHOW-CFLO
      D                MOVE "MODIFY" TO CQUE-DMSG
      D                PERFORM 240-SHOW-CQUE
      D                MOVE +5 TO S48
      D                MOVE "ADDING" TO CQUE-DMSG
      D                PERFORM 250-SHOW-CQU2
                       ADD +1 TO RQUEMAX
                       IF RQUEMAX > +64
                           DISPLAY "QUEUE DEPTH EXCEEDED #2"
      D                        ",NAME=" CQUE-NAME
      D                        ",(" CQUE-LO (1)
      D                        ","  CQUE-HI (1) ")"
      D                        ",(" CQUE-LO (2)
      D                        ","  CQUE-HI (2) ")"
      D                        ",(" CQUE-LO (3)
      D                        ","  CQUE-HI (3) ")"
      D                        ",(" CQUE-LO (4)
      D                        ","  CQUE-HI (4) ")"
                           CALL CEE3ABD USING ABEND-CODE,
                                              ABEND-FORMAT
                       END-IF
                       SET RQUENDX TO RQUEMAX
                       MOVE CUR2-QUEUE-DATA TO RNGQUE-TABLE (RQUENDX)
      D                PERFORM 210-SHOW-RNGQ
                   END-PERFORM
                   IF NOT-WORKFLOW-DONE
                       SET WKCRNDX TO WKFL-COUNT (WKFLNDX)
                       MOVE WKFL-RULE (WKFLNDX WKCRNDX) TO CQUE-NAME
      D                MOVE +6 TO S48
      D                PERFORM 230-SHOW-CFLO
      D                MOVE "ADDING" TO CQUE-DMSG
      D                PERFORM 240-SHOW-CQUE
                       ADD +1 TO RQUEMAX
                       IF RQUEMAX > +64
                           DISPLAY "QUEUE DEPTH EXCEEDED #2"
      D                        ",NAME=" CQUE-NAME
      D                        ",(" CQUE-LO (1)
      D                        ","  CQUE-HI (1) ")"
      D                        ",(" CQUE-LO (2)
      D                        ","  CQUE-HI (2) ")"
      D                        ",(" CQUE-LO (3)
      D                        ","  CQUE-HI (3) ")"
      D                        ",(" CQUE-LO (4)
      D                        ","  CQUE-HI (4) ")"
                           CALL CEE3ABD USING ABEND-CODE,
                                              ABEND-FORMAT
                       END-IF
                       SET RQUENDX TO RQUEMAX
                       MOVE CURR-QUEUE-DATA TO RNGQUE-TABLE (RQUENDX)
      D                PERFORM 210-SHOW-RNGQ
                   END-IF
      *        END-PERFORM
           END-PERFORM
           .

       110-SRCH-WKFL.
           SET WKFLNDX TO 1
           SEARCH WORKFLOW-TABLE
               AT END
                   DISPLAY "RULE '" WKFL-NEXTRULE
                       "', NOT FOUND, ABORTING"
                   CALL CEE3ABD USING ABEND-CODE, ABEND-FORMAT
               WHEN WKFL-NEXTRULE = WKFL-NAME (WKFLNDX)
      *             FUNCTION UPPER-CASE (WKFL-NAME (WKFLNDX))
                   SET WKFLSUB TO WKFLNDX
           END-SEARCH
      D    DISPLAY "RULE '" WKFL-NEXTRULE "' FOUND AT "
      D        WKFLSUB ",CRITCNT=" WKFL-COUNT (WKFLSUB)
           SET WKFLNDX TO WKFLSUB
           .

      D200-SHOW-WKFL.
      D    PERFORM VARYING WKFLNDX FROM 1 BY 1
      D        UNTIL WKFLNDX > WKFLMAX
      D        SET WKFLSUB TO WKFLNDX
      D        DISPLAY "WORKFLOW[" WKFLSUB "]"
      D            " NAME=" WKFL-NAME (WKFLNDX)
      D            ",CRITCNT=" WKFL-COUNT (WKFLNDX)
      D        PERFORM VARYING WKCRNDX FROM 1 BY 1
      D            UNTIL WKCRNDX > WKFL-COUNT (WKFLNDX)
      D            SET WKCRSUB TO WKCRNDX
      D            DISPLAY "    CRIT[" WKCRSUB "],CRIT="
      D                WKFL-CRIT  (WKFLNDX WKCRNDX) ",CNDX="
      D                WKFL-CNDX  (WKFLNDX WKCRNDX) ",COND="
      D                WKFL-COND  (WKFLNDX WKCRNDX) ",VAL="
      D                WKFL-VALUE (WKFLNDX WKCRNDX) ",RULE='"
      D                WKFL-RULE  (WKFLNDX WKCRNDX) "'"
      D        END-PERFORM
      D    END-PERFORM
      D    .

      D210-SHOW-RNGQ.
      D    PERFORM VARYING RQUENDX FROM 1 BY 1
      D        UNTIL RQUENDX > RQUEMAX
      D        SET RQUESUB TO RQUENDX
      D        DISPLAY "RANGEQUE[" RQUESUB "]"
      D            ",NAME=" RQUE-NAME (RQUENDX)
      D            ",(" RANG-LO (RQUENDX 1)
      D            ","  RANG-HI (RQUENDX 1) ")"
      D            ",(" RANG-LO (RQUENDX 2)
      D            ","  RANG-HI (RQUENDX 2) ")"
      D            ",(" RANG-LO (RQUENDX 3)
      D            ","  RANG-HI (RQUENDX 3) ")"
      D            ",(" RANG-LO (RQUENDX 4)
      D            ","  RANG-HI (RQUENDX 4) ")"
      D    END-PERFORM
      D    .

       220-POP-RNGQ.
           MOVE RNGQUE-TABLE (RQUEMAX) TO CURR-QUEUE-DATA
           SUBTRACT 1 FROM RQUEMAX
      D    MOVE "POPPED" TO CQUE-DMSG
      D    PERFORM 240-SHOW-CQUE
           .

      D230-SHOW-CFLO.
      D    DISPLAY "    CASE " S48
      D        ",NAME=" CQUE-NAME ",CRIT="
      D        CFLO-CRIT  ",CNDX="
      D        CFLO-CNDX  ",COND="
      D        CFLO-COND  ",VAL="
      D        CFLO-VALUE ",RULE='"
      D        CFLO-RULE  "'"
      D    .

      D240-SHOW-CQUE.
      D    DISPLAY "QUEUE ITEM " CQUE-DMSG
      D        ",NAME=" CQUE-NAME
      D        ",(" CQUE-LO (1)
      D        ","  CQUE-HI (1) ")"
      D        ",(" CQUE-LO (2)
      D        ","  CQUE-HI (2) ")"
      D        ",(" CQUE-LO (3)
      D        ","  CQUE-HI (3) ")"
      D        ",(" CQUE-LO (4)
      D        ","  CQUE-HI (4) ")"
      D    .

      D250-SHOW-CQU2.
      D    DISPLAY "QUEUE ITEM " CQUE-DMSG
      D        ",NAME=" CQU2-NAME
      D        ",(" CQU2-LO (1)
      D        ","  CQU2-HI (1) ")"
      D        ",(" CQU2-LO (2)
      D        ","  CQU2-HI (2) ")"
      D        ",(" CQU2-LO (3)
      D        ","  CQU2-HI (3) ")"
      D        ",(" CQU2-LO (4)
      D        ","  CQU2-HI (4) ")"
      D    .

       800-SCAN-COMMA.
           MOVE +0 TO SCANCNT
           MOVE +1 TO SCANPTR
           SET SCANNDX TO 1
      D    DISPLAY "COMMA SCANNING='" SCAN-TEXT (1 : SCAN-STRLEN) "'"
           PERFORM UNTIL SCANPTR > SCAN-STRLEN
               MOVE SPACES TO SCAN-TXT (SCANNDX)
               UNSTRING SCAN-TEXT (1 : SCAN-STRLEN)
                   DELIMITED BY ","
                   INTO    SCAN-TXT (SCANNDX)
                   COUNT   SCAN-LEN (SCANNDX)
                   POINTER SCANPTR
               END-UNSTRING
               IF SCAN-LEN (SCANNDX) > 0
      D            DISPLAY "SCAN-LEN AT " SCANPTR
      D                " = " SCAN-LEN (SCANNDX)
      D                ",'" SCAN-TXT (SCANNDX) (1 : SCAN-LEN (SCANNDX))
      D                "'"
                   ADD +1 TO SCANCNT
                   SET SCANNDX UP BY 1
               END-IF
           END-PERFORM
           .

       810-SCAN-RULE.
           MOVE +0 TO RULECNT
           MOVE +1 TO RULEPTR
           SET RULENDX TO 1
           MOVE SPACES TO UNSTR-DELIMS
      D    DISPLAY "RULE  SCANNING='" RULE-TEXT (1 : RULE-STRLEN) "'"
           PERFORM UNTIL RULEPTR > RULE-STRLEN
               MOVE SPACES TO RULE-TXT (RULENDX)
               UNSTRING RULE-TEXT (1 : RULE-STRLEN)
                   DELIMITED BY "<" OR ">" OR ":"
                   INTO    RULE-TXT (RULENDX)
                   DELIMITER IN UNSTR-DELIM (RULECNT + 1)
                   COUNT   RULE-LEN (RULENDX)
                   POINTER RULEPTR
               END-UNSTRING
               IF RULE-LEN (RULENDX) > 0
                   ADD +1 TO RULECNT
      D            DISPLAY "RULE-LEN AT " RULEPTR
      D                " = " RULE-LEN (RULENDX)
      D                ",'" RULE-TXT (RULENDX) (1 : RULE-LEN (RULENDX))
      D                "',DELIM[" RULECNT "]='"
      D                UNSTR-DELIM (RULECNT) "'"
                   SET RULENDX UP BY 1
               END-IF
           END-PERFORM
           .

       900-WRAP-UP.
           CLOSE INPUT-FILE.
           DISPLAY "PART 2=" RTNG-TOTAL
           CONTINUE.

       END PROGRAM DAY19PT2.
