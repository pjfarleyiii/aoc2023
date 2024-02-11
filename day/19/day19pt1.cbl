       PROCESS NOSEQ,DS(S),AR(E),TEST(SO),CP(1047)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY19PT1.
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
          05  ABEND-CODE                 PIC S9(09) COMP-5 VALUE +1040.
          05  ABEND-FORMAT               PIC S9(09) COMP-5 VALUE +1.
          05  RTNG-TOTAL                 PIC S9(09) COMP-5 VALUE +0.
          05  WKFL-STRLEN                PIC S9(04) COMP-5 VALUE +0.
          05  RTNG-STRLEN                PIC S9(04) COMP-5 VALUE +0.
          05  WKFL-STRING                PIC  X(64).
          05  WKFL-NEXTRULE              PIC  X(04).
          05  WKFL-CRITRULE              PIC  X(04).
          05  RTNG-STRING                PIC  X(64).
          05  UNSTR-DELIMS.
              10  UNSTR-DELIM            OCCURS 10 TIMES
                                         PIC  X     VALUE SPACE.
          05  SCAN-TEXT                  PIC X(64).
          05  RULE-TEXT                  PIC X(64).
          05  RTNG-TEXT                  PIC X(64).

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

       01 RTNGSTR-AREA.
          05  RTNGCNT                    PIC S9(04) COMP-5 VALUE 0.
          05  RTNGPTR                    PIC S9(04) COMP-5 VALUE 0.
          05  RTNGLEN-DATA.
              10  RTNGLEN                OCCURS 4 TIMES
                                         PIC S9(04) COMP-5 VALUE +0.
          05  RTNGSTR-DATA.
              10  RTNGSTR                OCCURS 4 TIMES
                                         PIC  X     VALUE SPACE.

       01 RATINGS-AREA.
          05  RTNGMAX                    PIC S9(04) COMP-5 VALUE +0.
          05  RTNGSUB                    PIC S9(04) COMP-5 VALUE +0.
          05  RATINGS-DATA.
              10  FILLER                 OCCURS 1024 TIMES
                                         INDEXED BY RTNGNDX.
                  15  RTNG-T             PIC S9(09) COMP-5 VALUE +0.
                  15  RTNG-X             PIC S9(04) COMP-5 VALUE +0.
                  15  RTNG-M             PIC S9(04) COMP-5 VALUE +0.
                  15  RTNG-A             PIC S9(04) COMP-5 VALUE +0.
                  15  RTNG-S             PIC S9(04) COMP-5 VALUE +0.

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
                      20  WKFL-COND      PIC  X     VALUE SPACE.
                      20  WKFL-CRIT      PIC  X     VALUE SPACE.
                      20  WKFL-RULE      PIC  X(04) VALUE SPACES.

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
                   MOVE LOW-VALUES TO RATINGS-DATA
                   MOVE +0 TO RTNGMAX
                   SET RTNGNDX TO 1
                   PERFORM 050-READ-INPUT-DATA
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
      D                    WKFL-CRIT  (WKFLNDX WKCRNDX) ",COND="
      D                    WKFL-COND  (WKFLNDX WKCRNDX) ",VAL="
      D                    WKFL-VALUE (WKFLNDX WKCRNDX) ",RULE='"
      D                    WKFL-RULE  (WKFLNDX WKCRNDX) "'"
      D            END-PERFORM
               ELSE
      *            PROCESS PART RATINGS
                   ADD +1 TO RTNGMAX
                   SET RTNGNDX TO RTNGMAX
                   UNSTRING LINEINPUT (1 : LINELEN)
                       DELIMITED BY "{" OR "}" INTO
                       UNSTR-DELIM (1)
                       RTNG-STRING  COUNT IN RTNG-STRLEN
                       UNSTR-DELIM (1)
                   END-UNSTRING
      D            DISPLAY "PARTRTNG[" RTNGMAX "]"
      D                ",RATINGS=[" RTNG-STRLEN "]='"
      D                RTNG-STRING (1 : RTNG-STRLEN) "'"
      D                ",DELIM='" UNSTR-DELIM (1) "'"
                   MOVE RTNG-STRING (1 : RTNG-STRLEN) TO SCAN-TEXT
                   MOVE RTNG-STRLEN TO SCAN-STRLEN
                   PERFORM 800-SCAN-COMMA
      D            DISPLAY "SCANCNT=" SCANCNT
                   PERFORM VARYING S41 FROM 1 BY 1 UNTIL S41 > SCANCNT
      D                DISPLAY "SCANTXT[" S41 "]=L(" SCAN-LEN (S41)
      D                    ")='" SCAN-TXT (S41) (1 : SCAN-LEN (S41))
      D                    "'"
                       COMPUTE S42 = SCAN-LEN (S41) - 2
                       EVALUATE TRUE
                           WHEN FUNCTION UPPER-CASE (
                                SCAN-TXT (S41) (1 : 1) ) = "X"
                                COMPUTE RTNG-X (RTNGNDX) =
                                    FUNCTION NUMVAL (
                                    SCAN-TXT (S41) (3 : S42) )
                                ADD RTNG-X (RTNGNDX) TO RTNG-T (RTNGNDX)
                           WHEN FUNCTION UPPER-CASE (
                                SCAN-TXT (S41) (1 : 1) ) = "M"
                                COMPUTE RTNG-M (RTNGNDX) =
                                    FUNCTION NUMVAL (
                                    SCAN-TXT (S41) (3 : S42) )
                                ADD RTNG-M (RTNGNDX) TO RTNG-T (RTNGNDX)
                           WHEN FUNCTION UPPER-CASE (
                                SCAN-TXT (S41) (1 : 1) ) = "A"
                                COMPUTE RTNG-A (RTNGNDX) =
                                    FUNCTION NUMVAL (
                                    SCAN-TXT (S41) (3 : S42) )
                                ADD RTNG-A (RTNGNDX) TO RTNG-T (RTNGNDX)
                           WHEN FUNCTION UPPER-CASE (
                                SCAN-TXT (S41) (1 : 1) ) = "S"
                                COMPUTE RTNG-S (RTNGNDX) =
                                    FUNCTION NUMVAL (
                                    SCAN-TXT (S41) (3 : S42) )
                                ADD RTNG-S (RTNGNDX) TO RTNG-T (RTNGNDX)
                       END-EVALUATE
                       CONTINUE
                   END-PERFORM
                   CONTINUE
               END-IF
               IF NOT END-OF-FILE
                   PERFORM 050-READ-INPUT-DATA
               END-IF
           END-PERFORM
      D    PERFORM 200-SHOW-WKFL
      D    PERFORM 210-SHOW-RTNG
           PERFORM VARYING RTNGNDX FROM 1 BY 1
               UNTIL RTNGNDX > RTNGMAX
               SET RTNGSUB TO RTNGNDX
      D        SET RTNGNDX TO RTNGSUB
      D        DISPLAY "MATCHING RTNG[" RTNGSUB "],X="
      D            RTNG-X (RTNGNDX) ",M="
      D            RTNG-M (RTNGNDX) ",A="
      D            RTNG-A (RTNGNDX) ",S="
      D            RTNG-S (RTNGNDX)
               MOVE "IN" TO WKFL-NEXTRULE
               PERFORM UNTIL WKFL-NEXTRULE = "A   " OR "R   "
      *            PERFORM VARYING WKFLNDX FROM 1 BY 1
      *                UNTIL WKFLNDX > WKFLMAX OR
      *                      WKFL-NEXTRULE =
      *                      FUNCTION UPPER-CASE (WKFL-NAME (WKFLNDX))
      *                SET WKFLSUB TO WKFLNDX
      *            END-PERFORM
                   SET WKFLNDX TO 1
                   SEARCH WORKFLOW-TABLE
                       AT END
                           DISPLAY "RULE '" WKFL-NEXTRULE
                               "', NOT FOUND, ABORTING"
                           CALL CEE3ABD USING ABEND-CODE, ABEND-FORMAT
                       WHEN WKFL-NEXTRULE =
                            FUNCTION UPPER-CASE (WKFL-NAME (WKFLNDX))
                           SET WKFLSUB TO WKFLNDX
                   END-SEARCH
      D            DISPLAY "RULE '" WKFL-NEXTRULE "' FOUND AT "
      D                WKFLSUB ",CRITCNT=" WKFL-COUNT (WKFLSUB)
                   SET WKFLNDX TO WKFLSUB
      *            WE FOUND THE WORKFLOW, NOW APPLY THE CRITERIA
                   MOVE SPACES TO WKFL-CRITRULE
                   PERFORM VARYING WKCRNDX FROM 1 BY 1
                       UNTIL WKFL-CRITRULE NOT = SPACES
                          OR WKCRNDX > WKFL-COUNT (WKFLNDX)
                       SET WKCRSUB TO WKCRNDX
      D                DISPLAY "MATCHING CRITERIA[" WKCRSUB "]='"
      D                    WKFL-CRIT (WKFLNDX WKCRNDX) "','"
      D                    WKFL-COND (WKFLNDX WKCRNDX) "',"
      D                    WKFL-VALUE (WKFLNDX WKCRNDX) ",'"
      D                    WKFL-RULE (WKFLNDX WKCRNDX) "'"
      *                BLANK WKFL-CRIT MEANS RULE NAME WITH NO CRITERIA
      *                NON-BLANK WKFL-CRIT MEANS APPLY THE CRITERIA
                       EVALUATE TRUE ALSO TRUE
                           WHEN WKFL-CRIT (WKFLNDX WKCRNDX) = SPACE
                           ALSO TRUE
                               MOVE FUNCTION UPPER-CASE (
                                    WKFL-RULE (WKFLNDX WKCRNDX) )
                                 TO WKFL-CRITRULE
                           WHEN FUNCTION UPPER-CASE (
                                WKFL-CRIT (WKFLNDX WKCRNDX) ) = "X"
                           ALSO WKFL-COND (WKFLNDX WKCRNDX)   = "<"
                             IF RTNG-X (RTNGNDX) <
                                WKFL-VALUE (WKFLNDX WKCRNDX)
                               MOVE FUNCTION UPPER-CASE (
                                    WKFL-RULE (WKFLNDX WKCRNDX) )
                                 TO WKFL-CRITRULE
                             END-IF
                           WHEN FUNCTION UPPER-CASE (
                                WKFL-CRIT (WKFLNDX WKCRNDX) ) = "X"
                           ALSO WKFL-COND (WKFLNDX WKCRNDX)   = ">"
                             IF RTNG-X (RTNGNDX) >
                                WKFL-VALUE (WKFLNDX WKCRNDX)
                               MOVE FUNCTION UPPER-CASE (
                                    WKFL-RULE (WKFLNDX WKCRNDX) )
                                 TO WKFL-CRITRULE
                             END-IF
                           WHEN FUNCTION UPPER-CASE (
                                WKFL-CRIT (WKFLNDX WKCRNDX) ) = "M"
                           ALSO WKFL-COND (WKFLNDX WKCRNDX)   = "<"
                             IF RTNG-M (RTNGNDX) <
                                WKFL-VALUE (WKFLNDX WKCRNDX)
                               MOVE FUNCTION UPPER-CASE (
                                    WKFL-RULE (WKFLNDX WKCRNDX) )
                                 TO WKFL-CRITRULE
                             END-IF
                           WHEN FUNCTION UPPER-CASE (
                                WKFL-CRIT (WKFLNDX WKCRNDX) ) = "M"
                           ALSO WKFL-COND (WKFLNDX WKCRNDX)   = ">"
                             IF RTNG-M (RTNGNDX) >
                                WKFL-VALUE (WKFLNDX WKCRNDX)
                               MOVE FUNCTION UPPER-CASE (
                                    WKFL-RULE (WKFLNDX WKCRNDX) )
                                 TO WKFL-CRITRULE
                             END-IF
                           WHEN FUNCTION UPPER-CASE (
                                WKFL-CRIT (WKFLNDX WKCRNDX) ) = "A"
                           ALSO WKFL-COND (WKFLNDX WKCRNDX)   = "<"
                             IF RTNG-A (RTNGNDX) <
                                WKFL-VALUE (WKFLNDX WKCRNDX)
                               MOVE FUNCTION UPPER-CASE (
                                    WKFL-RULE (WKFLNDX WKCRNDX) )
                                 TO WKFL-CRITRULE
                             END-IF
                           WHEN FUNCTION UPPER-CASE (
                                WKFL-CRIT (WKFLNDX WKCRNDX) ) = "A"
                           ALSO WKFL-COND (WKFLNDX WKCRNDX)   = ">"
                             IF RTNG-A (RTNGNDX) >
                                WKFL-VALUE (WKFLNDX WKCRNDX)
                               MOVE FUNCTION UPPER-CASE (
                                    WKFL-RULE (WKFLNDX WKCRNDX) )
                                 TO WKFL-CRITRULE
                             END-IF
                           WHEN FUNCTION UPPER-CASE (
                                WKFL-CRIT (WKFLNDX WKCRNDX) ) = "S"
                           ALSO WKFL-COND (WKFLNDX WKCRNDX)   = "<"
                             IF RTNG-S (RTNGNDX) <
                                WKFL-VALUE (WKFLNDX WKCRNDX)
                               MOVE FUNCTION UPPER-CASE (
                                    WKFL-RULE (WKFLNDX WKCRNDX) )
                                 TO WKFL-CRITRULE
                             END-IF
                           WHEN FUNCTION UPPER-CASE (
                                WKFL-CRIT (WKFLNDX WKCRNDX) ) = "S"
                           ALSO WKFL-COND (WKFLNDX WKCRNDX)   = ">"
                             IF RTNG-S (RTNGNDX) >
                                WKFL-VALUE (WKFLNDX WKCRNDX)
                               MOVE FUNCTION UPPER-CASE (
                                    WKFL-RULE (WKFLNDX WKCRNDX) )
                                 TO WKFL-CRITRULE
                             END-IF
                           WHEN OTHER
                               DISPLAY "RULE '" WKFL-NEXTRULE
                                   "', INVALID CRITERIA='"
                                   WKFL-CRIT (WKFLNDX WKCRNDX)
                                   "', ABORTING"
                               CALL CEE3ABD USING ABEND-CODE,
                                                  ABEND-FORMAT
                       END-EVALUATE
                   END-PERFORM
                   IF WKFL-CRITRULE > SPACES
      D                DISPLAY "RULE '" WKFL-NEXTRULE
      D                    "' CRIT " WKCRSUB " MATCHED, NEXT='"
      D                    WKFL-CRITRULE "'"
                       MOVE WKFL-CRITRULE TO WKFL-NEXTRULE
      D            ELSE
      D                DISPLAY "RULE '" WKFL-NEXTRULE
      D                    "', NO MATCHED CRITERIA, ABORTING"
      D                CALL CEE3ABD USING ABEND-CODE,
      D                                   ABEND-FORMAT
                   END-IF
               END-PERFORM
      D        DISPLAY "NEXTRULE='" WKFL-NEXTRULE "'"
      D            ",WKFLNAME[" WKFLSUB "]='" WKFL-NAME (WKFLSUB)
      D            ",FROM CRIT[" WKCRSUB "]"
               IF WKFL-NEXTRULE = "A   "
                   ADD RTNG-T (RTNGNDX) TO RTNG-TOTAL
               END-IF
           END-PERFORM
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
      D                WKFL-CRIT  (WKFLNDX WKCRNDX) ",COND="
      D                WKFL-COND  (WKFLNDX WKCRNDX) ",VAL="
      D                WKFL-VALUE (WKFLNDX WKCRNDX) ",RULE='"
      D                WKFL-RULE  (WKFLNDX WKCRNDX) "'"
      D        END-PERFORM
      D    END-PERFORM
      D    .

      D210-SHOW-RTNG.
      D    PERFORM VARYING RTNGNDX FROM 1 BY 1
      D        UNTIL RTNGNDX > RTNGMAX
      D        SET RTNGSUB TO RTNGNDX
      D        DISPLAY "RTNG[" RTNGSUB "],X="
      D            RTNG-X (RTNGNDX) ",M="
      D            RTNG-M (RTNGNDX) ",A="
      D            RTNG-A (RTNGNDX) ",S="
      D            RTNG-S (RTNGNDX) ",TOTAL="
      D            RTNG-T (RTNGNDX)
      D    END-PERFORM
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
           DISPLAY "PART 1=" RTNG-TOTAL
           CONTINUE.

       END PROGRAM DAY19PT1.
