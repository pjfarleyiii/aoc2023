       PROCESS NOSEQ,DS(S),AR(E),TEST(SO),CP(1047)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY17ALL
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

       01  PTYQ-PGMNAMES.
           05  PTYQPUSH          PIC  X(8) VALUE "PTYQPUSH".
           05  PTYQPOP           PIC  X(8) VALUE "PTYQPOP".

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

       01  WORK-FLAGS.
           05 CHANGED-FLAG               PIC X.
              88 HASCHANGED                    VALUE 'Y'.
              88 HASNOTCHANGED                 VALUE 'N'.
           05 FOUND-ENERGIZ-FLAG         PIC X.
              88 FOUND-ENERGIZ                 VALUE 'Y'.
              88 NOT-FOUND-NRGZ                VALUE 'N'.
           05 FOUND-NEW-BEAM-FLAG        PIC X.
              88 FOUND-NEW-BEAM                VALUE 'Y'.
              88 NOT-FOUND-NEWB                VALUE 'N'.
           05  SEEN-FOUND-SW             PIC  X.
               88  NOT-SEEN-FOUND              VALUE "N".
               88  SEEN-FOUND                  VALUE "Y".
           05  PART-NO                   PIC  9(1)  VALUE ZEROES.

       01  WORK-AREAS.
           05  ENERGIZED                 PIC S9(09) COMP-5 VALUE +0.
           05  MIN-STEP                  PIC S9(04) COMP-5 VALUE +0.
           05  MAX-STEP                  PIC S9(04) COMP-5 VALUE +0.
           05  ORIG-COST                 PIC S9(04) COMP-5 VALUE +0.
           05  MIN-COST                  PIC S9(04) COMP-5 VALUE +0.
           05  SRCH-DATA.
               10  SRCH-LOCN.
                   15  SRCH-ROW          PIC S9(04) COMP-5 VALUE 0.
                   15  SRCH-COL          PIC S9(04) COMP-5 VALUE 0.
               10  SRCH-DIR              PIC S9(04) COMP-5 VALUE 0.
           05  GOAL-LOCN.
               10  MAX-ROW               PIC S9(04) COMP-5 VALUE 0.
               10  MAX-COL               PIC S9(04) COMP-5 VALUE 0.
           05  NEW-LOCN.
               10  NEW-ROW               PIC S9(04) COMP-5 VALUE 0.
               10  NEW-COL               PIC S9(04) COMP-5 VALUE 0.

       01 HEATMAP-AREA.
          05  HMAPMAX                    PIC S9(04) COMP-5 VALUE +0.
          05  HMAPLEN                    PIC S9(04) COMP-5 VALUE +0.
          05  HEATMAP-DATA.
              10  FILLER                 OCCURS 256 TIMES
                                         INDEXED BY HMAPNDX.
                  15  HEATMAP            PIC  X(256) VALUE SPACES.

       01 NEW-MAP-AREA.
          05  NEWMMAX                    PIC S9(04) COMP-5 VALUE +0.
          05  NEWMLEN                    PIC S9(04) COMP-5 VALUE +0.
          05  NEW-MAP-DATA.
              10  FILLER                 OCCURS 256 TIMES
                                         INDEXED BY NEWMNDX.
                  15  NEW-MAP            PIC  X(256) VALUE SPACES.

       01  SEEN-MAPLOC-TABLE.
           05  SEENMAX               PIC S9(09) COMP-5 VALUE 0.
           05  SEEN-SIZE             PIC S9(09) COMP-5 VALUE 262144.
           05  SEEN-AREA.
               10  SEEN-DATA         OCCURS 262144 TIMES
                                     INDEXED BY SEENNDX.
                   15  SEEN-LOCN.
                       20  SEEN-ROW  PIC S9(04) COMP-5 VALUE 0.
                       20  SEEN-COL  PIC S9(04) COMP-5 VALUE 0.
                   15  SEEN-DIR      PIC S9(04) COMP-5 VALUE 0.

       01  CHECK-MAPLOC-TABLE.
           05  CHECKMAX              PIC S9(09) COMP-5 VALUE 0.
           05  CHECK-SIZE            PIC S9(09) COMP-5 VALUE 262144.
           05  CHECK-AREA.
               10  CHECK-DATA        OCCURS 262144 TIMES.
                   15  CHECK-LOCN.
                       20  CHECK-ROW PIC S9(04) COMP-5 VALUE 0.
                       20  CHECK-COL PIC S9(04) COMP-5 VALUE 0.
                   15  CHECK-DIR     PIC S9(04) COMP-5 VALUE 0.

       01  TASK-PTR              POINTER.

       01  TOP-PTR               POINTER.

       01  LINK-KEY              PIC S9(8) COMP-5.

       01  STEP-KEY              PIC S9(8) COMP-5.

       01  STEP-PTR              POINTER VALUE NULL.

       01  STPN-PTR              POINTER VALUE NULL.

       01  NODE-SUB              PIC S9(8) COMP-5.

       01  HEAP-PTR              POINTER VALUE NULL.

       01  PUSHD-PTR             POINTER VALUE NULL.

       01  POPPD-PTR             POINTER VALUE NULL.

       01  TASK-TABLE.
           05  TASK              OCCURS 10 TIMES.
               10  TASK-KEY      PIC S9(8) COMP-5.
               10  TASK-NAME     PIC  X(40).

       LINKAGE SECTION.
       01  PARM-AREA.
           05  PARM-LEN          PIC S9(4) COMP-5.
           05  PARM-MINLEN       PIC  9(2).
           05  FILLER            PIC  X.
           05  PARM-MAXLEN       PIC  9(2).

       01  STEP.
           05  STEP-DATA.
               10  STEP-COST     PIC S9(4) COMP-5.
               10  STEP-LOCN.
                   15  STEP-ROW  PIC S9(4) COMP-5.
                   15  STEP-COL  PIC S9(4) COMP-5.
               10  STEP-DIR      PIC S9(4) COMP-5.

       01  STPN.
           05  STPN-DATA.
               10  STPN-COST     PIC S9(4) COMP-5.
               10  STPN-LOCN.
                   15  STPN-ROW  PIC S9(4) COMP-5.
                   15  STPN-COL  PIC S9(4) COMP-5.
               10  STPN-DIR      PIC S9(4) COMP-5.

       01  SHOW.
           05  SHOW-DATA.
               10  SHOW-COST     PIC S9(4) COMP-5.
               10  SHOW-LOCN.
                   15  SHOW-ROW  PIC S9(4) COMP-5.
                   15  SHOW-COL  PIC S9(4) COMP-5.
               10  SHOW-DIR      PIC S9(4) COMP-5.

       01  HEAP-T.
           05  HEAP-EYECATCH     PIC  X(8).
               88 VALID-HEAP               VALUE "BINHEAPT".
           05  NODES-PTR         POINTER.
           05  HEAP-LEN          PIC S9(8) COMP-5.
           05  HEAP-SIZE         PIC S9(8) COMP-5.

       01  NODE-TABLE.
           05  NODE-EYECATCH     PIC  X(8).
               88 VALID-NODE               VALUE "BINNODET".
           05  NODE-T            OCCURS 0 TO 524288
                                 DEPENDING ON HEAP-SIZE
                                 INDEXED BY NODENDX.
               10  NODE-KEY      PIC S9(8) COMP-5.
               10  NODE-DATA     POINTER.

       01  POPPD-TASK.
           05  POPPD-KEY         PIC S9(8) COMP-5.
           05  POPPD-NAME        PIC  X(40).

       PROCEDURE DIVISION USING PARM-AREA.
      D    PERFORM 999-PTYQ-IVP
           PERFORM 000-HOUSEKEEPING
           PERFORM 100-PROCESS-INPUT-DATA
           PERFORM 900-WRAP-UP
           GOBACK.

       000-HOUSEKEEPING.
           IF PARM-LEN >= +5 AND
              PARM-MINLEN NUMERIC AND PARM-MAXLEN NUMERIC
               MOVE PARM-MINLEN TO MIN-STEP
               MOVE PARM-MAXLEN TO MAX-STEP
           ELSE
               MOVE +1          TO MIN-STEP
               MOVE +3          TO MAX-STEP
           END-IF
      D    DISPLAY "MINSTEP=" MIN-STEP ",MAXSTEP=" MAX-STEP
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
           MOVE LINELEN TO HMAPLEN
           PERFORM UNTIL END-OF-FILE
               ADD +1 TO HMAPMAX
               SET HMAPNDX TO HMAPMAX
               MOVE LINEINPUT (1 : LINELEN) TO HEATMAP (HMAPNDX)
               IF NOT END-OF-FILE
                   PERFORM 050-READ-INPUT-DATA
               END-IF
           END-PERFORM
      D    PERFORM 200-SHOW-MAP
           MOVE +0 TO CHECKMAX SEENMAX
           MOVE HEATMAP-AREA TO NEW-MAP-AREA
           COMPUTE MAX-ROW = HMAPMAX - 1
           COMPUTE MAX-COL = HMAPLEN - 1

      *    HEAPPUSH(Q,(0, (0, 0, 0)))
           ALLOCATE STEP RETURNING STEP-PTR
           MOVE LOW-VALUES TO STEP
           CALL PTYQPUSH USING HEAP-PTR, STEP-KEY, STEP-PTR
      *    HEAPPUSH(Q,(0, (0, 0, 1)))
           ALLOCATE STEP RETURNING STEP-PTR
           MOVE LOW-VALUES TO STEP
           MOVE +1 TO STEP-DIR
           CALL PTYQPUSH USING HEAP-PTR, STEP-KEY, STEP-PTR
      D    PERFORM 999-PTYQ-SHOW-STEP
           SET ADDRESS OF HEAP-T TO HEAP-PTR

           PERFORM WITH TEST BEFORE UNTIL HEAP-LEN = +0
      *        COST, (Y, X, DIRECTION) = HEAPPOP(Q) #Q.GET()
               CALL PTYQPOP USING HEAP-PTR, STEP-KEY, STEP-PTR
               SET ADDRESS OF HEAP-T TO HEAP-PTR
               SET ADDRESS OF STEP   TO STEP-PTR
      D        DISPLAY "POPPED KEY=" STEP-KEY
      D            ",COST=" STEP-COST
      D            ",LOCN=(" STEP-ROW "," STEP-COL ")"
      D            ",DIR=" STEP-DIR
      D            "                    "
      D            ",HEAP AT 0X" FUNCTION HEX-OF (HEAP-PTR)
      D            ",NODES AT 0X"   FUNCTION HEX-OF (NODES-PTR)
      D            ",HEAPLEN=" HEAP-LEN ",HEAPSIZE=" HEAP-SIZE
      D            ",HEAPEYE=" HEAP-EYECATCH
               MOVE STEP-COST TO MIN-COST
      *        IF (Y, X) == GOAL:
      *            BREAK
               IF STEP-LOCN = GOAL-LOCN
                   DISPLAY "GOAL REACHED"
                   EXIT PERFORM
               END-IF
      *        IF (Y, X, DIRECTION) IN SEEN:
      *            CONTINUE
               MOVE STEP-LOCN TO SRCH-LOCN
               MOVE STEP-DIR  TO SRCH-DIR
               PERFORM SRCH-SEEN-MAPLOC
               IF SEEN-FOUND
      D            DISPLAY "SEEN-FOUND #1, EXITING HEAP LOOP"
                   EXIT PERFORM CYCLE
               END-IF
      *        SEEN.ADD((Y, X, DIRECTION))
      *        ORIGINAL_COST = COST
               PERFORM ADD-SEEN-NEXT
               MOVE STEP-COST TO ORIG-COST
      *        FOR S IN [-1, 1]:
               MOVE -1 TO S41
               PERFORM 2 TIMES
      *            COST = ORIGINAL_COST
      *            NEW_Y, NEW_X = Y, X
      *            MOVE ORIG-COST TO STEP-COST, MIN-COST
                   MOVE ORIG-COST TO STEP-COST
                   MOVE STEP-LOCN TO NEW-LOCN
      D            DISPLAY "S=" S41 ",MAXSTEP=" MAX-STEP
      D                ",COST=" STEP-COST
      D                ",(NEWY,NEWX)=(" STEP-ROW "," STEP-COL ")"
      *            FOR I IN RANGE(1, MAXVAL + 1):
                   PERFORM VARYING S42 FROM +1 BY +1
                       UNTIL S42 > MAX-STEP
      D                COMPUTE S43 = S42 * S41
      D                DISPLAY "I=" S42 ",(I * S)=" S43
      *                IF DIRECTION == 1:
      *                    NEW_X = X + I * S
      *                ELSE:
      *                    NEW_Y = Y + I * S
                       IF STEP-DIR = 1
                           COMPUTE NEW-COL = STEP-COL + (S42 * S41)
                       ELSE
                           COMPUTE NEW-ROW = STEP-ROW + (S42 * S41)
                       END-IF
      D                DISPLAY "NEWROW=" NEW-ROW ",NEWCOL=" NEW-COL
      D                    ",MAXROW=" MAX-ROW ",MAXCOL=" MAX-COL
      *                IF NEW_X < 0 OR NEW_Y < 0 OR
      *                   NEW_X > MAX_X OR NEW_Y > MAX_Y:
      *                     BREAK
                       IF NEW-COL < 0       OR NEW-ROW < 0 OR
                          NEW-COL > MAX-COL OR NEW-ROW > MAX-ROW
      D                    DISPLAY "NEWROW/COL INVALID, EXITING S42"
                           EXIT PERFORM
                       END-IF
      *                COST += GRID[NEW_Y, NEW_X]
                       COMPUTE STEP-COST = STEP-COST +
                           FUNCTION NUMVAL (
                               HEATMAP (NEW-ROW + 1) (NEW-COL + 1 : 1) )
      D                DISPLAY "NEWCOST=" STEP-COST ",GRID[Y,X]='"
      D                    HEATMAP (NEW-ROW + 1) (NEW-COL + 1 : 1) "'"
      *                MOVE STEP-COST TO MIN-COST
      *                IF ((NEW_Y, NEW_X, 1 - DIRECTION)) IN SEEN:
      *                    CONTINUE
                       MOVE NEW-LOCN TO SRCH-LOCN
                       COMPUTE SRCH-DIR = 1 - STEP-DIR
                       PERFORM SRCH-SEEN-MAPLOC
                       IF SEEN-FOUND
      D                    DISPLAY "SEEN-FOUND #2, CONTINUING S42"
                           EXIT PERFORM CYCLE
                       END-IF
      *                IF I >= MINVAL:
      *                    HEAPPUSH(Q,(COST, (NEW_Y, NEW_X, 1 - DIRECTION)))
                       IF S42 >= MIN-STEP
                           ALLOCATE STPN RETURNING STPN-PTR
                           MOVE STEP-COST TO STPN-COST STEP-KEY
                           MOVE SRCH-DIR  TO STPN-DIR
                           MOVE NEW-ROW   TO STPN-ROW
                           MOVE NEW-COL   TO STPN-COL
      D                    DISPLAY "PUSHING KEY=" STEP-KEY
      D                        ",DATA AT 0X" FUNCTION HEX-OF (STPN-PTR)
      D                        ",COST=" STPN-COST
      D                        ",LOCN=(" STPN-ROW "," STPN-COL ")"
      D                        ",DIR=" STPN-DIR
      D                    ",HEAP AT 0X" FUNCTION HEX-OF (HEAP-PTR)
      D                    ",NODES AT 0X"   FUNCTION HEX-OF (NODES-PTR)
      D                    ",HEAPLEN=" HEAP-LEN ",HEAPSIZE=" HEAP-SIZE
      D                    ",HEAPEYE=" HEAP-EYECATCH
                           CALL PTYQPUSH USING HEAP-PTR, STEP-KEY,
                                               STPN-PTR
      D                    PERFORM 999-PTYQ-SHOW-STEP
                       END-IF
                   END-PERFORM
                   MOVE +1 TO S41
               END-PERFORM
      D        PERFORM 999-PTYQ-SHOW-STEP
               FREE STEP-PTR
           END-PERFORM
           .

     D 200-SHOW-MAP.
     D     PERFORM VARYING HMAPNDX FROM 1 BY 1
     D         UNTIL HMAPNDX > HMAPMAX
     D         SET S41 TO HMAPNDX
     D         DISPLAY "MAP[" S41 "]='"
     D             HEATMAP (HMAPNDX) (1 : HMAPLEN) "'"
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

       SRCH-SEEN-MAPLOC.
           SET NOT-SEEN-FOUND TO TRUE
           IF SEENMAX > +0
               SET SEENNDX TO 1
               SEARCH SEEN-DATA VARYING SEENNDX
                   WHEN SEENNDX > SEENMAX
      D                DISPLAY "NOT FOUND LOCN=(" SRCH-ROW
      D                    "," SRCH-COL "),DIR=" SRCH-DIR
                       SET SEENNDX TO SEEN-SIZE
      *            WHEN SEEN-LOCN (SEENNDX) = SRCH-LOCN AND
      *                 SEEN-DIR  (SEENNDX) = SRCH-DIR
                   WHEN SEEN-DATA (SEENNDX) = SRCH-DATA
                       SET SEEN-FOUND TO TRUE
      D                DISPLAY "FOUND SEEN[" SEENMAX
      D                    "]=(" SEEN-ROW (SEENMAX)
      D                    "," SEEN-COL  (SEENMAX)
      D                    "),DIR=" SEEN-DIR  (SEENMAX)
                       SET SEENNDX TO SEEN-SIZE
               END-SEARCH
           END-IF
           .

       ADD-SEEN-NEXT.
           ADD 1 TO SEENMAX
      *    MOVE SRCH-LOCN TO SEEN-LOCN (SEENMAX)
      *    MOVE SRCH-DIR  TO SEEN-DIR  (SEENMAX)
           MOVE SRCH-DATA TO SEEN-DATA (SEENMAX)
      D    DISPLAY "ADDED SEEN[" SEENMAX "]=(" SEEN-ROW (SEENMAX)
      D        "," SEEN-COL  (SEENMAX) "),DIR=" SEEN-DIR  (SEENMAX)
           .

       900-WRAP-UP.
           CLOSE INPUT-FILE.
           DISPLAY "COST=" MIN-COST
           CONTINUE.

       999-PTYQ-IVP.
           MOVE +6 TO LINK-KEY        TASK-KEY  (LINK-KEY)
           MOVE "EAT SCONES."      TO TASK-NAME (LINK-KEY)
           SET TASK-PTR TO ADDRESS OF TASK      (LINK-KEY)
           CALL PTYQPUSH USING HEAP-PTR, LINK-KEY, TASK-PTR

           MOVE +3 TO LINK-KEY        TASK-KEY  (LINK-KEY)
           MOVE "CLEAR DRAINS."    TO TASK-NAME (LINK-KEY)
           SET TASK-PTR TO ADDRESS OF TASK      (LINK-KEY)
           CALL PTYQPUSH USING HEAP-PTR, LINK-KEY, TASK-PTR

           MOVE +4 TO LINK-KEY        TASK-KEY  (LINK-KEY)
           MOVE "FEED CAT."        TO TASK-NAME (LINK-KEY)
           SET TASK-PTR TO ADDRESS OF TASK      (LINK-KEY)
           CALL PTYQPUSH USING HEAP-PTR, LINK-KEY, TASK-PTR

           MOVE +5 TO LINK-KEY        TASK-KEY  (LINK-KEY)
           MOVE "MAKE TEA."        TO TASK-NAME (LINK-KEY)
           SET TASK-PTR TO ADDRESS OF TASK      (LINK-KEY)
           CALL PTYQPUSH USING HEAP-PTR, LINK-KEY, TASK-PTR

           MOVE +1 TO LINK-KEY        TASK-KEY  (LINK-KEY)
           MOVE "SOLVE RC TASKS."  TO TASK-NAME (LINK-KEY)
           SET TASK-PTR TO ADDRESS OF TASK      (LINK-KEY)
           CALL PTYQPUSH USING HEAP-PTR, LINK-KEY, TASK-PTR

           MOVE +2 TO LINK-KEY        TASK-KEY  (LINK-KEY)
           MOVE "TAX RETURN."      TO TASK-NAME (LINK-KEY)
           SET TASK-PTR TO ADDRESS OF TASK      (LINK-KEY)
           CALL PTYQPUSH USING HEAP-PTR, LINK-KEY, TASK-PTR

           SET ADDRESS OF HEAP-T TO HEAP-PTR
           DISPLAY "HEAP AT 0X" FUNCTION HEX-OF (HEAP-PTR)
               ",NODES AT 0X"   FUNCTION HEX-OF (NODES-PTR)
               ",HEAPLEN=" HEAP-LEN ",HEAPSIZE=" HEAP-SIZE
               ",HEAPEYE=" HEAP-EYECATCH
           SET ADDRESS OF NODE-TABLE TO NODES-PTR

           PERFORM 999-PTYQ-SHOW-NAME

           PERFORM WITH TEST BEFORE UNTIL HEAP-LEN = +0
               SET TOP-PTR TO HEAP-PTR
               SET ADDRESS OF HEAP-T TO HEAP-PTR
               SET ADDRESS OF NODE-TABLE TO NODES-PTR
               DISPLAY "NODE(1)=" NODE-KEY (2) ", 0X"
                   FUNCTION HEX-OF (NODE-DATA (2))
               CALL PTYQPOP USING HEAP-PTR, LINK-KEY, POPPD-PTR
               IF LINK-KEY > +0
                   SET ADDRESS OF POPPD-TASK TO POPPD-PTR
                   DISPLAY "POPPED VALUES: "
                       LINK-KEY "," POPPD-KEY ", '" POPPD-NAME "'"
               END-IF
               DISPLAY
                   "HEAPLEN=" HEAP-LEN ",HEAPSIZE=" HEAP-SIZE
           END-PERFORM
           CONTINUE.

       999-PTYQ-SHOW-NAME.
           PERFORM VARYING NODENDX FROM 1 BY 1 UNTIL NODENDX > HEAP-SIZE
               SET NODE-SUB TO NODENDX
               DISPLAY "NODE[" NODE-SUB "]=(" NODE-KEY (NODENDX)
                   ", 0X" FUNCTION HEX-OF (NODE-DATA (NODENDX)) ")"
               IF NODE-DATA (NODENDX) NOT = NULL
                   SET ADDRESS OF POPPD-TASK TO NODE-DATA (NODENDX)
                   DISPLAY "    NODE-DATA=(" POPPD-KEY ",'"
                       POPPD-NAME "')"
               END-IF
           END-PERFORM
           CONTINUE.

       999-PTYQ-SHOW-STEP.
           DISPLAY "HEAP AT 0X" FUNCTION HEX-OF (HEAP-PTR)
               ",NODES AT 0X"   FUNCTION HEX-OF (NODES-PTR)
               ",HEAPLEN=" HEAP-LEN ",HEAPSIZE=" HEAP-SIZE
               ",HEAPEYE=" HEAP-EYECATCH
           SET ADDRESS OF NODE-TABLE TO NODES-PTR
           DISPLAY "NODEEYE=" NODE-EYECATCH
           PERFORM VARYING NODENDX FROM 1 BY 1
               UNTIL NODENDX > HEAP-LEN + 1
               SET NODE-SUB TO NODENDX
               DISPLAY "NODE[" NODE-SUB "]=(" NODE-KEY (NODENDX)
                   ", 0X" FUNCTION HEX-OF (NODE-DATA (NODENDX)) ")"
               IF NODE-DATA (NODENDX) NOT = NULL
                   SET ADDRESS OF SHOW TO NODE-DATA (NODENDX)
                   DISPLAY "    STEP-DATA=(" SHOW-COST ","
                       SHOW-ROW "," SHOW-COL "," SHOW-DIR ")"
               END-IF
           END-PERFORM
           CONTINUE.
       END PROGRAM DAY17ALL.
