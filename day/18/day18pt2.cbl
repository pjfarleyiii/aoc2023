       PROCESS NOSEQ,DS(S),AR(E),TEST(SO),CP(1047)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY18PT2.

      * AREA OF AN ODDLY SHAPED POLYGON FOUND HERE:
      *
      * https://arachnoid.com/area_irregular_polygon/index.html

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

       01 CALLED-PROGRAMS.
           05  CEE3ABD                   PIC  X(08) VALUE "CEE3ABD".

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
          05  ABEND-CODE                 PIC S9(09) COMP-5 VALUE +1040.
          05  ABEND-FORMAT               PIC S9(09) COMP-5 VALUE +1.
          05  AREA-TOTAL                 PIC S9(18) COMP-5 VALUE +0.
          05  PERIMETER                  PIC S9(18) COMP-5 VALUE +0.
          05  START-DATA.
              10  START-X                PIC S9(04) COMP-5 VALUE +0.
              10  START-Y                PIC S9(04) COMP-5 VALUE +0.
              10  START-DX               PIC S9(04) COMP-5 VALUE +0.
              10  START-DY               PIC S9(04) COMP-5 VALUE +0.
          05  MOVE-DATA.
              10  MOVE-X                 PIC S9(18) COMP-5 VALUE +0.
              10  MOVE-Y                 PIC S9(18) COMP-5 VALUE +0.
              10  MOVE-DX                PIC S9(09) COMP-5 VALUE +0.
              10  MOVE-DY                PIC S9(09) COMP-5 VALUE +0.
          05  MOVED-AREA.
              10  MOVDMAX                PIC S9(04) COMP-5 VALUE +0.
              10  MOVED-DATA             OCCURS 2 TIMES.
                  15  MOVED-X            PIC S9(04) COMP-5 VALUE +0.
                  15  MOVED-Y            PIC S9(04) COMP-5 VALUE +0.
                  15  MOVED-DX           PIC S9(04) COMP-5 VALUE +0.
                  15  MOVED-DY           PIC S9(04) COMP-5 VALUE +0.
          05  MOVE-CNT-NUM               PIC S9(09) COMP-5 VALUE +0.
          05  MOVE-CLR-LAST              PIC S9(04) COMP-5 VALUE +0.
          05  MOVE-CLR-NUM               PIC S9(09) COMP-5 VALUE +0.
          05  FILLER                     REDEFINES MOVE-CLR-NUM.
              10  FILLER                 PIC  X.
              10  MOVE-CLR-BYTES         PIC  X(03).
          05  MOVE-CHAR                  PIC  X.
          05  MOVE-COUNT                 PIC  X(02).
          05  MOVE-DELIM                 PIC  X.
          05  MOVE-COLOR.
              10  MOVE-COLOR5            PIC  X(06).
              10  MOVE-COLOR-LAST        PIC  X.

       01 VERTICES-AREA.
          05  VERTMAX                    PIC S9(04) COMP-5 VALUE +0.
          05  VERTSUB                    PIC S9(04) COMP-5 VALUE +0.
          05  VERTICES-DATA.
              10  FILLER                 OCCURS 1024 TIMES
                                         INDEXED BY VERTNDX.
                  15  VERT-X             PIC S9(09) COMP-5 VALUE +0.
                  15  VERT-Y             PIC S9(09) COMP-5 VALUE +0.

       01 DIRECTN-AREA.
          05  DIRCMAX                    PIC S9(04) COMP-5 VALUE +4.
          05  DIRCSUB                    PIC S9(04) COMP-5 VALUE +0.
          05  DIRECTN-DATA.
              10  DIRECTN-TABLE          OCCURS 4 TIMES
                                         INDEXED BY DIRCNDX.
                  15  DIRC-X             PIC S9(04) COMP-5 VALUE +0.
                  15  DIRC-Y             PIC S9(04) COMP-5 VALUE +0.
                  15  DIRC-CHAR          PIC  X.
                  15  DIRC-NUM           PIC  X.

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
           MOVE +0 TO DIRC-X (1) DIRC-X (2) DIRC-Y (3) DIRC-Y (4)
           MOVE +1 TO            DIRC-Y (2)            DIRC-X (4)
           MOVE -1 TO DIRC-Y (1)            DIRC-X (3)
           MOVE "U" TO DIRC-CHAR (1)
           MOVE "D" TO DIRC-CHAR (2)
           MOVE "L" TO DIRC-CHAR (3)
           MOVE "R" TO DIRC-CHAR (4)
           MOVE "3" TO DIRC-NUM  (1)
           MOVE "1" TO DIRC-NUM  (2)
           MOVE "2" TO DIRC-NUM  (3)
           MOVE "0" TO DIRC-NUM  (4)
      D    PERFORM VARYING DIRCNDX FROM 1 BY 1
      D        UNTIL DIRCNDX > DIRCMAX
      D        SET S41 TO DIRCNDX
      D        DISPLAY "DIR[" S41 "]=("
      D            DIRC-X (DIRCNDX) ","
      D            DIRC-Y (DIRCNDX) ")"
      D    END-PERFORM
           ADD +1 TO VERTMAX
           SET VERTNDX TO 1
           MOVE +0 TO VERT-X (VERTNDX) VERT-Y (VERTNDX)
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
           MOVE +0 TO MOVE-X MOVE-Y
           PERFORM UNTIL END-OF-FILE
      *        SPLIT APART INPUT PARAMETERS
               UNSTRING LINEINPUT (1 : LINELEN)
                   DELIMITED BY SPACE OR "(" OR ")" INTO
                   MOVE-CHAR
                   MOVE-COUNT
                   MOVE-DELIM
                   MOVE-COLOR
               END-UNSTRING
      D        DISPLAY "DIRECTN=" MOVE-CHAR
      D            ",COUNT=" MOVE-COUNT
      D            ",COLOR='" MOVE-COLOR "'"
               MOVE ZERO TO MOVE-COLOR5 (1 : 1)
               MOVE FUNCTION HEX-TO-CHAR (MOVE-COLOR5) TO MOVE-CLR-BYTES
               COMPUTE MOVE-CNT-NUM = MOVE-CLR-NUM
               SET DIRCNDX TO 1
               SEARCH DIRECTN-TABLE
                   AT END
                       DISPLAY "INVALID DIRECTION '" MOVE-CHAR
                           "' AT LINE " LINECTR
                           ",ABORTING WITH CODE " ABEND-CODE
                       CALL CEE3ABD USING ABEND-CODE, ABEND-FORMAT
                   WHEN DIRC-NUM  (DIRCNDX) = MOVE-COLOR-LAST
                       MOVE DIRC-X (DIRCNDX) TO MOVE-DX
                       MOVE DIRC-Y (DIRCNDX) TO MOVE-DY
                       MOVE DIRC-CHAR (DIRCNDX) TO MOVE-CHAR
                       SET DIRCSUB TO DIRCNDX
               END-SEARCH
      D        DISPLAY "DIRECTN=" MOVE-CHAR
      D            ",DIREC[" DIRCSUB "]=(" MOVE-DX "," MOVE-DY ")"
      D            ",COUNT=" MOVE-COUNT ",CNTNUM=" MOVE-CNT-NUM
      D            ",COLOR='" MOVE-COLOR "'"
      D            ",CLRNUM=" MOVE-CLR-NUM
      D            "=X'" FUNCTION HEX-OF (MOVE-CLR-NUM) "'"
               ADD MOVE-CNT-NUM TO PERIMETER
               ADD +1 TO VERTMAX
               SET VERTNDX UP BY 1
               COMPUTE MOVE-X = MOVE-X + (MOVE-DX * MOVE-CNT-NUM)
               COMPUTE MOVE-Y = MOVE-Y + (MOVE-DY * MOVE-CNT-NUM)
               MOVE MOVE-X TO VERT-X (VERTNDX)
               MOVE MOVE-Y TO VERT-Y (VERTNDX)
               IF NOT END-OF-FILE
                   PERFORM 050-READ-INPUT-DATA
               END-IF
           END-PERFORM
      D    PERFORM 200-SHOW-MAP

      *    NOW COMPUTE AREA FROM VERTICES
           COMPUTE S41 = VERTMAX - 1
           PERFORM VARYING VERTNDX FROM 2 BY 1
               UNTIL VERTNDX > VERTMAX
      D        SET S41 TO VERTNDX
      D        SUBTRACT +1 FROM S41 GIVING S42
      D        COMPUTE S43 = VERT-X (VERTNDX) * VERT-Y (VERTNDX - 1)
      D        COMPUTE S44 = VERT-Y (VERTNDX) * VERT-X (VERTNDX - 1)
      D        DISPLAY "VTX[" S42 "]=("
      D            VERT-X (VERTNDX) ","
      D            VERT-Y (VERTNDX) ")"
      D            ",AREA=" AREA-TOTAL " + (" S43 " - " S44 ")"
               COMPUTE AREA-TOTAL = AREA-TOTAL +
                   ( VERT-X (VERTNDX) * VERT-Y (VERTNDX - 1) ) -
                   ( VERT-Y (VERTNDX) * VERT-X (VERTNDX - 1) )
           END-PERFORM
           COMPUTE AREA-TOTAL = FUNCTION ABS (
               FUNCTION INTEGER(AREA-TOTAL / 2) ) +
               FUNCTION INTEGER(PERIMETER / 2) + 1
           .

      D200-SHOW-MAP.
      D    PERFORM VARYING VERTNDX FROM 1 BY 1
      D        UNTIL VERTNDX > VERTMAX
      D        SET S41 TO VERTNDX
      D        DISPLAY "VTX[" S41 "]=("
      D            VERT-X (VERTNDX) ","
      D            VERT-Y (VERTNDX) ")"
      D    END-PERFORM
      D    .

       900-WRAP-UP.
           CLOSE INPUT-FILE.
      D    DISPLAY "PERIMETER=" PERIMETER
           DISPLAY "PART 2=" AREA-TOTAL
           CONTINUE.

       END PROGRAM DAY18PT2.
