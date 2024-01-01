       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY1PT2.
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
      *-------------
       FILE SECTION.

       FD  INPUT-FILE
           RECORD VARYING 1 TO 32756 DEPENDING ON LINELEN
           RECORDING MODE V.
       01  INPUT-FIELDS.
           05  LINEINPUT  PIC X(32756).

       WORKING-STORAGE SECTION.
       01  INPUT-FILE-VARIABLES.
           05  LINELEN           PIC  9(8) BINARY.
           05  LINECTR           PIC S9(8) BINARY VALUE +0.
           05  SW-END-OF-FILE    PIC  X    VALUE SPACE.
               88 END-OF-FILE              VALUE "Y".

       01  LINE-VALUES.
           05  DIGITPTR          PIC  9(8) BINARY.
           05  CHARPTR           PIC  9(8) BINARY.
           05  PART1-VALUE       PIC  9(8) VALUE ZEROES.
           05  PART2-VALUE       PIC  9(8) VALUE ZEROES.
           05  LINE-VALUE        PIC  99.
           05  FILLER            REDEFINES LINE-VALUE.
               10  LINE-D1       PIC  9.
               10  LINE-D2       PIC  9.
           05  NAME-VALUE        PIC  99.
           05  FILLER            REDEFINES NAME-VALUE.
               10  NAME-D1       PIC  9.
               10  NAME-D2       PIC  9.

       01  PART2-TABLE.
           05  CHARSUB           PIC  9(8) BINARY.
           05  CHARLEN           PIC  9(8) BINARY.
           05  DGT-TABLE.
               10  FILLER        PIC  X(6) VALUE '3ONE  '.
               10  FILLER        PIC  X(6) VALUE '3TWO  '.
               10  FILLER        PIC  X(6) VALUE '5THREE'.
               10  FILLER        PIC  X(6) VALUE '4FOUR '.
               10  FILLER        PIC  X(6) VALUE '4FIVE '.
               10  FILLER        PIC  X(6) VALUE '3SIX  '.
               10  FILLER        PIC  X(6) VALUE '5SEVEN'.
               10  FILLER        PIC  X(6) VALUE '5EIGHT'.
               10  FILLER        PIC  X(6) VALUE '4NINE '.
           05  DGT-NAMES         REDEFINES
               DGT-TABLE         OCCURS 9 TIMES.
               10  NAME-LEN      PIC  9.
               10  DGT-NAME      PIC  X(5).



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
           END-READ.

       100-PROCESS-INPUT-DATA.
           PERFORM UNTIL END-OF-FILE
DEBUG>D        DISPLAY LINEINPUT (1 : LINELEN)
               MOVE ZEROES TO LINE-VALUE, NAME-VALUE
               PERFORM VARYING DIGITPTR FROM +1 BY +1
                   UNTIL DIGITPTR > LINELEN
                   IF LINEINPUT (DIGITPTR : 1) NUMERIC
                       MOVE LINEINPUT (DIGITPTR : 1)
                         TO LINE-D1
                       EXIT PERFORM
                   END-IF
               END-PERFORM
DEBUG>D        DISPLAY "AT " LINECTR " 1ST=" LINE-D1
DEBUG>D            "             "     " CPTR=" DIGITPTR
               PERFORM VARYING CHARPTR FROM +1 BY +1
                   UNTIL CHARPTR > LINELEN
                   PERFORM VARYING CHARSUB FROM +1 BY +1
                       UNTIL CHARSUB > +9
                       MOVE NAME-LEN (CHARSUB) TO CHARLEN
                       IF (CHARPTR + CHARLEN - 1) <= LINELEN
DEBUG>D                    DISPLAY "CP=" CHARPTR " SUB=" CHARSUB
DEBUG>D                        " LEN=" CHARLEN
DEBUG>D                        " LSUB=" LINEINPUT (CHARPTR : CHARLEN)
DEBUG>D                        ",DNAM=" DGT-NAME (CHARSUB) (1 : CHARLEN)
                           IF FUNCTION UPPER-CASE (
                                  LINEINPUT (CHARPTR : CHARLEN) ) =
                              DGT-NAME (CHARSUB) (1 : CHARLEN)
                               MOVE CHARSUB TO NAME-D1
                               EXIT PERFORM
                           END-IF
                       END-IF
                   END-PERFORM
DEBUG>D            DISPLAY "AT " LINECTR " 1ST=" NAME-D1
DEBUG>D                " CSUB=" CHARSUB    " CPTR=" CHARPTR
                   IF NAME-D1 NOT = ZERO
                       EXIT PERFORM
                   END-IF
               END-PERFORM
DEBUG>D        DISPLAY "AT " LINECTR
DEBUG>D            " DPTR<CPTR=" DIGITPTR " < " CHARPTR
DEBUG>D            " DIG=" LINE-D1 " CHR=" NAME-D1
               IF DIGITPTR < CHARPTR
                   MOVE LINE-D1 TO NAME-D1
               END-IF
               PERFORM VARYING DIGITPTR FROM LINELEN BY -1
                   UNTIL DIGITPTR < +1
                   IF LINEINPUT (DIGITPTR : 1) NUMERIC
                       MOVE LINEINPUT (DIGITPTR : 1)
                         TO LINE-D2
                       ADD LINE-VALUE TO PART1-VALUE
                       EXIT PERFORM
                   END-IF
               END-PERFORM
DEBUG>D        DISPLAY "AT " LINECTR " CAL=" LINE-VALUE
DEBUG>D            " PT1=" PART1-VALUE
DEBUG>D        DISPLAY "AT " LINECTR " 2ND=" LINE-D2
DEBUG>D            " PT1=" PART1-VALUE " CPTR=" DIGITPTR
               PERFORM VARYING CHARPTR FROM LINELEN BY -1
                   UNTIL CHARPTR < +1
                   PERFORM VARYING CHARSUB FROM +1 BY +1
                       UNTIL CHARSUB > +9
                       MOVE NAME-LEN (CHARSUB) TO CHARLEN
                       IF (CHARPTR + CHARLEN - 1) <= LINELEN
DEBUG>D                    DISPLAY "CP=" CHARPTR " SUB=" CHARSUB
DEBUG>D                        " LEN=" CHARLEN
DEBUG>D                        " LSUB=" LINEINPUT (CHARPTR : CHARLEN)
DEBUG>D                        ",DNAM=" DGT-NAME (CHARSUB) (1 : CHARLEN)
                           IF FUNCTION UPPER-CASE (
                                  LINEINPUT (CHARPTR : CHARLEN) ) =
                              DGT-NAME (CHARSUB) (1 : CHARLEN)
                               MOVE CHARSUB TO NAME-D2
                               EXIT PERFORM
                           END-IF
                       END-IF
                   END-PERFORM
DEBUG>D            DISPLAY "AT " LINECTR " 2ND=" NAME-D2
DEBUG>D                " CSUB=" CHARSUB    " CPTR=" CHARPTR
                   IF NAME-D2 NOT = ZERO
                       EXIT PERFORM
                   END-IF
               END-PERFORM
DEBUG>D        DISPLAY "AT " LINECTR
DEBUG>D            " DPTR>CPTR=" DIGITPTR " > " CHARPTR
DEBUG>D            " DIG=" LINE-D1 " CHR=" NAME-D1
               IF DIGITPTR > CHARPTR
                   MOVE LINE-D2 TO NAME-D2
               END-IF
               ADD NAME-VALUE TO PART2-VALUE
DEBUG>D        DISPLAY "AT " LINECTR " CAL=" NAME-VALUE
DEBUG>D            " PT2=" PART2-VALUE
DEBUG>D        DISPLAY "AT " LINECTR " 2ND=" NAME-D2
DEBUG>D            " PT2=" PART2-VALUE " CPTR=" CHARPTR
               READ INPUT-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD +1 TO LINECTR
DEBUG>D                DISPLAY "LINE " LINECTR " LEN=" LINELEN
               END-READ
           END-PERFORM
           DISPLAY "PART1=" PART1-VALUE.
           DISPLAY "PART2=" PART2-VALUE.

       900-WRAP-UP.
           CLOSE INPUT-FILE.
