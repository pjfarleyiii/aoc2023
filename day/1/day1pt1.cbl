       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY1PT1.
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

       01  LINE-VALUE-PT1.
           05  CHARPTR           PIC  9(8) BINARY.
           05  PART1-VALUE       PIC  9(8) VALUE ZEROES.
           05  LINE-VALUE        PIC  99.
           05  FILLER            REDEFINES LINE-VALUE.
               10  LINE-D1       PIC  9.
               10  LINE-D2       PIC  9.

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
               PERFORM VARYING CHARPTR FROM +1 BY +1
                   UNTIL CHARPTR > LINELEN
                   IF LINEINPUT (CHARPTR : 1) NUMERIC
                       MOVE LINEINPUT (CHARPTR : 1)
                         TO LINE-D1
DEBUG>D                DISPLAY "AT " LINECTR " 1ST=" LINE-D1
DEBUG>D                    "             "     " CPTR=" CHARPTR
                       EXIT PERFORM
                   END-IF
               END-PERFORM
               PERFORM VARYING CHARPTR FROM LINELEN BY -1
                   UNTIL CHARPTR < +1
                   IF LINEINPUT (CHARPTR : 1) NUMERIC
                       MOVE LINEINPUT (CHARPTR : 1)
                         TO LINE-D2
                       ADD LINE-VALUE TO PART1-VALUE
                       DISPLAY "AT " LINECTR " CAL=" LINE-VALUE
                           " PT1=" PART1-VALUE
DEBUG>D                DISPLAY "AT " LINECTR " 2ND=" LINE-D2
DEBUG>D                    " PT1=" PART1-VALUE " CPTR=" CHARPTR
                       EXIT PERFORM
                   END-IF
               END-PERFORM
               READ INPUT-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD +1 TO LINECTR
DEBUG>D                DISPLAY "LINE " LINECTR " LEN=" LINELEN
               END-READ
           END-PERFORM
           DISPLAY "PART1=" PART1-VALUE.

       900-WRAP-UP.
           CLOSE INPUT-FILE.
