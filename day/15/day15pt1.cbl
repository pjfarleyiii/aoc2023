       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY15PT1.
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
           05  SW-END-OF-FILE    PIC X(01) VALUE SPACES.
               88 END-OF-FILE              VALUE "Y".

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

       01  WORK-VARIABLES.
           05  HASH-TOTAL                PIC S9(09) COMP-5.
           05  HASH-VALUE                PIC S9(04) COMP-5.
           05  HASH-START                PIC S9(04) COMP-5.
           05  HASHLEN                   PIC S9(04) COMP-5.
           05  HASHPTR                   PIC S9(04) COMP-5.
           05  ASCII-TABLE.
               10  FILLER PIC X(16) VALUE "????????????????".
               10  FILLER PIC X(16) VALUE "????????????????".
               10  FILLER PIC X(16) VALUE " !""#$%&'()*+,-./".
               10  FILLER PIC X(16) VALUE "0123456789:;<=>?".
               10  FILLER PIC X(16) VALUE "@ABCDEFGHIJKLMNO".
               10  FILLER PIC X(16) VALUE "PQRSTUVWXYZ[\]^_".
               10  FILLER PIC X(16) VALUE "`abcdefghijklmno".
               10  FILLER PIC X(16) VALUE "pqrstuvwxyz{|}~?".
           05  EBCDIC-TABLE.
           10  X_00 PIC X(16) VALUE X"000102031A091A7F1A1A1A0B0C0D0E0F".
           10  X_10 PIC X(16) VALUE X"101112131A1A081A18191A1A1C1D1E1F".
           10  X_20 PIC X(16) VALUE X"1A1A1A1A1A0A171B1A1A1A1A1A050607".
           10  X_30 PIC X(16) VALUE X"1A1A161A1A1A1A041A1A1A1A14151A1A".
           10  X_40 PIC X(16) VALUE X"201A1A1A1A1A1A1A1A1A5B2E3C282B21".
           10  X_50 PIC X(16) VALUE X"261A1A1A1A1A1A1A1A1A5D242A293B5E".
           10  X_60 PIC X(16) VALUE X"2D1A1A1A1A1A1A1A1A1A7C2C255F3E3F".
           10  X_70 PIC X(16) VALUE X"1A1A1A1A1A1A1A1A1A603A2340273D22".
           10  X_80 PIC X(16) VALUE X"1A6162636465666768691A1A1A1A1A1A".
           10  X_90 PIC X(16) VALUE X"1A6A6B6C6D6E6F7071721A1A1A1A1A1A".
           10  X_A0 PIC X(16) VALUE X"1A7E737475767778797A1A1A1A1A1A1A".
           10  X_B0 PIC X(16) VALUE X"1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A".
           10  X_C0 PIC X(16) VALUE X"7B4142434445464748491A1A1A1A1A1A".
           10  X_D0 PIC X(16) VALUE X"7D4A4B4C4D4E4F5051521A1A1A1A1A1A".
           10  X_E0 PIC X(16) VALUE X"5C1A535455565758595A1A1A1A1A1A1A".
           10  X_F0 PIC X(16) VALUE X"303132333435363738391A1A1A1A1A1A".
           05  BYTE-CONVERT.
               10  FILLER                PIC  X VALUE LOW-VALUES.
               10  BYTE-CVT              PIC  X VALUE LOW-VALUES.
           05  CHAR-CVT                  REDEFINES BYTE-CONVERT
                                         PIC S9(04) COMP-5.
           05  GROUP-TEXT                PIC  X(16).
           05  HASHLINE                  PIC  X(32756).

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
               DISPLAY LINEINPUT (1 : LINELEN)
      *        ONLY USE THESE TWO LINES TO TEST THE HASH ALGORITHM
      *        MOVE "HASH" TO LINEINPUT (1 : 4)
      *        MOVE +4 TO LINELEN
      *        CONVERT WHOLE LINE TO ASCII TO BE USED LATER
               PERFORM VARYING S41 FROM 1 BY 1
                   UNTIL S41 > LINELEN
                   MOVE LINEINPUT (S41 : 1) TO BYTE-CVT
                   MOVE EBCDIC-TABLE (CHAR-CVT + 1 : 1)
                     TO HASHLINE (S41 : 1)
      *            ONLY UNCOMMENT THESE LINES TO DEBUG ASCII CONVERSION
      *            DISPLAY "CHAR='" LINEINPUT (S41 : 1) "'=X'"
      *                FUNCTION HEX-OF (LINEINPUT (S41 : 1))
      *                "'=" CHAR-CVT
      *                " ==> X'" FUNCTION HEX-OF (HASHLINE (S41 : 1))
      *                "'"
               END-PERFORM
      D        PERFORM VARYING S41 FROM 1 BY 64
      D            UNTIL S41 > LINELEN
      D            DISPLAY FUNCTION HEX-OF (
      D                HASHLINE (S41: FUNCTION MIN (64, LINELEN)) )
      D        END-PERFORM
      *        IF (S41 - 64) > +0
      *            DISPLAY FUNCTION HEX-OF (HASHLINE (S41 - 64 :
      *                FUNCTION MOD (LINELEN, 64)))
      *        END-IF
      *        EXTRACT GROUPS OF TEXT SEPARATED BY COMMAS
      *        HASH EACH GROUP AND ADD THAT TO THE HASH TOTAL
               MOVE ZERO TO S41 HASH-TOTAL
               INSPECT LINEINPUT (1 : LINELEN) TALLYING S41 FOR ALL ","
               ADD  +1 TO S41
      D        DISPLAY "FIELDS=" S41
               PERFORM VARYING S48 FROM 1 BY 1 UNTIL S48 > LINELEN
                   MOVE SPACES TO GROUP-TEXT
                   MOVE +0 TO HASHLEN
                   MOVE S48 TO HASHPTR
      D            COMPUTE S47 = LINELEN - S48 + 1
      D            DISPLAY "SCAN START AT " S48
      D                ",SCAN LEN=" S47
      D                ",SCANNING '" LINEINPUT (S48 : S47)
      D                "'"
                   UNSTRING LINEINPUT (1 : LINELEN)
                       DELIMITED BY ","
                       INTO GROUP-TEXT
                       COUNT   HASHLEN
                       POINTER HASHPTR
                   END-UNSTRING
      D            DISPLAY "HASH TEXT IS '" GROUP-TEXT (1 : HASHLEN) "'"
      D                ",COUNT=" HASHLEN ",POINTER=" HASHPTR
      D                ",START=" S48
      *            SAVE START FOR HASH CALCULATION
                   MOVE S48 TO HASH-START
      *            MOVE START TO NEXT GROUP
                   COMPUTE S48 = HASHPTR - 1
      D            DISPLAY "NEXT COMMA AT " S48
                   IF HASHLEN = +0
      D                DISPLAY "UNSTRING LENGTH ZERO, EXITING"
                       EXIT PERFORM
                   Else
                       PERFORM 200-CALC-ORD
      D                DISPLAY "HASH VALUE IS " HASH-VALUE
                   END-IF
                   ADD HASH-VALUE TO HASH-TOTAL
               END-PERFORM
               DISPLAY "HASH TOTAL IS " HASH-TOTAL
               READ INPUT-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD +1 TO LINECTR
DEBUG>D                DISPLAY "LINE " LINECTR " LEN=" LINELEN
               END-READ
           END-PERFORM
           DISPLAY " ".

       200-CALC-ORD.
      *    NOW APPLY HASH ALGORITHM
           MOVE +0 TO HASH-VALUE
           PERFORM VARYING S41 FROM HASH-START BY 1
               UNTIL S41 > HASH-START + HASHLEN - 1
      D        COMPUTE S42 = HASH-VALUE
      D        COMPUTE S43 = FUNCTION ORD (HASHLINE (S41 : 1)) - 1
      D        COMPUTE S44 = S42 + S43
      D        COMPUTE S45 = S44 * 17
      D        COMPUTE S46 = FUNCTION MOD (S45, 256)
      D        DISPLAY "H=" S42
      D            " + ORD(X'" FUNCTION HEX-OF (HASHLINE (S41 : 1))
      D            "')=" S43 "=" S44 ",ORD(...)*17=" S45
      D            ",MOD(..., 256)=" S46
               COMPUTE HASH-VALUE = FUNCTION MOD (
                   (HASH-VALUE +
                    FUNCTION ORD (HASHLINE (S41 : 1)) - 1) * 17,
                    256)
           END-PERFORM
           CONTINUE.

       900-WRAP-UP.
           CLOSE INPUT-FILE.
