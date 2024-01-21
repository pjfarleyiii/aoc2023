       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY15PT2.
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
           05  HASH-DEL-CNT              PIC S9(04) COMP-5.
           05  HASH-EQL-CNT              PIC S9(04) COMP-5.
           05  BOX-LBL                   PIC S9(04) COMP-5.
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
           05  HASH-FOC-LEN              PIC  9.
           05  GROUP-TEXT                PIC  X(16).
           05  LABEL-SRCH                PIC  X(07).
           05  HASHLINE                  PIC  X(32756).

       01 BOXS-AREA.
          05  BOXSMAX                    PIC S9(04) COMP-5 VALUE +0.
          05  BOXS-DATA.
              10  BOXS-TABLE             OCCURS 256 TIMES
                                         INDEXED BY BOXSNDX.
                  15  LENSMAX            PIC S9(04) COMP-5 VALUE +0.
                  15  LENS-TABLE         OCCURS 256 TIMES
                                         INDEXED BY LENSNDX.
                      20  LENS-LABEL     PIC  X(07) VALUE SPACES.
                      20  LENS-LENGTH    PIC  9     VALUE ZERO.

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
               DISPLAY LINEINPUT (1 : FUNCTION MIN (250, LINELEN))
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
      D            UNTIL S41 > FUNCTION MIN (250, LINELEN)
      D            DISPLAY FUNCTION HEX-OF (
      D                HASHLINE (S41: FUNCTION MIN (64, LINELEN)) )
      D        END-PERFORM
      *        IF (S41 - 64) > +0
      *            DISPLAY FUNCTION HEX-OF (HASHLINE (S41 - 64 :
      *                FUNCTION MOD (LINELEN, 64)))
      *        END-IF
      *        EXTRACT GROUPS OF TEXT SEPARATED BY COMMAS
      *        HASH EACH GROUP AND OBEY THE INSTRUCTION TO STORE/DELETE
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
      D                ",SCANNING '"
      D                LINEINPUT (S48 : FUNCTION MIN (88, S47))
      D                "'"
                   UNSTRING LINEINPUT (1 : LINELEN)
                       DELIMITED BY ","
                       INTO GROUP-TEXT
                       COUNT   HASHLEN
                       POINTER HASHPTR
                   END-UNSTRING
                   MOVE +0 TO HASH-DEL-CNT HASH-EQL-CNT
                   INSPECT GROUP-TEXT TALLYING HASH-DEL-CNT
                       FOR ALL "-"
                   INSPECT GROUP-TEXT TALLYING HASH-EQL-CNT
                       FOR ALL "="
                   COMPUTE HASHLEN = HASHLEN - HASH-DEL-CNT -
                       (2 * HASH-EQL-CNT)
                   IF HASH-EQL-CNT = 1
                       MOVE GROUP-TEXT (HASHLEN + 2 : 1) TO HASH-FOC-LEN
                   ELSE
                       MOVE ZERO TO HASH-FOC-LEN
                   END-IF
      D            DISPLAY "HASH TEXT IS '" GROUP-TEXT (1 : HASHLEN) "'"
      D                ",COUNT=" HASHLEN ",POINTER=" HASHPTR
      D                ",START=" S48
      D                ",DEL-CNT=" HASH-DEL-CNT ",EQL-CNT=" HASH-EQL-CNT
      D                ",FOCAL-LEN=" HASH-FOC-LEN
      *            SAVE START FOR HASH CALCULATION
                   MOVE S48 TO HASH-START
      *            MOVE START TO NEXT GROUP
                   COMPUTE S48 = HASHPTR - 1
      D            DISPLAY "NEXT COMMA AT " S48
                   IF HASHLEN = +0
      D                DISPLAY "UNSTRING LENGTH ZERO, EXITING"
                       EXIT PERFORM
                   ELSE
                       PERFORM 200-CALC-ORD
      D                DISPLAY "HASH VALUE IS " HASH-VALUE
                   END-IF
      *            NOW PERFORM THE OPERATION REQUESTED ON THE LENS BOX
      *            BOX NUMBER IS HASH-VALUE + 1 BECAUSE COBOL TABLE
      *            INDEXES ARE 1-BASED NOT ZERO-BASED
                   COMPUTE BOX-LBL = HASH-VALUE + 1
                   MOVE GROUP-TEXT (1 : HASHLEN) TO LABEL-SRCH
      D            DISPLAY "LABEL#=" BOX-LBL ",LABEL='" LABEL-SRCH "'"
                   PERFORM 300-SEARCH-BOX
               END-PERFORM
      D        DISPLAY "CONTENTS OF ALL NON-EMPTY BOXES, 0-BASED"
      D        PERFORM VARYING BOXSNDX FROM 1 BY 1 UNTIL BOXSNDX > 256
      D            SET S41 TO BOXSNDX
      D            SUBTRACT +1 FROM S41
      D            IF LENSMAX (BOXSNDX) > +0
      D                PERFORM VARYING LENSNDX FROM 1 BY 1
      D                    UNTIL LENSNDX > LENSMAX (BOXSNDX)
      D                    SET S42 TO LENSNDX
      D                    SUBTRACT +1 FROM S42
      D                    DISPLAY "BOX[" S41 "]"
      D                        ",LENS[" S42 "]=('"
      D                        LENS-LABEL (BOXSNDX, LENSNDX)
      D                        "'," LENS-LENGTH (BOXSNDX, LENSNDX) ")"
      D                END-PERFORM
      D            END-IF
      D        END-PERFORM
      *        NOW CALCULATE THE SUM OF THE 1-BASED BOX NUMBER TIMES
      *        EACH FOCAL LENGTH TIMES ITS POSITION FOR NON-EMPTY BOXES
               PERFORM VARYING BOXSNDX FROM 1 BY 1 UNTIL BOXSNDX > 256
                   SET S41 TO BOXSNDX
                   IF LENSMAX (BOXSNDX) > +0
                       PERFORM VARYING LENSNDX FROM 1 BY 1
                           UNTIL LENSNDX > LENSMAX (BOXSNDX)
                           SET S42 TO LENSNDX
                           COMPUTE HASH-VALUE = S41 * S42 *
                               LENS-LENGTH (BOXSNDX, LENSNDX)
      D                    DISPLAY "BOX[" S41 "] * LENS[" S42
      D                        "] * FOCAL-LEN["
      D                        LENS-LENGTH (BOXSNDX, LENSNDX)
      D                        "] = " HASH-VALUE
                           ADD HASH-VALUE TO HASH-TOTAL
                       END-PERFORM
                   END-IF
               END-PERFORM
               DISPLAY "LENS TOTAL IS " HASH-TOTAL
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

       300-SEARCH-BOX.
           SET BOXSNDX TO BOX-LBL
      D    DISPLAY "SEARCH FOR '" LABEL-SRCH "' IN BOX=" BOX-LBL
           EVALUATE TRUE
               WHEN HASH-EQL-CNT = 1
      D            DISPLAY "EQUAL  PATH,MAX=" LENSMAX (BOXSNDX)
                   IF LENSMAX (BOXSNDX) > +0
                       MOVE +1 TO S41
                       SET LENSNDX TO 1
                       SEARCH LENS-TABLE           VARYING S41
                           AT END
      D                        DISPLAY "LABEL '" LABEL-SRCH
      D                            "' NOT FOUND, ADDING '" LABEL-SRCH
      D                            "',LENGTH=" HASH-FOC-LEN
                               ADD +1 TO LENSMAX (BOXSNDX)
                               MOVE LENSMAX (BOXSNDX) TO S41
                               MOVE LABEL-SRCH
                                 TO LENS-LABEL (BOXSNDX, S41)
                               MOVE HASH-FOC-LEN
                                 TO LENS-LENGTH (BOXSNDX, S41)
                           WHEN LABEL-SRCH =
                                LENS-LABEL (BOXSNDX, S41)
      D                        DISPLAY "LABEL '" LABEL-SRCH
      D                            "' FOUND, REPLACING LENGTH="
      D                            LENS-LENGTH (BOXSNDX, S41)
      D                            " BY " HASH-FOC-LEN
                               MOVE HASH-FOC-LEN
                                 TO LENS-LENGTH (BOXSNDX, S41)
                       END-SEARCH
                   ELSE
      D                DISPLAY "BOX=" BOX-LBL
      D                    " EMPTY, ADDING LABEL '" LABEL-SRCH "'"
      D                    ",LENGTH=" HASH-FOC-LEN
                       ADD +1 TO LENSMAX (BOXSNDX)
                       MOVE LENSMAX (BOXSNDX) TO S41
                       MOVE LABEL-SRCH TO LENS-LABEL (BOXSNDX, S41)
                       MOVE HASH-FOC-LEN
                         TO LENS-LENGTH (BOXSNDX, S41)
                   END-IF
               WHEN HASH-DEL-CNT = 1
      D            DISPLAY "DELETE PATH,MAX=" LENSMAX (BOXSNDX)
                   IF LENSMAX (BOXSNDX) > +0
                       MOVE +1 TO S41
                       SET LENSNDX TO 1
                       SEARCH LENS-TABLE           VARYING S41
                           AT END
                               COMPUTE S42 = LENSMAX (BOXSNDX) + 1
                           WHEN LABEL-SRCH =
                                LENS-LABEL (BOXSNDX, S41)
                               MOVE S41 TO S42
                       END-SEARCH
      D                DISPLAY "DELETE SRCH=" S41 "," S42
                       IF S41 > LENSMAX (BOXSNDX)
      D                    DISPLAY "LABEL '" LABEL-SRCH
      D                        "' NOT FOUND, NOTHING TO DELETE"
                           CONTINUE
      *                ELSE IF S41 = LENSMAX (BOXSNDX)
      *                    SUBTRACT +1 FROM LENSMAX (BOXSNDX)
                       ELSE
                           PERFORM VARYING S41 FROM S42 BY 1
                               UNTIL S41 >= LENSMAX (BOXSNDX)
                               MOVE LENS-TABLE (BOXSNDX, S41 + 1)
                                 TO LENS-TABLE (BOXSNDX, S41)
                           END-PERFORM
                           MOVE SPACES TO LENS-LABEL  (BOXSNDX, S41)
                           MOVE ZERO   TO LENS-LENGTH (BOXSNDX, S41)
                           SUBTRACT +1 FROM LENSMAX (BOXSNDX)
                       END-IF
                   ELSE
      D                DISPLAY "BOX=" BOX-LBL
      D                    " EMPTY, NOTHING TO DELETE"
                       CONTINUE
                   END-IF
           END-EVALUATE
      D    DISPLAY "CONTENTS OF BOX " BOX-LBL
      D    IF LENSMAX (BOXSNDX) = +0
      D        DISPLAY "BOX IS EMPTY"
      D    ELSE
      D        PERFORM VARYING LENSNDX FROM 1 BY 1
      D            UNTIL LENSNDX > LENSMAX (BOXSNDX)
      D            SET S41 TO LENSNDX
      D            DISPLAY "LENS[" S41 "]=('"
      D                LENS-LABEL (BOXSNDX, LENSNDX)
      D                "'," LENS-LENGTH (BOXSNDX, LENSNDX) ")"
      D        END-PERFORM
      D    END-IF
           CONTINUE.

       900-WRAP-UP.
           CLOSE INPUT-FILE.
