      *==============================
       IDENTIFICATION DIVISION.
      *==============================
       PROGRAM-ID.    COV1903
       AUTHOR.        Georges B.
      *Report Covid19 API Data ...
      *==============================
       ENVIRONMENT DIVISION.
      *==============================
       INPUT-OUTPUT SECTION.
      *------------------------------
       FILE-CONTROL.
           SELECT PRINT-LINE   ASSIGN TO PRTLINE.
      *
           SELECT COVID19D-FILE ASSIGN TO COVID19D
              ORGANIZATION   IS INDEXED
              ACCESS MODE    IS SEQUENTIAL
              RECORD KEY     IS CV-CODE-DATE
              FILE STATUS    IS WS-COVID19D-STATUS
           .
      *==============================
       DATA DIVISION.
      *==============================
       FILE SECTION.
      *------------------------------
       FD  PRINT-LINE RECORDING MODE F.
       01  PRINT-REC               PIC X(132).
      *    03 CV-CODE-O            PIC BXXB.
      *    03 FILLER               PIC X(02).
      *    03 CV-DATE-O            PIC X(10).
      *    03 FILLER               PIC X(02).
      *    03 CV-NEW-CONFIRMED-O   PIC X(15).
      *    03 FILLER               PIC X(60).
      *
       FD  COVID19D-FILE.
       COPY COVID19D REPLACING ==:TAG1:== BY ==COVID19D==
                               ==:TAG2:== BY ==CV==.
      *
       WORKING-STORAGE SECTION.
      *------------------------------
       77  WS-CODE-PREVIOUS        PIC X(02) VALUE SPACES.
       77  WS-C-CODE-TO-READ       PIC X(02) VALUE SPACES.
       77  WS-SUB-PGM-NAME         PIC X(08) VALUE SPACES.
      *
       01 WS-COVID19D-STATUS       PIC 9(02) VALUE ZEROS.
      *
       01 FLAGS.
           03 WS-LAST-REC          PIC X VALUE SPACE.
      *
       01  COUNTERS.
           03 WS-COVID19D-T-RECS   PIC 9(05) COMP-3 VALUE ZEROES.
      *
       COPY COUNTRYS REPLACING     ==:TAG1:==     BY ==WS-COUNTRYS==
                                   ==:TAG2:==     BY ==WS-C==.
       COPY CALLSTUS REPLACING     ==:RESULT:==   BY ==RESULT==.
       COPY DATETIME REPLACING     ==:TAG:==      BY ==WS-CURRENT==.
       COPY DATEMDYO REPLACING     ==:TAG:==      BY ==WS-CURRENT==
                                   ==:TAG1:==     BY =='/'==.
      *
       01  WS-HEADER-1.
           03 FILLER               PIC X(20) VALUE 'COV1903'.
           03 TITLE-1              PIC X(40) VALUE
                '*** FILE COVID19D REPORT *** '.
           03 FILLER               PIC X(08) VALUE 'Date: '.
           03 DATE-1               PIC X(10).
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(08) VALUE 'Time: '.
           03 TIME-1               PIC X(08).
           03 FILLER               PIC X(44).
      *
       01  WS-HEADER-2-A.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(10).
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE 'NEW'.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE 'TOTAL'.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE 'NEW'.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE 'TOTAL'.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE 'NEW'.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE 'TOTAL'.
           03 FILLER               PIC X(02).
      *
       01  WS-HEADER-2-B.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(10) VALUE 'DATE'.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE 'CONFIRMED'.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE 'CONFIRMED'.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE 'DEATHS'.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE 'DEATHS'.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE 'RECOVERED'.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE 'RECOVERED'.
      *
       01  WS-HEADER-3.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(10) VALUE ALL '='.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE ALL '='.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE ALL '='.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE ALL '='.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE ALL '='.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE ALL '='.
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(12) VALUE ALL '='.
      *
       01  WS-DETAILS-1.
           03 CV-COUNTRY-LABEL-O   PIC X(10).
           03 CV-CODE-O            PIC X(02).
           03 CV-DATE-SEPARATOR-1  PIC X(03).
           03 CV-COUNTRY-NAME-O    PIC X(50).
           03 FILLER               PIC X(67).
      *
       01  WS-DETAILS-2.
           03 FILLER               PIC X(02).
           03 CV-DATE-O.
            05 CV-DATE-YEAR-O      PIC 9(04).
            05 CV-DATE-SEPARATOR-2A PIC X(01).
            05 CV-DATE-MONTH-O     PIC 9(02).
            05 CV-DATE-SEPARATOR-2B PIC X(01).
            05 CV-DATE-DAY-O       PIC 9(02).
           03 FILLER               PIC X(02).
           03 CV-NEW-CONFIRMED-O   PIC ZZZZ,ZZZ,ZZ9.
           03 FILLER               PIC X(02).
           03 CV-TOTAL-CONFIRMED-O PIC ZZZZ,ZZZ,ZZ9.
           03 FILLER               PIC X(02).
           03 CV-NEW-DEATHS-O      PIC ZZZZ,ZZZ,ZZ9.
           03 FILLER               PIC X(02).
           03 CV-TOTAL-DEATHS-O    PIC ZZZZ,ZZZ,ZZ9.
           03 FILLER               PIC X(02).
           03 CV-NEW-RECOVERED-O   PIC ZZZZ,ZZZ,ZZ9.
           03 FILLER               PIC X(02).
           03 CV-TOTAL-RECOVERED-O PIC ZZZZ,ZZZ,ZZ9.
      *
       01  WS-TRAILER-1.
           03 FILLER              PIC X(40) VALUE
                        '*** End of file COVID19D Report. '.
           03 COVID19D-T-RECS     PIC ZZZ,ZZ9.
           03 FILLER              PIC X(30) VALUE
                        ' records were found. ***'.
      *==============================
       PROCEDURE DIVISION.
      *==============================
       OPEN-FILES.
           OPEN INPUT  COVID19D-FILE
           OPEN OUTPUT PRINT-LINE
           .
      *
       WRITE-HEADERS.
      *    GET DATE ....
           MOVE SPACES                  TO PRINT-REC
           MOVE FUNCTION CURRENT-DATE   TO WS-CURRENT-DATE-DATA
           MOVE WS-CURRENT-MONTH        TO WS-CURRENT-MONTH-O
           MOVE WS-CURRENT-DAY          TO WS-CURRENT-DAY-O
           MOVE WS-CURRENT-YEAR         TO WS-CURRENT-YEAR-O
      *
           MOVE WS-CURRENT-HOURS        TO WS-CURRENT-HOURS-O
           MOVE WS-CURRENT-MINUTES      TO WS-CURRENT-MINUTES-O
           MOVE WS-CURRENT-SECONDS      TO WS-CURRENT-SECONDS-O
      *
           MOVE WS-CURRENT-DATE-O       TO DATE-1
           MOVE WS-CURRENT-TIME-O       TO TIME-1
      *
           WRITE PRINT-REC            FROM WS-HEADER-1
           WRITE PRINT-REC            FROM WS-HEADER-2-A
           WRITE PRINT-REC            FROM WS-HEADER-2-B
           WRITE PRINT-REC            FROM WS-HEADER-3
           .
      *
       READ-NEXT-RECORD.
           PERFORM READ-RECORD
           PERFORM UNTIL WS-LAST-REC = 'Y'
      *     PERFORM 20  TIMES
                PERFORM WRITE-RECORD
                PERFORM READ-RECORD
            END-PERFORM
           .
      *
       WRITE-TRAILERS.
           MOVE SPACES                  TO PRINT-REC
           MOVE WS-COVID19D-T-RECS      TO COVID19D-T-RECS
           WRITE PRINT-REC            FROM WS-TRAILER-1
           .
      *
       CLOSE-STOP.
           CLOSE        COVID19D-FILE
                        PRINT-LINE
           GOBACK
           .
      *
       READ-RECORD.
           READ COVID19D-FILE
                AT END       MOVE 'Y' TO WS-LAST-REC
                NOT AT END   ADD 1 TO WS-COVID19D-T-RECS
           END-READ
           .
      *
       WRITE-RECORD.
           MOVE SPACES             TO WS-DETAILS-1 WS-DETAILS-2
                                      PRINT-REC
      *    CHECKING COUNTRY BREAKING ...
           IF CV-CODE = WS-CODE-PREVIOUS
             MOVE SPACES           TO CV-CODE-O
           ELSE
             MOVE CV-CODE          TO WS-CODE-PREVIOUS
             MOVE CV-CODE          TO WS-C-CODE-TO-READ
             MOVE SPACES           TO WS-COUNTRYS-REC WS-RESULT-INFO
             PERFORM GET-COUNTRY-DATA
             MOVE ALL '-'          TO WS-DETAILS-1
             WRITE PRINT-REC     FROM WS-DETAILS-1
             MOVE SPACES           TO WS-DETAILS-1
             MOVE 'Country: '      TO CV-COUNTRY-LABEL-O
             MOVE CV-CODE          TO CV-CODE-O
             MOVE ' - '            TO CV-DATE-SEPARATOR-1
             MOVE WS-C-NAME        TO CV-COUNTRY-NAME-O
             WRITE PRINT-REC     FROM WS-DETAILS-1
             MOVE ALL '-'          TO WS-DETAILS-1
             WRITE PRINT-REC     FROM WS-DETAILS-1
           END-IF

           MOVE SPACES             TO WS-DETAILS-2
           MOVE CV-DATE-MONTH      TO CV-DATE-MONTH-O
           MOVE '-'                TO CV-DATE-SEPARATOR-2A
           MOVE CV-DATE-DAY        TO CV-DATE-DAY-O
           MOVE '-'                TO CV-DATE-SEPARATOR-2B
           MOVE CV-DATE-YEAR       TO CV-DATE-YEAR-O
           MOVE CV-NEW-CONFIRMED   TO CV-NEW-CONFIRMED-O
           MOVE CV-TOTAL-CONFIRMED TO CV-TOTAL-CONFIRMED-O
           MOVE CV-NEW-DEATHS      TO CV-NEW-DEATHS-O
           MOVE CV-TOTAL-DEATHS    TO CV-TOTAL-DEATHS-O
           MOVE CV-NEW-RECOVERED   TO CV-NEW-RECOVERED-O
           MOVE CV-TOTAL-RECOVERED TO CV-TOTAL-RECOVERED-O
           WRITE PRINT-REC       FROM WS-DETAILS-2
           .
      *
       GET-COUNTRY-DATA.
           MOVE 'SUB0002'          TO WS-SUB-PGM-NAME
           CALL WS-SUB-PGM-NAME USING
             WS-C-CODE-TO-READ, WS-COUNTRYS-REC, WS-RESULT-INFO
           .
      *