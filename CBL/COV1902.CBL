      *==============================
       IDENTIFICATION DIVISION.
      *==============================
       PROGRAM-ID.    COV1902
       AUTHOR.        Georges B.
      *Report Covid19 Countries ...
      *==============================
       ENVIRONMENT DIVISION.
      *==============================
       INPUT-OUTPUT SECTION.
      *------------------------------
       FILE-CONTROL.
           SELECT PRINT-LINE   ASSIGN TO PRTLINE.
      *
           SELECT COUNTRYS-FILE ASSIGN TO COUNTRYS
              ORGANIZATION   IS INDEXED
              ACCESS MODE    IS SEQUENTIAL
              RECORD KEY     IS C-CODE
              FILE STATUS    IS WS-COUNTRYS-STATUS
           .
      *==============================
       DATA DIVISION.
      *==============================
       FILE SECTION.
      *------------------------------
       FD  PRINT-LINE RECORDING MODE F.
       01  PRINT-REC.
           03 C-CODE-O            PIC BXXB.
           03 FILLER              PIC X(02).
           03 C-NAME-O            PIC X(50).
           03 FILLER              PIC X(02).
           03 C-SLUG-O            PIC X(50).
      *
       FD  COUNTRYS-FILE.
       COPY COUNTRYS REPLACING ==:TAG1:== BY ==COUNTRYS==
                               ==:TAG2:== BY ==C==.
      *
       WORKING-STORAGE SECTION.
      *------------------------------
       01  WS-COUNTRYS-STATUS      PIC 99 VALUE ZEROS.
      *
       01 FLAGS.
           03 LASTREC              PIC X VALUE SPACE.
           03 WS-PAGE-BREAK        PIC 9(02) VALUE 65.
      *
       01  COUNTERS.
           03 WS-COUNTRYS-T-RECS   PIC 9(05) COMP-3 VALUE ZEROES.
           03 WS-PG-WRITTEN-LINES  PIC 9(02) VALUE ZEROS.
           03 WS-PAGE-NUMBER       PIC 9(05) VALUE ZEROS.
      *
       COPY DATETIME     REPLACING ==:TAG:==      BY ==WS-CURRENT==.
       COPY DATEMDYO     REPLACING ==:TAG:==      BY ==WS-CURRENT==
                                   ==:TAG1:==     BY =='/'==.
      *
       01  WS-HEADER-1.
           03 FILLER               PIC X(16) VALUE 'COV1902'.
           03 TITLE-1              PIC X(40) VALUE
                '*** COUNTRIES REPORT *** '.
           03 FILLER               PIC X(08) VALUE 'Date: '.
           03 DATE-1               PIC X(10).
           03 FILLER               PIC X(02).
           03 FILLER               PIC X(08) VALUE 'Time: '.
           03 TIME-1               PIC X(08).
           03 FILLER               PIC X(04).
           03 FILLER               PIC X(07) VALUE 'Page: '.
           03 PAGE-NUMBER          PIC ZZZZ9.
           03 FILLER               PIC X(28).
      *
       01  WS-HEADER-2.
           03 COUNTRY-CODE        PIC X(04) VALUE 'CODE'.
           03 FILLER              PIC X(02).
           03 COUNTRY-SLUG        PIC X(50) VALUE 'NAME'.
           03 FILLER              PIC X(02).
           03 COUNTRY-NAME        PIC X(50) VALUE 'SLUG'.
      *
       01  WS-HEADER-3.
           03 FILLER              PIC X(04) VALUE '===='.
           03 FILLER              PIC X(02).
           03 COUNTRY-SLUG        PIC X(50) VALUE ALL '='.
           03 FILLER              PIC X(02).
           03 COUNTRY-NAME        PIC X(50) VALUE ALL '='.
      *
       01  WS-TRAILER-1.
           03 FILLER              PIC X(30) VALUE
                        '*** End of Countries Report. '.
           03 COUNTRYS-T-RECS     PIC ZZZ,ZZ9.
           03 FILLER              PIC X(30) VALUE
                        ' Countries were found. ***'.
      *==============================
       PROCEDURE DIVISION.
      *==============================
       OPEN-FILES.
           OPEN INPUT  COUNTRYS-FILE
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
           ADD 1                        TO WS-PAGE-NUMBER
           MOVE WS-PAGE-NUMBER          TO PAGE-NUMBER
      *
           WRITE PRINT-REC            FROM WS-HEADER-1
                 AFTER ADVANCING PAGE
           WRITE PRINT-REC            FROM WS-HEADER-2
           WRITE PRINT-REC            FROM WS-HEADER-3
           MOVE 3                       TO WS-PG-WRITTEN-LINES
           .
      *
       READ-NEXT-RECORD.
           PERFORM READ-RECORD
            PERFORM UNTIL LASTREC = 'Y'
                PERFORM WRITE-RECORD
                PERFORM READ-RECORD
            END-PERFORM
           .
      *
       WRITE-TRAILERS.
           MOVE SPACES                  TO PRINT-REC
           MOVE WS-COUNTRYS-T-RECS      TO COUNTRYS-T-RECS
           WRITE PRINT-REC            FROM WS-TRAILER-1
           .
      *
       CLOSE-STOP.
           CLOSE        COUNTRYS-FILE
                        PRINT-LINE
           GOBACK
           .
      *
       READ-RECORD.
           READ COUNTRYS-FILE
                AT END       MOVE 'Y' TO LASTREC
                NOT AT END   ADD 1 TO WS-COUNTRYS-T-RECS
           END-READ
           .
      *
       WRITE-RECORD.
           IF WS-PG-WRITTEN-LINES > WS-PAGE-BREAK
             PERFORM WRITE-HEADERS
             MOVE 0           TO WS-PG-WRITTEN-LINES
           END-IF

      *    DISPLAY 'Country Code: ' C-CODE
      *    DISPLAY 'Country Name: ' C-NAME
      *    DISPLAY 'Country Slug: ' C-SLUG
           MOVE SPACES       TO PRINT-REC
           MOVE C-CODE       TO C-CODE-O
           MOVE C-NAME       TO C-NAME-O
           MOVE C-SLUG       TO C-SLUG-O

           WRITE PRINT-REC
           ADD 1             TO WS-PG-WRITTEN-LINES
           .
      *