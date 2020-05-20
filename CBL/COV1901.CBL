      *=============================
       IDENTIFICATION DIVISION.
      *=============================
       PROGRAM-ID.     COV1901.
       AUTHOR.         GEORGES B.
      *=============================
       ENVIRONMENT DIVISION.
      *=============================
       INPUT-OUTPUT SECTION.
      *-----------------------------
       FILE-CONTROL.
           SELECT   COVID19CSV-FILE     ASSIGN COV19CSV
      *             ORGANIZATION        IS LINE SEQUENTIAL
      *             ACCESS MODE         IS SEQUENTIAL
      *             FILE STATUS         IS WS-COV19CSV-STATUS
           .
           SELECT   COUNTRYS-FILE       ASSIGN COUNTRYS
                    ORGANIZATION        IS INDEXED
      *             ACCESS MODE         IS SEQUENTIAL
                    ACCESS MODE         IS DYNAMIC
                    RECORD KEY          IS C-CODE
                    FILE STATUS         IS WS-COUNTRYS-STATUS
           .
           SELECT   COVID19D-FILE       ASSIGN COVID19D
                    ORGANIZATION        IS INDEXED
                    ACCESS MODE         IS RANDOM
                    RECORD KEY          IS CV-CODE-DATE
                    FILE STATUS         IS WS-COVID19D-STATUS
           .
      *=============================
       DATA DIVISION.
      *=============================
       FILE SECTION.
      *-----------------------------
      *FD  COVID19CSV-FILE
      *    RECORDING MODE IS U
      *    RECORD IS VARYING FROM 4 TO 260 CHARACTERS
      *     .
       FD  COVID19CSV-FILE
           RECORDING MODE IS V
           RECORD IS VARYING FROM 4 TO 256 CHARACTERS
           .
       01  COVID19CSV-REC-A        PIC X(4).
       01  COVID19CSV-REC-B        PIC X(195).
      *
       FD  COUNTRYS-FILE.
      *    RECORDING MODE IS F. *> DOES NOT APPLY FOR NONPHYSICAL SEQ
       COPY COUNTRYS REPLACING     ==:TAG1:== BY ==COUNTRYS==
                                   ==:TAG2:== BY ==C==.
      *
       FD  COVID19D-FILE.
       COPY COVID19D REPLACING     ==:TAG1:== BY ==COVID19D==
                                   ==:TAG2:== BY ==CV==.
      *-----------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------
       01  WS-FILES-STATUS.
           03  WS-COV19CSV-STATUS  PIC 99 VALUE ZEROS.
           03  WS-COUNTRYS-STATUS  PIC 99 VALUE ZEROS.
           03  WS-COVID19D-STATUS  PIC 99 VALUE ZEROS.
      *
       01  WS-FLAGS.
           03 WS-OPTION            PIC X VALUE 'U'.
      *    N=INITIALIZE FILES, U=UPDATE FILES ***
            88 WS-OPTION-NEW       VALUE 'N'.
            88 WS-OPTION-UPDATE    VALUE 'U'.
           03 WS-END-OF-FILE       PIC X VALUE 'N'.
            88 WS-END-OF-FILE-OK   VALUE 'Y'.
           03 WS-FILE-ERROR        PIC X VALUE 'N'.
            88 WS-FILE-ERROR-FOUND VALUE 'Y'.
      *
       01  WS-COUNTERS.
           03 WS-COVID19CSV-READ   PIC 9(05) VALUE ZEROS.
           03 WS-COUNTRYS-INSERTED PIC 9(05) VALUE ZEROS.
           03 WS-COUNTRYS-UPDATED  PIC 9(05) VALUE ZEROS.
           03 WS-COVID19D-INSERTED PIC 9(05) VALUE ZEROS.
           03 WS-COVID19D-UPDATED  PIC 9(05) VALUE ZEROS.
       01  WS-UNSTRING-DATA.
           03 WS-CTL-1             PIC 9(3) VALUE ZEROES.
           03 WS-CTL-2             PIC 9(3) VALUE ZEROES.
           03 WS-CTL-3             PIC 9(3) VALUE ZEROES.
           03 WS-CTL-4             PIC 9(3) VALUE ZEROES.
           03 WS-CTL-5             PIC 9(3) VALUE ZEROES.
           03 WS-CTL-6             PIC 9(3) VALUE ZEROES.
           03 WS-CTL-7             PIC 9(3) VALUE ZEROES.
           03 WS-CTL-8             PIC 9(3) VALUE ZEROES.
           03 WS-CTL-9             PIC 9(3) VALUE ZEROES.
           03 WS-CTL-10            PIC 9(3) VALUE ZEROES.
           03 WS-CTL-11            PIC 9(3) VALUE ZEROES.
           03 WS-DELIMITER-1       PIC X    VALUE SPACES.
           03 WS-DELIMITER-2       PIC X    VALUE SPACES.
           03 WS-DELIMITER-3       PIC X    VALUE SPACES.
           03 WS-DELIMITER-4       PIC X    VALUE SPACES.
           03 WS-POINTER-1         PIC 9(3) VALUE ZEROES.
           03 WS-POINTER-2         PIC 9(3) VALUE ZEROES.
           03 WS-TALLYING-FIELDS-1 PIC 9(3) VALUE ZEROES.
           03 WS-TALLYING-FIELDS-2 PIC 9(3) VALUE ZEROES.
      *
       01  WS-REC-OUT.
           03 WS-FIRST-QUOTE       PIC X(20) VALUE SPACES.
           03 WS-COUNTRY           PIC X(50) VALUE SPACES.
           03 WS-FIRST-COMMA       PIC X(20) VALUE SPACES.
           03 WS-COUNTRY-CODE-QUOTED.
              05 FILLER            PIC X       VALUE SPACE.
              05 WS-COUNTRY-CODE   PIC X(5)    VALUE SPACES.
           03 WS-SLUG-QUOTED.
              05 FILLER            PIC X       VALUE SPACE.
              05 WS-SLUG           PIC X(50)   VALUE SPACES.
           03 WS-NEW-CONFIRMED     PIC 9(6)    VALUE ZEROES.
           03 WS-TOTAL-CONFIRMED   PIC 9(6)    VALUE ZEROES.
           03 WS-NEW-DEATHS        PIC 9(6)    VALUE ZEROES.
           03 WS-TOTAL-DEATHS      PIC 9(6)    VALUE ZEROES.
           03 WS-NEW-RECOVERED     PIC 9(6)    VALUE ZEROES.
           03 WS-TOTAL-RECOVERED   PIC 9(6)    VALUE ZEROES.
           03 WS-DATE-TIME-QUOTED.
              05 FILLER            PIC X       VALUE SPACE.
              05 WS-DATE-TIME           PIC X(21)   VALUE SPACES.
              05 WS-DATE-TIME-R REDEFINES WS-DATE-TIME.
                 07 WS-DATE.
                    09 WS-DATE-YEAR   PIC X(04).
                    09 FILLER         PIC X.
                    09 WS-DATE-MONTH  PIC X(02).
                    09 FILLER         PIC X.
                    09 WS-DATE-DAY    PIC X(02).
                 07 WS-TIME.
                    09 FILLER         PIC X.
                    09 WS-TIME-HH     PIC X(02).
                    09 FILLER         PIC X.
                    09 WS-TIME-MM     PIC X(02).
                    09 FILLER         PIC X.
                    09 WS-TIME-SS     PIC X(02).
                 07 FILLER         PIC X.   *>Time Zone? ==> Z
                 07 FILLER         PIC X.   *>This is the closing quote
      
      *=============================
       PROCEDURE DIVISION.
      *=============================
       MAIN.
           ACCEPT WS-OPTION
           IF WS-OPTION-NEW OR WS-OPTION-UPDATE
             CONTINUE
             DISPLAY '*** WS-OPTION: ' WS-OPTION
           ELSE
             DISPLAY '*** ERROR WS-OPTION is Missing or not Valid'
             EXIT PARAGRAPH
           END-IF
           PERFORM OPEN-FILES
           IF WS-FILE-ERROR-FOUND
             EXIT PARAGRAPH
           END-IF
           PERFORM READ-FILE
           PERFORM CLOSE-FILES
           .
      *
       MAIN-STOP.
           GOBACK
           .
      *
       OPEN-FILES.
           OPEN INPUT    COVID19CSV-FILE
           EVALUATE TRUE
             WHEN WS-OPTION-NEW
               OPEN OUTPUT COUNTRYS-FILE
               OPEN OUTPUT COVID19D-FILE
             WHEN WS-OPTION-UPDATE
      *      OPEN I-O    COUNTRYS-FILE
             OPEN OUTPUT COUNTRYS-FILE
             OPEN I-O    COVID19D-FILE
           END-EVALUATE
      *
           IF WS-COUNTRYS-STATUS > 0
              DISPLAY '*** ERROR OPENING COUNTRYS-FILE-STATUS: '
                          WS-COUNTRYS-STATUS
              MOVE  'Y'            TO WS-FILE-ERROR
              EXIT PARAGRAPH
           END-IF
      *
           IF WS-COVID19D-STATUS > 0
              DISPLAY '*** ERROR OPENING COVID19D-FILE-STATUS: '
                         WS-COVID19D-STATUS
              MOVE  'Y'            TO WS-FILE-ERROR
              EXIT PARAGRAPH
           END-IF
           .
      *
       READ-FILE.
           PERFORM READ-NEXT
           PERFORM UNTIL WS-END-OF-FILE-OK
             PERFORM GET-INPUT-DATA
             PERFORM WRITE-FILES
             PERFORM READ-NEXT
           END-PERFORM
      
           PERFORM WRITE-TRAILERS
           .
      *
       CLOSE-FILES.
           CLOSE COVID19CSV-FILE
                 COUNTRYS-FILE
                 COVID19D-FILE
           .
      *
       READ-NEXT.
           READ COVID19CSV-FILE
              AT END MOVE 'Y'      TO WS-END-OF-FILE
              NOT AT END ADD 1     TO WS-COVID19CSV-READ
           END-READ
           .
      *
       GET-INPUT-DATA.
      *I need to different UNSTRING statements because Country can
      * include a comma, like "Iran, Islamic Republic of"
      *Using WS-POINTER-1, I am trying to skip the first quote ...
           MOVE 2                  TO WS-POINTER-1
           UNSTRING COVID19CSV-REC-B
               DELIMITED BY '"'
               INTO WS-COUNTRY
                         DELIMITER IN WS-DELIMITER-2 COUNT IN WS-CTL-2
                    WITH POINTER WS-POINTER-1
                    TALLYING WS-TALLYING-FIELDS-1
           END-UNSTRING
      *
           COMPUTE WS-POINTER-2 = WS-CTL-2 + 4
           UNSTRING COVID19CSV-REC-B
               DELIMITED BY ','
               INTO
      *             WS-FIRST-COMMA
      *                  DELIMITER IN WS-DELIMITER-3 COUNT IN WS-CTL-3
                    WS-COUNTRY-CODE-QUOTED
                         DELIMITER IN WS-DELIMITER-4 COUNT IN WS-CTL-4
                    WS-SLUG-QUOTED
                    WS-NEW-CONFIRMED
                    WS-TOTAL-CONFIRMED
                    WS-NEW-DEATHS
                    WS-TOTAL-DEATHS
                    WS-NEW-RECOVERED
                    WS-TOTAL-RECOVERED
                    WS-DATE-TIME-QUOTED
                    WITH POINTER WS-POINTER-2
                    TALLYING WS-TALLYING-FIELDS-2
           END-UNSTRING
      *
           INSPECT WS-COUNTRY-CODE
              REPLACING ALL QUOTES BY SPACE
           INSPECT WS-SLUG
             REPLACING ALL QUOTES BY SPACE
           INSPECT WS-DATE-TIME
              REPLACING ALL QUOTES BY SPACE
      *
           IF WS-SLUG = 'iran'
      *       DISPLAY '*** WS-COUNTRY: '   WS-COUNTRY
              DISPLAY '*** WS-DEL-1: '     WS-DELIMITER-1
              DISPLAY '*** WS-CTL-1: '     WS-CTL-1
              DISPLAY '*** WS-DEL-2: '     WS-DELIMITER-2
              DISPLAY '*** WS-CTL-2: '     WS-CTL-2
              DISPLAY '*** WS-DEL-3: '     WS-DELIMITER-3
              DISPLAY '*** WS-CTL-3: '     WS-CTL-3
              DISPLAY '*** WS-DEL-4: '     WS-DELIMITER-4
              DISPLAY '*** WS-CTL-4: '     WS-CTL-4
              DISPLAY '*** WS-POINTER-1: ' WS-POINTER-1
           END-IF
           .
      *
       WRITE-FILES.
           PERFORM BUILD-OUTPUT-RECORDS
           EVALUATE TRUE
             WHEN WS-OPTION-NEW
               PERFORM WRITE-COUNTRYS-REC
               PERFORM WRITE-COVID19D-REC
             WHEN WS-OPTION-UPDATE
               PERFORM WRITE-COUNTRYS-REC
               PERFORM UPDATE-COVID19D-REC
           .
      *
       BUILD-OUTPUT-RECORDS.
      *    *** COUNTRYS ***
           MOVE WS-COUNTRY-CODE    TO C-CODE
           MOVE WS-COUNTRY         TO C-NAME
           MOVE WS-SLUG            TO C-SLUG
      *    *** COVID19D ***
           MOVE WS-COUNTRY-CODE    TO CV-CODE
           MOVE WS-DATE-YEAR       TO CV-DATE-YEAR
           MOVE WS-DATE-MONTH      TO CV-DATE-MONTH
           MOVE WS-DATE-DAY        TO CV-DATE-DAY
           MOVE WS-NEW-CONFIRMED   TO CV-NEW-CONFIRMED
           MOVE WS-TOTAL-CONFIRMED TO CV-TOTAL-CONFIRMED
           MOVE WS-NEW-DEATHS      TO CV-NEW-DEATHS
           MOVE WS-TOTAL-DEATHS    TO CV-TOTAL-DEATHS
           MOVE WS-NEW-RECOVERED   TO CV-NEW-RECOVERED
           MOVE WS-TOTAL-RECOVERED TO CV-TOTAL-RECOVERED
           .
      *
       WRITE-COUNTRYS-REC.
           DISPLAY 'Rec: ' COUNTRYS-REC
           WRITE COUNTRYS-REC
           IF WS-COUNTRYS-STATUS > 0
                DISPLAY '*** Error writing COUNTRYS-FILE: '
                WS-COUNTRYS-STATUS
           ELSE
               ADD 1               TO WS-COUNTRYS-INSERTED
           END-IF
           .
      *
       WRITE-COVID19D-REC.
           WRITE COVID19D-REC
           IF WS-COVID19D-STATUS > 0
              DISPLAY '*** Error WRITE COVID19D: '
                     WS-COVID19D-STATUS
           ELSE
              ADD 1                TO WS-COVID19D-INSERTED
           END-IF
           .
      *
       UPDATE-COVID19D-REC.
           READ COVID19D-FILE
           DISPLAY '*** Reading COVID19D: '
                         WS-COVID19D-STATUS
      *
           EVALUATE TRUE
             WHEN WS-COVID19D-STATUS = 00
               REWRITE COVID19D-REC
               IF WS-COVID19D-STATUS > 0
                    DISPLAY '*** Error REWRITE file COVID19D: '
                         WS-COVID19D-STATUS
               ELSE
                 ADD 1             TO WS-COVID19D-UPDATED
               END-IF
      
             WHEN OTHER
               WRITE COVID19D-REC
               IF WS-COVID19D-STATUS > 0
                    DISPLAY '*** Error WRITE COVID19D: '
                         WS-COVID19D-STATUS
               ELSE
                 ADD 1             TO WS-COVID19D-INSERTED
               END-IF
           END-EVALUATE
           .
      *
       WRITE-TRAILERS.
           DISPLAY 'COVID19CSV records READ: '    WS-COVID19CSV-READ
           DISPLAY 'COUNTRYS records INSERTED: '  WS-COUNTRYS-INSERTED
           DISPLAY 'COUNTRYS records UPDATED: '   WS-COUNTRYS-UPDATED
           DISPLAY 'COVID19D records INSERTED: '  WS-COVID19D-INSERTED
           DISPLAY 'COVID19D records UPDATED: '   WS-COVID19D-UPDATED
           .
