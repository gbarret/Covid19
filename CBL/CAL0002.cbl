      *=========================
       IDENTIFICATION DIVISION.
      *=========================
       PROGRAM-ID.        CAL0002.
      *Calling a program by reference ...
      *=========================
       ENVIRONMENT DIVISION.
      *=========================

      *=========================
       DATA DIVISION.
      *=========================
       WORKING-STORAGE SECTION.
      *-------------------------
       77  WS-SUB-PGM              PIC X(08) VALUE 'SUB0002'.
       77  WS-C-CODE-TO-READ       PIC X(02).
      *
       01  WS-RESULT-INFO.
           03 WS-RESULT            PIC X.
              88 WS-RESULT-OK      VALUE 'Y'.
              88 WS-RESULT-NO-OK   VALUE 'N'.
           03 WS-RESULT-CODE       PIC XX.
           03 WS-RESULT-MESSAGE    PIC X(30).
      *
           COPY COUNTRYS REPLACING ==:TAG1:== BY ==WS-COUNTRYS==
                                   ==:TAG2:== BY ==WS-C==.
      * 01  WS-COUNTRYS-REC.
      *     03 WS-C-CODE            PIC X(02).
      *     03 WS-C-NAME            PIC X(50).
      *     03 WS-C-SLUG            PIC X(50).
      *=========================
       PROCEDURE DIVISION.
      *=========================
      *     MOVE "19331945"     TO WS-C-CODE
           ACCEPT WS-C-CODE-TO-READ    *> Comment
      *    Get info about Roosevelt ...
           MOVE SPACES TO WS-COUNTRYS-REC WS-RESULT-INFO
           CALL WS-SUB-PGM
              USING WS-C-CODE-TO-READ, WS-COUNTRYS-REC, WS-RESULT-INFO.
           IF WS-RESULT-OK
              DISPLAY 'Country Code: ' WS-C-CODE
              DISPLAY 'Country Name: ' WS-C-NAME
              DISPLAY 'Country Slug: ' WS-C-SLUG
           ELSE
              DISPLAY '*** Error: ' WS-RESULT-MESSAGE
                       'Code: ' WS-RESULT-CODE
           END-IF
           GOBACK
           .
      *