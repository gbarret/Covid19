      *=========================
       IDENTIFICATION DIVISION.
      *=========================
       PROGRAM-ID.     SUB0002.
       AUTHOR.         GEORGES BARRETO.
      *    Called program to get a single record from file COUNTRYS.
      *=========================
       ENVIRONMENT DIVISION.
      *=========================
       INPUT-OUTPUT SECTION.
      *-------------------------
       FILE-CONTROL.
           SELECT COUNTRYS-FILE  ASSIGN COUNTRYS
              ORGANIZATION IS INDEXED
              ACCESS MODE RANDOM
              RECORD KEY IS C-CODE
              FILE STATUS IS WS-COUNTRYS-FILE-STATUS
           .
      *=========================
       DATA DIVISION.
      *=========================
       FILE SECTION.
      *-------------------------
       FD  COUNTRYS-FILE.
       COPY COUNTRYS REPLACING ==:TAG1:== BY ==COUNTRYS==
                               ==:TAG2:== BY ==C==.
      *-------------------------
       WORKING-STORAGE SECTION.
      *-------------------------
       77  WS-COUNTRYS-FILE-STATUS          PIC 99 VALUE ZEROS.
           88 WS-COUNTRYS-FILE-STATUS-OK    VALUE 0.
           88 WS-COUNTRYS-FILE-STATUS-ERROR VALUE 1 THRU 99.
       01  FLAGS.
           03 WS-OPEN-STATUS       PIC 9 VALUE ZERO.
              88 WS-OPEN-ERROR     VALUE 1.
           03 WS-READ-STATUS       PIC 9 VALUE ZERO.
              88 WS-READ-OK        VALUE 0.
              88 WS-READ-ERROR     VALUE 1.
      *-------------------------
       LINKAGE SECTION.
      *-------------------------
       77  LS-C-CODE-TO-READ      PIC X(02).
      *
       01  LS-RESULT-INFO.
           03 LS-RESULT            PIC X VALUE 'Y'.
              88 LS-RESULT-OK      VALUE 'Y'.
              88 LS-RESULT-NO-OK   VALUE 'N'.
           03 LS-RESULT-CODE       PIC XX.
           03 LS-RESULT-MESSAGE    PIC X(30).
       01  LS-COUNTRYS-REC         PIC X(102).
      *=========================
       PROCEDURE DIVISION USING LS-C-CODE-TO-READ, LS-COUNTRYS-REC,
              LS-RESULT-INFO.
      *=========================
       MAIN.
           MOVE 'Y'                      TO LS-RESULT
           PERFORM OPEN-FILE
           IF WS-OPEN-ERROR
              MOVE 'N'                   TO LS-RESULT
              EXIT PARAGRAPH
           END-IF
      *
           PERFORM READ-RECORD
           IF WS-READ-ERROR
              MOVE 'N'                   TO LS-RESULT
              MOVE WS-COUNTRYS-FILE-STATUS TO LS-RESULT-CODE
              MOVE "Record not found "   TO LS-RESULT-MESSAGE
              EXIT PARAGRAPH
           END-IF
      *
           PERFORM WRITE-RECORD
           .
      *
       CLOSE-FILE.
           CLOSE COUNTRYS-FILE
           .
      *
       STOP-PROGRAM.
           GOBACK
           .
      *
       OPEN-FILE.
           MOVE 1   TO WS-OPEN-STATUS   *> Defaults to ERROR ...
           OPEN INPUT COUNTRYS-FILE
           EVALUATE TRUE
              WHEN WS-COUNTRYS-FILE-STATUS-OK
                 DISPLAY "*** WS-COUNTRYS-FILE Open was successfully! "
                 MOVE 0   TO WS-OPEN-STATUS
               WHEN WS-COUNTRYS-FILE-STATUS-ERROR
                 DISPLAY "*** Error opening WS-COUNTRYS-FILE, Status: "
                 WS-COUNTRYS-FILE-STATUS
               WHEN OTHER
                 CONTINUE
           END-EVALUATE
           .
      *
       READ-RECORD.
           MOVE LS-C-CODE-TO-READ   TO C-CODE
           DISPLAY 'LS-C-CODE-TO-READ: ' LS-C-CODE-TO-READ
           READ COUNTRYS-FILE
                 KEY IS C-CODE
                 INVALID KEY
                    DISPLAY "INVALID KEY"
                    MOVE 1   TO WS-READ-STATUS
           END-READ
           .
      *
       WRITE-RECORD.
           MOVE COUNTRYS-REC     TO LS-COUNTRYS-REC
           DISPLAY LS-COUNTRYS-REC
           .
      *