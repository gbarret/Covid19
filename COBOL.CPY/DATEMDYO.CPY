      *EDITED DATE FORMAT MM/DD/YYYY
      *USING THIS COPY, EXAMPLE:   TAG ==> PREFIX,
      *                            X ==> DATE SEPARATOR LIKE / OR -
      * COPY DATEMDYO REPLACING    ==:TAG:==      BY ==WS-XXX==
      *                            ==:TAG1:==     BY =='/'==.
       01  :TAG:-DATE-TIME-O.
           03 :TAG:-DATE-O.
             05 :TAG:-MONTH-O     PIC 9(02).
             05 FILLER             PIC X(01) VALUE :TAG1:.
             05 :TAG:-DAY-O       PIC 9(02).
             05 FILLER             PIC X(01) VALUE :TAG1:.
             05 :TAG:-YEAR-O      PIC 9(04).
           03 :TAG:-TIME-O.
             05 :TAG:-HOURS-O     PIC 9(02).
             05 FILLER             PIC X(01) VALUE ':'.
             05 :TAG:-MINUTES-O   PIC 9(02).
             05 FILLER             PIC X(01) VALUE ':'.
             05 :TAG:-SECONDS-O   PIC 9(02).
