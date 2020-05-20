      *CURRENT-DATE FUNCTION THAT RETURNS A 21-CHARACTER ALPHANUMERIC
      *VALUE THAT REPRESENTS CALENDAR DATE, TIME OF DAY, AND TIME
      *DIFFERENTIAL FROM GREENWICH MEAN TIME PROVIDED BT THE SYSTMEM
      *WHERE FUNCTION IS EVALUATED.
      *USING THIS COPY EXAMPLE:
      * COPY DATETIME REPLACING ==:TAG:== BY ==WS-CURRENT==.
       01  :TAG:-DATE-DATA.
           03 :TAG:-DATE.
             05 :TAG:-YEAR         PIC 9(04).
             05 :TAG:-MONTH        PIC 9(02).
             05 :TAG:-DAY          PIC 9(02).
           03 :TAG:-TIME.
             05 :TAG:-HOURS        PIC 9(02).
             05 :TAG:-MINUTES      PIC 9(02).
             05 :TAG:-SECONDS      PIC 9(02).
             05 :TAG:-SEC-HUNDREDS  PIC 9(02).
           03 :TAG:-ZONE.
      *      IF LOCAL TIME IS "-" (BEHIND) OR "+" (SAME OR AHEAD)
      *      GREENWICH TIME OR "0" (IF SYSTEM DOES NOT PROVIDE DIFF)
             05 :TAG:-ZONE-S       PIC X.
             88 :TAG:-ZONE-S-OK    VALUES '-' '+' '0'.
      *      NUMBER OF HOURS BEHID OR AHEAD GREENWICH TIME
             05 :TAG:-ZONE-H       PIC 9(02).
             88 :TAG:-ZONE-H-NEG   VALUES 00 THRU 12.  *> IF -
             88 :TAG:-ZONE-H-POS   VALUES 00 THRU 13.  *> IF +
             88 :TAG:-ZONE-H-ZERO  VALUE 00.           *> IF 0
      *      NUMBER OF ADDITIONAL MINUTES BEHIND OR AHEAD GREENWICH TIME
             05 :TAG:-ZONE-M       PIC 9(02).
             88 :TAG:-ZONE-M-NEG   VALUES 00 THRU 59. *> IF - OR +
             88 :TAG:-ZONE-M-ZERO  VALUE 00.          *> IF 0
