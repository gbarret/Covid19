      **** COVID19D FILE ***
      **** CREATED BY GBS ***
      **** INITIALLY USED BY PGRM COV1901.CBL ***
      **** EXAMPLE:
      *COPY COVI19D REPLACING ==:TAG1:== BY COVID19D
      *                       ==:TAG2:== BY COV.
       01  :TAG1:-REC.
           03 :TAG2:-CODE-DATE.
             05 :TAG2:-CODE            PIC X(02).
             05 :TAG2:-DATE.
               07 :TAG2:-DATE-YEAR     PIC 9(04).
               07 :TAG2:-DATE-MONTH    PIC 9(02).
               07 :TAG2:-DATE-DAY      PIC 9(02).
           03 :TAG2:-NEW-CONFIRMED     PIC 9(09) COMP-3.
           03 :TAG2:-TOTAL-CONFIRMED   PIC 9(09) COMP-3.
           03 :TAG2:-NEW-DEATHS        PIC 9(09) COMP-3.
           03 :TAG2:-TOTAL-DEATHS      PIC 9(09) COMP-3.
           03 :TAG2:-NEW-RECOVERED     PIC 9(09) COMP-3.
           03 :TAG2:-TOTAL-RECOVERED   PIC 9(09) COMP-3.
