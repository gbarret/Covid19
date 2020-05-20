      **** COUNTRYS FILE ***
      **** CREATED BY GBS ***
      **** INITIALLY USED BY PGRM COV1901.CBL ***
      **** EXAMPLE:
      *COPY COUNTRYS REPLACING     ==:TAG1:== BY COUNTRYS
      *                            ==:TAG2:== BY CV.
       01  :TAG1:-REC.
           03 :TAG2:-CODE               PIC X(02).
           03 :TAG2:-NAME               PIC X(50).
           03 :TAG2:-SLUG               PIC X(50).
