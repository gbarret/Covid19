      *EDITED DATE FORMAT MM/DD/YYYY
      *USING THIS COPY, EXAMPLE:   RESULT ==> PREFIX
      * COPY CALLSTUS REPLACING    ==:RESULT:==      BY ==XXX==.
       01  WS-:RESULT:-INFO.
           03 WS-:RESULT:            PIC X.
              88 WS-:RESULT:-OK      VALUE 'Y'.
              88 WS-:RESULT:-NO-OK   VALUE 'N'.
           03 WS-:RESULT:-CODE       PIC XX.
           03 WS-:RESULT:-MESSAGE    PIC X(30).