//COV1902J JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSLIB DD DSN=&SYSUID..COBOL.CPY,DISP=SHR
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(COV1902),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(COV1902),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=COV1902
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//**//ACCTREC   DD DSN=&SYSUID..DATA.COPY,DISP=SHR
//COUNTRYS  DD DSN=&SYSUID..COUNTRYS,DISP=SHR
//PRTLINE   DD SYSOUT=*,OUTLIM=15000
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF