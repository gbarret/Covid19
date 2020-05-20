//COV1901J    JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN        EXEC IGYWCL
//COBOL.SYSLIB  DD DSN=&SYSUID..COBOL.CPY,DISP=SHR
//COBOL.SYSIN   DD DSN=&SYSUID..CBL(COV1901),DISP=SHR
//LKED.SYSLMOD  DD DSN=&SYSUID..LOAD(COV1901),DISP=SHR
//***************************************************/
//  IF RC = 0 THEN
//***************************************************/
//RUN           EXEC PGM=COV1901
//STEPLIB       DD DSN=&SYSUID..LOAD,DISP=SHR
//COV19CSV      DD DSN=&SYSUID..PS(COVID19A),DISP=SHR
//COUNTRYS      DD DSN=&SYSUID..COUNTRYS,DISP=SHR
//COVID19D      DD DSN=&SYSUID..COVID19D,DISP=SHR
//SYSOUT        DD SYSOUT=*,OUTLIM=1500
//CEEDUMP       DD DUMMY
//SYSUDUMP      DD DUMMY
//***************************************************/
//*** Parameters: N=NEW, U=UPDATE *******************/
N
/*
//  ELSE
//  ENDIF
