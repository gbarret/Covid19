//*** Create VSAM file COVID19D used in lab Covid19 ***
//DEFVSAM      JOB 1,NOTIFY=&SYSUID
//S1           EXEC PGM=IDCAMS
//SYSPRINT     DD SYSOUT=*
//SYSIN        DD *,SYMBOLS=CNVTSYS
  DELETE &SYSUID..COVID19D
  SET MAXCC=0
  DEFINE CLUSTER ( NAME ( &SYSUID..COVID19D ) -
            VOLUME(VPWRKB) -
            TRACKS(15) -
            RECORDSIZE(40,40) -
            INDEXED KEYS(10 0) -
            REUSE -
            SHAREOPTIONS(2) -
            SPANNED -
            SPEED -
            CONTROLINTERVALSIZE(4096) )
