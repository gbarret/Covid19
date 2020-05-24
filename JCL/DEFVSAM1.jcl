//*** Create VSAM file Countrys used in lab Covid19 ***
//DEFVSAM   JOB 1,NOTIFY=&SYSUID
//S1   EXEC PGM=IDCAMS
//SYSPRINT  DD SYSOUT=*
//SYSIN     DD *,SYMBOLS=CNVTSYS
  DELETE &SYSUID..COUNTRYS
  SET MAXCC=0
  DEFINE CLUSTER ( NAME ( &SYSUID..COUNTRYS ) -
            VOLUME(VPWRKB) -
            TRACKS(15) -
            RECORDSIZE(102,102) -
            INDEXED KEYS(2 0) -
            REUSE -
            SHAREOPTIONS(2) -
            SPANNED -
            SPEED -
            CONTROLINTERVALSIZE(4096) )
