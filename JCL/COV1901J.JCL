//COV1901J    JOB 1,NOTIFY=&SYSUID
//* *******************************************************************
//*                          COV1901.JCL                              *
//*                                                                   *
//*                                                                   *
//*********************************************************************
//*
//* Title      - Convert Covi19 API data from CSV to VSAM format
//* Author     - Georges Barreto
//* Date       - May 20,2020
//*
//* This program will create files with Covid19's data for future
//*              reports. It will CREATE/UPDATE 2 files from a CSV file
//* - COV19CSV: CSV file: contains data from
//*             https://api.covid19api.com/summary (JSON)
//* - COUNTRYS: Indexed VSAM file Countries info such as code & name
//* - COVID19D: Indexed VSAM file with data from Covi19's API
//*
//*                     *****************
//*                     * COV1901J.jcl  *
//*                     *****************
//*                             *
//*                             *                *************
//*    ***********              *                *************
//*   *   N / U  *=>            *            <=> *  COUNTRYS *
//*   ************  |           *           |    ********vsam*
//*                 |   *****************   |
//*                 |=> *  COV1901.cbl  *<=>|
//*                 |   *****************   |    *************
//*   ************  |           *           |    *************
//*   ************  |           *            <=> * COVID19D  *
//*   * COV19CSV *=>            *                ********vsam*
//*   ************              *
//*                             *
//*                             *
//*                     *****************
//*                     *      EOJ      *
//*                     *****************
//*
//*********************************************************************
//* STEP 1: COMPILE program ...
//COBRUN        EXEC IGYWCL
//COBOL.SYSLIB  DD DSN=&SYSUID..COBOL.CPY,DISP=SHR
//COBOL.SYSIN   DD DSN=&SYSUID..CBL(COV1901),DISP=SHR
//LKED.SYSLMOD  DD DSN=&SYSUID..LOAD(COV1901),DISP=SHR
//***************************************************/
//  IF RC = 0 THEN
//***************************************************/
//* STEP 2: RUN program ...
//RUN           EXEC PGM=COV1901
//STEPLIB       DD DSN=&SYSUID..LOAD,DISP=SHR
//COV19CSV      DD DSN=&SYSUID..PS(COVID19B),DISP=SHR
//COUNTRYS      DD DSN=&SYSUID..COUNTRYS,DISP=SHR
//COVID19D      DD DSN=&SYSUID..COVID19D,DISP=SHR
//SYSOUT        DD SYSOUT=*,OUTLIM=1500
//CEEDUMP       DD DUMMY
//SYSUDUMP      DD DUMMY
//***************************************************/
//*** Parameters: N=NEW, U=UPDATE *******************/
U
/*
//  ELSE
//  ENDIF