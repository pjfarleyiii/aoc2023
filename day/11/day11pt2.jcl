//AOC7PFB2 JOB CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),                       JOB08036
//         NOTIFY=&SYSUID,REGION=0M  ,RESTART=RUN
/*JOBPARM LINES=00009
//*--------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=(IGY.V6R4M0.SIGYPROC)
// SET PROGRAM=DAY11PT1    <=== YOUR PROGRAM NAME
//*
//CL       EXEC IGYWCL,
//             PARM.COBOL='NOSEQ,MAP,DS(S)',
//             GOPGM=&PROGRAM,PGMLIB=&SYSUID..AOC23.LOAD
//COBOL.SYSIN  DD DISP=SHR,DSN=&SYSUID..AOC23.SRC(&PROGRAM)
//LKED.SYSLMOD DD UNIT=,SPACE=
//*
//RUN      EXEC PGM=&PROGRAM,PARM=2,
//             TIME=(,10),    LIMIT 10 SEC CPU FOR TESTING
//             COND=((4,LT,CL.COBOL),(4,LT,CL.LKED))
//STEPLIB  DD  DISP=SHR,DSN=&SYSUID..AOC23.LOAD
//SYSOUT   DD  SYSOUT=*,RECFM=VB,LRECL=196
//AOCINPUT DD  PATH='/home/&SYSUID/day/11/intest1', input',  intest1',
//             PATHDISP=(KEEP,KEEP),
//             PATHOPTS=(ORDONLY),
//             RECFM=V,LRECL=32760
//CEEOPTS  DD  *  THIS DD TURNS ON COBOL DEBUGGING OUTPUT
DEBUG
