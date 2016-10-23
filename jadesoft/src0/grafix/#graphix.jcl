//* 20/12/85 810171939  MEMBER NAME  #GRAPHIX (S)           JCL
//         JOB 00010622,CLASS=E,TIME=(0,15),MSGLEVEL=(2,0)
//*MAIN RELPRI=LOW
//*
//**********************************************************************
//*                                                                    *
//*    GRAPHICS LINKING JOB WITH NO OVERLAYS                           *
//*    GRAPHICS LINKING JOB WITH NO OVERLAYS       -------             *
//*    GRAPHICS LINKING JOB WITH NO OVERLAYS       J A D E             *
//*    GRAPHICS LINKING JOB WITH NO OVERLAYS       -------             *
//*    GRAPHICS LINKING JOB WITH NO OVERLAYS                           *
//*                                                                    *
//*                                                                    *
//**********************************************************************
//*                                                                    *
//*    USES CERN STYLE JCL PROCEDURE   L I N K                         *
//*                                                                    *
//*    NOTE: UNIT=FAST  M U S T  BE GIVEN IN THE DEFINITION OF         *
//*          THE DDNAME SYSLMOD. OTHERWISE UNIT=DISK IS ASSUMED.       *
//*                                                                    *
//**********************************************************************
//*
//NEWLIB EXEC NEWLIB,PS=NULLFILE,
// PL='F11LHO.GRAPHL',TO=DISK
./ SCRATCH JADE0
//*
//COMPR EXEC COMPRESS,LIB='F11LHO.GRAPHL'
//*
//LINK EXEC LINK,LPRM='LIST,SIZE=(512K,72K)',LRGN=600K
//*
//L.SYSLIB  DD DSN=F11LHO.GRAFIX.L,DISP=SHR,VOL=SER=STOR05,UNIT=FAST
//          DD DSN=F22KLE.VERTEX.L,DISP=SHR,VOL=SER=STOR05,UNIT=FAST
//          DD DSN=F22KLE.JVTXC.GL,DISP=SHR,VOL=SER=STOR05,UNIT=FAST
//          DD DSN=F11LHO.JADEGL,DISP=SHR,VOL=SER=STOR05,UNIT=FAST
//          DD DSN=F11GOD.PATRECLD,DISP=SHR,VOL=SER=STOR05,UNIT=FAST
//          DD DSN=JADELG.LOAD,DISP=SHR,VOL=SER=STOR05,UNIT=FAST
//          DD DSN=JADEPR.JADELD,DISP=SHR,VOL=SER=STOR05,UNIT=FAST
//          DD DSN=F22ALL.JADEMUL,DISP=SHR,VOL=SER=STOR05,UNIT=FAST
//          DD DSN=F22NAR.TOFLIB.L,DISP=SHR,VOL=SER=STOR05,UNIT=FAST
//          DD DSN=F11LHO.TAGG.L,DISP=SHR,VOL=SER=STOR05,UNIT=FAST
//          DD DSN=F1EBLO.BOSLIB.L,DISP=SHR,VOL=SER=DISK71,UNIT=FAST
//          DD DSN=R02SCH.AG2USER.LOAD,DISP=SHR
//          DD DSN=R02SCH.TSOIPS.LOAD,DISP=SHR
//          DD DSN=R02SCH.TCSUSER.LOAD,DISP=SHR
//          DD DSN=SYS1.DESY.LHXE,DISP=SHR
//          DD DSN=SYS1.FORT.LHXE,DISP=SHR
//          DD DSN=SYS1.FORT.LF77,DISP=SHR
//          DD DSN=R01UTL.CERN.KERNLIB4,DISP=SHR
//*
//*                LOAD MODULE OUTPUT
//*
//L.SYSLMOD DD DSN=F11LHO.GRAPHL,DISP=SHR,VOL=SER=STOR05,UNIT=FAST
//*
//*L.SYSLMOD DD DSN=F22???.GRAPHL,DISP=(NEW,CATLG,DELETE),UNIT=FAST,
//*         SPACE=(TRK,(90,10,5),RLSE),DCB=R01DCB.LOAD
//*
//*                ADDITIONAL DD DEFINITIONS FOLLOW (IF ANY)
//*
//*
//*                LINKAGE EDITOR INPUT COMMANDS
//*
//L.SYSIN   DD *
  INCLUDE SYSLIB(GPHMAIN,MUINIG)
  ENTRY MAIN
  NAME JADE(R)
/*
