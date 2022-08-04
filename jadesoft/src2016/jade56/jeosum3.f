C   28/03/97 703282257  MEMBER NAME  JEOSUM3  (JADE56.S)    FORTRAN
C   28/03/97            MEMBER NAME  JBTJ20   (JADE56.S)    FORTRAN
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPRIV4.NR14
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR57
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     22401     22800    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPRIV4.NR14
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR58
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     22801     23200    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPRIV4.NR14
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR59
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     23201     23600    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPRIV4.NR14
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR60
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     23601     24600    99999
C   28/03/97            MEMBER NAME  JBTJ20D  (JADE56.S)    FORTRAN
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(21,00),CLASS=D
//*MAIN RELPRI=MED
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJ40,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPRIV4.NR12
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//*T02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*  SPACE=(TRK,(80,11),RLSE),DSN=F11OLS.ETAPRI86.NR57
//FT02F001 DD DISP=SHR,
//     DSN=F11OLS.ETAPRI.SUM2
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
         0      3200    99999
C   28/03/97            MEMBER NAME  JBTJ2016 (JADE56.S)    FORTRAN
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR2
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR9
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      3201      3600    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR2
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR10
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      3601      4000    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR2
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR11
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      4001      4400    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR2
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR12
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      4401      4800    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR2
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR13
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      4801      5200    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR2
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR14
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      5201      5600    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR2
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR15
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      5601      6000    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR2
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR16
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      6001      6400    99999
C   28/03/97            MEMBER NAME  JBTJ2024 (JADE56.S)    FORTRAN
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR17
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      6401      6800    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR18
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      6801      7200    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR19
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      7201      7600    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR20
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      7601      8000    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR21
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      8001      8400    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR22
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      8401      8800    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR23
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      8801      9200    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR24
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      9201      9600    99999
C   28/03/97            MEMBER NAME  JBTJ2032 (JADE56.S)    FORTRAN
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR25
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      9601     10000    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR26
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     10001     10400    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR27
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     10401     10800    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR28
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     10801     11200    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR29
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     11201     11600    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR30
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     11601     12000    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR31
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     12001     12400    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR32
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     12401     12800    99999
C   28/03/97            MEMBER NAME  JBTJ2040 (JADE56.S)    FORTRAN
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR33
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     12801     13200    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR34
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     13201     13600    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR35
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     13601     14000    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR36
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     14001     14400    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR37
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     14401     14800    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR38
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     14801     15200    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR39
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     15201     15600    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR40
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     15601     16000    99999
C   28/03/97            MEMBER NAME  JBTJ2048 (JADE56.S)    FORTRAN
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR41
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     16001     16400    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR42
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     16401     16800    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR43
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     16801     17200    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR44
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     17201     17600    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR45
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     17601     18000    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR46
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     18001     18400    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR47
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     18401     18800    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR48
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     18801     19200    99999
C   28/03/97            MEMBER NAME  JBTJ2056 (JADE56.S)    FORTRAN
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR49
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     19201     19600    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR50
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     19601     20000    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR51
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     20001     20400    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR52
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     20401     20800    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR53
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     20801     21200    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR54
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     21201     21600    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR55
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     21601     22000    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR56
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     22001     22400    99999
C   28/03/97            MEMBER NAME  JBTJ2064 (JADE56.S)    FORTRAN
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR57
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     22401     22900    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR58
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     22901     23400    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR59
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     23401     23900    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR60
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     23901     24400    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR61
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     24401     24900    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR62
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     24901     25400    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR63
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     25401     25900    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR1
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR64
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
     25901     27400    99999
C   28/03/97            MEMBER NAME  JBTJ208  (JADE56.S)    FORTRAN
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR2
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR1
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
         0       400    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR2
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR2
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
       401       800    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR2
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR3
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
       801      1200    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR2
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR4
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      1201      1600    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR2
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR5
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      1601      2000    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR2
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR6
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      2001      2400    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR2
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR7
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      2401      2800    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJEL,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPV4.NR2
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(40,11),RLSE),DSN=F11OLS.ETAPRI86.NR8
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
      2801      3200    99999
C   28/03/97            MEMBER NAME  JBTJ40E  (JADE56.S)    FORTRAN
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJ40,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.UNIGAM4V.E100D9
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(80,11),RLSE),DSN=F11OLS.UNIGAM86.E100D9
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
         0      2000    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJ40,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.UNIGAM4V.E100DA
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(80,11),RLSE),DSN=F11OLS.UNIGAM86.E100DA
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
         0      2000    99999
//F11JLL97 JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(3,00),CLASS=K
//*MAIN RELPRI=LOW
//JOBLIB DD DSN=F11OLS.LKLDMC,DISP=SHR
//*
//**********************************************************************
//*                                                                    *
//*         M C J A D E   TRACKING JOB  (WITHOUT MUON TRACKING)        *
//*                                                                    *
//**********************************************************************
//*
//TRACKING EXEC PGM=LMDTJ40,REGION=840K
//*
//*                 INPUT
//FT03F001 DD DISP=SHR,DSN=F11OLS.UNIGAM4V.E100DB
//*
//*                 OUTPUT
//*FT02F001 DD DUMMY,DCB=R01DCB.VBS
//FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//   SPACE=(TRK,(80,11),RLSE),DSN=F11OLS.UNIGAM86.E100DB
//*GO.FT02F001 DD DISP=SHR,
//*    DSN=F11OLS.ETPRF.S1
//*GO.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.ETPRF.S1,
//*       MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*        DCB=R01DCB.VBS,
//*        DSN=F11OLS.TRACK1.FTAPRJ.B15
//FT06F001 DD SYSOUT=*
//FT05F001 DD *
    IEVMIN    IEVMAX
         0      2000    99999
C   28/03/97            MEMBER NAME  JBUNIGAM (JADE56.S)    FORTRAN
//F11OLS   JOB CLASS=K,MSGLEVEL=(1,0),TIME=(3,00)
//*MAIN RELPRI=LOW
// EXEC NEWLIB,PS='F11OLS.JADESR',PL='F11OLS.JADELD',
//  UNIT=FAST,LEVEL=8,OPT=3,FORT=XL
//MACRO DD DSN=F11GOD.PATRECSR,DISP=SHR,UNIT=FAST
./ C USER0
./ I 10200
C END OF JOB, AFTER PRINT OF IGGS,JIGGS AND BEFORE PLOTS
./ I 7800
C LEVEL 7
      IPO = IDATA(IBLN('PATR'))
      IPALGN = IDATA(IBLN('ALGN'))
      IPLGCL = IDATA(IBLN('LGCL'))
      IPVECT = IDATA(IBLN('VECT'))
      IF(IPALGN.LE.0.OR.IPLGCL.LE.0) GO TO 105
      NCLST = IDATA(IPLGCL+7)
      NWPCL = IDATA(IPLGCL+25)
      JUMPCL = IDATA(IPLGCL+2)
C  4-VECTOR PHOTON ENERGY
      LO = IDATA(IPVECT+1)
      LTR = IDATA(IPVECT+2)
      NTRVEC = IDATA(IPVECT+4)
      IP4 = IPVECT + LO
      PTRUE = ADATA(IP4+4)
      GO TO 999
105   WRITE(6,101) IPALGN,IPLGCL
101   FORMAT(' **** WARNING NO ALGN/LGCL BANKS,  POINTERS :',2I8)
      GO TO 1
999   NMOMGM = 0
      IF(NCLST.LE.0) GO TO 1027
      IP = IDATA(IPLGCL+3) + IPLGCL - 1 - NWPCL
      ITP = 0
      DO 522  IGM = 1,NCLST
      IP = IP + NWPCL
      CALL TRACK(IP,ITP,RMIN,NHT,FI,THE,PTOT,PT,PL,PX,PY,PZ)
      IF(IDATA(IP+8).NE.0) GO TO 522
      NMOMGM = NMOMGM + 1
      PTOTGM = PTOT
      PSEEN = ADATA(IP+NWPCL)
      COSTHG = COS(THE)
522   CONTINUE
1027  CONTINUE
      IF(NMOMGM.NE.1) GO TO 1
C
C ONE PHOTON FOUND, NOW COMPARE ENERGY
C
      IF(IPRIN.EQ.0) WRITE(6,4010) PTRUE
4010  FORMAT('      TRUE PHOTON ENERGY: ',E12.4)
      IF(IPRIN.EQ.0) IPRIN = 1
C
      RATIO1 = PTOTGM / PSEEN
      RATIO2 = PTOTGM / PTRUE
      RATIO3 = PSEEN / PTRUE
      CALL HFILL(1,RATIO1,0,1.)
      CALL HFILL(2,RATIO2,0,1.)
      CALL HFILL(3,RATIO3,0,1.)
      CALL HFILL(4,PSEEN,0,1.)
      CALL HFILL(5,PTOTGM,0,1.)
      CALL HFILL(6,PSEEN,0,1.)
      CALL HFILL(7,PTOTGM,0,1.)
      CALL HFILL(8,PSEEN,0,1.)
      CALL HFILL(9,PTOTGM,0,1.)
      CALL HFILL(21,COSTHG,0,1.)
      GO TO 11
./ I 7400
C LEVEL 6
./ I 6800
C LEVEL 5
./ I 6200
C LEVEL 4
./ I 5600
C LEVEL 3
./ I 5000
C LEVEL 2
      IF((JIGG(2)/100)*100.EQ.JIGG(2))
     $ WRITE(6,2702) JIGG(2),HEAD(18),HEAD(19)
2702  FORMAT(' NNREC RUN EVENT ',3I8)
./ I 4400
C LEVEL 1
./ I 3800
C LEVEL 0
C  ITH,IRLTHD,IRLTH2,IRLTH3 ARE CLUSTER THRESHOLDS
C   IPHALG STEARS THE READOUT THRESHOLD IN MC EVENTS, DEFAULT NONZERO
      ITH = 25
C     IRLTHD = 1
C     IRLTH2 = 1
C     IRLTH3 = 1
      IPHALG = 1
      IPRIN = 0
      WRITE(6,1221) ITH,IPHALG
1221  FORMAT('  CLUSTER THRESHOLD, READOUT FLAG ',2I5)
C
      CALL HBOOK1(1,' CORRECTED ENERGY/SEEN ENERGY$',100,0.,3.)
      CALL HBOOK1(2,' CORRECTED ENERGY/TRUE ENERGY$',100,0.,3.)
      CALL HBOOK1(3,' SEEN ENERGY/TRUE ENERGY$',100,0.,3.)
      CALL HBOOK1(4,' SEEN ENERGY$',100,0.,0.5)
      CALL HBOOK1(5,'CORRECTED ENERGY$',100,0.,0.5)
      CALL HBOOK1(6,' SEEN ENERGY$',100,0.,1.0)
      CALL HBOOK1(7,'CORRECTED ENERGY$',100,0.,1.0)
      CALL HBOOK1(8,' SEEN ENERGY$',100,0.,1.5)
      CALL HBOOK1(9,'CORRECTED ENERGY$',100,0.,1.5)
      CALL HBOOK1(21,' COS THETA$',100,0.,1.)
1223  CONTINUE
./ I 1300
C DECLARATIVE LEVEL
      COMMON /CLGPRM/ ITH,MAXCLS,IRLTHD,IRLTH2,IRLTH3
      COMMON / CRDSTA / IMCDUM(12),IPHALG
// EXEC FLG,PARM.LKED='SIZE=(600K,60K)',REGION.LKED=800K,
//  REGION.GO=1600K
//LKED.SYSLIB DD
//            DD
//            DD DISP=SHR,UNIT=,DSN=&&LOAD
//            DD DISP=SHR,UNIT=FAST,DSN=F11GOD.PATRECLD
//            DD DISP=SHR,UNIT=FAST,DSN=F11LHO.JADEGL
//            DD DISP=SHR,UNIT=FAST,DSN=JADELG.LOAD
//            DD DISP=SHR,UNIT=FAST,DSN=F22ALL.JADEMUL
//            DD DISP=SHR,UNIT=FAST,DSN=R01UTL.HBOOK321.L
//            DD DISP=SHR,UNIT=FAST,DSN=R01UTL.CERN.KERNLIB4
//            DD DISP=SHR,UNIT=FAST,DSN=F1EBLO.BOSLIB.L
//LKED.SYSIN DD *
 INCLUDE SYSLIB(JDMAIN0)
 INCLUDE SYSLIB(USER0)
//GO.FT22F001 DD UNIT=FAST,DISP=SHR,DSN=F11LHO.AUPDAT1
//*                        INPUT:
//GO.FT02F001 DD UNIT=TAPE,DISP=OLD,DSN=F11OLS.UNIGAM.E400D3.SF5
//*  DD UNIT=AFF=FT02F001,DISP=OLD,DSN=F11LHO.JDATA08.REFORM.G0197V00
//*GO.FT02F001 DD DISP=SHR,DSN=F11OLS.LND52.UNTRACK
//*GO.FT02F001 DD UNIT=TAPE,DISP=OLD,LABEL=(1,NL),
//*   DCB=(R01DCB.VBS,LRECL=6229,BLKSIZE=6233,DEN=4),
//*   DSN=F22PET.NZNE.TPMH1,VOL=SER=F11106
//*                        OUTPUT:
//*GO.FT03F001 DD UNIT=DISK,DISP=(,PASS),
//*   DCB=(R01DCB.VBS,LRECL=6236),
//*       SPACE=(CYL,(220,20)),DSN=&&SCRTCH
//*GO.FT03F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.TIMEOUT1,
//*         MSVGP=VTMP,UNIT=3330V
//GO.FT03F001 DD DUMMY,DCB=R01DCB.VBS
//*GO.FT03F001 DD UNIT=FAST,DISP=SHR,DSN=F11OLS.RAETEST
//*GO.FT03F001 DD UNIT=FAST,DISP=(NEW,CATLG,DELETE),DCB=R01DCB.VBS,
//*   SPACE=(TRK,(10,2),RLSE),DSN=F11OLS.BARRIES.PROBLEM
//*GO.FT03F001 DD UNIT=TAPE,DISP=(NEW,CATLG,DELETE),
//*    DCB=(R01DCB.VBS,LRECL=6236,BLKSIZE=24948),
//*   DSN=F11OLS.REDUCTWO.G0216V00
//*
//*  COPY DISK OUTPUT TO TAPE
//*
//*COPY EXEC PGM=IEBGENER
//*SYSUT1 DD UNIT=DISK,DISP=(OLD,DELETE),DSN=&&SCRTCH
//*SYSUT2 DD UNIT=TAPE,DISP=(NEW,CATLG,DELETE),
//*          DCB=(R01DCB.VBS,LRECL=6236,BLKSIZE=24948),
//*           DSN=F11OLS.REDUCTWO.G0196V00
//*SYSPRINT DD SYSOUT=*
//*SYSIN DD DUMMY
C   28/03/97            MEMBER NAME  JBUNIGCV (JADE56.S)    FORTRAN
//F11OLS   JOB 00010622,CLASS=K,MSGLEVEL=(1,0),TIME=(3,00)
//*MAIN RELPRI=LOW
//*
//*  FOR GIVEN ENERGY, PLOT VARIOUS DIRECTIONS
//*
// EXEC NEWLIB,PS='F11OLS.JADESR',PL='F11OLS.JADELD',
//  UNIT=FAST,LEVEL=8,OPT=3,FORT=XL
//MACRO DD DSN=F11GOD.PATRECSR,DISP=SHR,UNIT=FAST
./ C USER0
./ I 10200
C END OF JOB, AFTER PRINT OF IGGS,JIGGS AND BEFORE PLOTS
      WRITE(6,8881) PTRUE
8881  FORMAT(' TRUE ENERGY:',E12.4)
C
      DELDD = .01
      DPL =  .6 - .5*DELDD
C
      DO 3535  ID = 1,100
      DPL = DPL + DELDD
C
      RAT = 0.
      ERRHSW(ID) = 0.
      ISUM1= HSHW(ID)
      ISUM2= HNOSHW(ID)
      ISUM = ISUM1 + ISUM2
      SUM = FLOAT(ISUM)
      IF(SUM.GT.0.) RAT = FLOAT(ISUM1)/SUM
      CALL HFILL( 1,DPL,0,RAT)
C
C ERROR ARRAY
C
      SUM1 = FLOAT(ISUM1)
      ERRHSW(ID) = SQRT(SUM1 +SUM1*SUM1/SUM )/SUM
C
C
C  STANDARD FORMULA, AS USED IN TRKGAM
C
      QPROB=EXP(-DPL*7./9.)
      IF(PTRUE.LT.0.5) QPROB=EXP(-DPL*7./9.*(1.-6.2E-02/SQRT(PTRUE)))
C GO TO 3535 MEANS NO CONVERSION
C     IF(QPROB.GT.RN(DUM)) GO TO 3535
C
      WEIGHT = 1. - QPROB
      CALL HFILL( 2,DPL,0,WEIGHT)
C
3535  CONTINUE
C
      CALL HPAKE(1,ERRHSW(1))
C
./ I 7800
C LEVEL 7
      IPO = IDATA(IBLN('PATR'))
      IPALGN = IDATA(IBLN('ALGN'))
      IPLGCL = IDATA(IBLN('LGCL'))
      IPVECT = IDATA(IBLN('VECT'))
      IF(IPALGN.LE.0.OR.IPLGCL.LE.0) GO TO 105
      NCLST = IDATA(IPLGCL+7)
      NWPCL = IDATA(IPLGCL+25)
      JUMPCL = IDATA(IPLGCL+2)
C
C  4-VECTOR PHOTON ENERGY AND COSTHE
C
      LO = IDATA(IPVECT+1)
      LTR = IDATA(IPVECT+2)
      NTRVEC = IDATA(IPVECT+4)
      IP4 = IPVECT + LO
      PTRUE = ADATA(IP4+4)
C
      COSTPV = ADATA(IP4+3)/PTRUE
      CALL TJOCK(COSTPV,XLEN90,XLENDM)
      DDD = XLENDM
C
      GO TO 999
C
105   WRITE(6,101) IPALGN,IPLGCL
101   FORMAT(' **** WARNING NO ALGN/LGCL BANKS,  POINTERS :',2I8)
      GO TO 1
999   NMOMGM = 0
      IF(NCLST.LE.0) GO TO 1027
      IP = IDATA(IPLGCL+3) + IPLGCL - 1 - NWPCL
      ITP = 0
      DO 522  IGM = 1,NCLST
      IP = IP + NWPCL
      CALL TRACK(IP,ITP,RMIN,NHT,FI,THE,PTOT,PT,PL,PX,PY,PZ)
      IF(IDATA(IP+8).NE.0) GO TO 522
      NMOMGM = NMOMGM + 1
      PTOTGM = PTOT
      PSEEN = ADATA(IP+NWPCL)
      COSTHG = COS(THE)
522   CONTINUE
1027  CONTINUE
      IF(NMOMGM.NE.1) GO TO 1
C
C ONE PHOTON FOUND, NOW HISTOGRAM AS FUNCTION OF DDD, SHOWER/NOSHOWER
C
      NSF56 = 0
      LSF56 = 0
      IPSF56 = IDATA(IBLN('SF56'))
      IF(IPSF56.LE.0) GO TO 3880
      NSF56 = IDATA(IPSF56+1)
      LSF56 = NSF56
      IF(NSF56.GT.10) LSF56 = 10
3880  CONTINUE
C
      DELDD = .01
      DPL =  .6
C
      DO 2525  ID = 1,100
      DPL = DPL + DELDD
      IF(DDD.GT.DPL) GO TO 2525
C
      NP = IDATA(IPSF56+2)
      IF(NP.EQ.1) HNOSHW(ID) = HNOSHW(ID) + 1
      IF(NP.GT.1) HSHW(ID) = HSHW(ID) + 1
      GO TO 2526
2525  CONTINUE
2526  CONTINUE
C
      GO TO 11
./ I 7400
C LEVEL 6
./ I 6800
C LEVEL 5
./ I 6200
C LEVEL 4
./ I 5600
C LEVEL 3
./ I 5000
C LEVEL 2
      IF((JIGG(2)/100)*100.EQ.JIGG(2))
     $ WRITE(6,2702) JIGG(2),HEAD(18),HEAD(19)
2702  FORMAT(' NNREC RUN EVENT ',3I8)
./ I 4400
C LEVEL 1
./ I 3800
C LEVEL 0
C  ITH IS CLUSTER THRESHOLD
C   IPHALG STEARS THE READOUT THRESHOLD IN MC EVENTS, DEFAULT NONZERO
      ITH = 25
      IPHALG = 0
      WRITE(6,1221) ITH,IPHALG
1221  FORMAT('  CLUSTER THRESHOLD, READOUT FLAG ',2I5)
C
      ISECLF = 3
      PTRUEO = 0.
      ICOSM = 0
C
      CALL HBOOK1( 1,'RATIO CONV/ALL VS X0, SF5/6 PROG.$',100,0.6,1.6)
      CALL HBOOK1( 2,'RATIO CONV/ALL VS X0, STANDARD$',100,0.6,1.6)
1223  CONTINUE
./ I 1300
C DECLARATIVE LEVEL
      COMMON /CLGPRM/ ITH,MAXCLS,IRLTHD,IRLTH2,IRLTH3
      COMMON / CRDSTA / IMCDUM(12),IPHALG
      COMMON / CTLIM / ISECLF
      DIMENSION HSHW(100), HNOSHW(100), ERRHSW(100)
//*
// EXEC GEP,PARM.LKED='LIST,SIZE=(600K,60K)',REGION.LKED=800K,
//      REGION.GO=1600K,VS=HBOOK,
//      DN='F11OLS.UNIGAMCV.E400'
//LKED.SYSLIB DD
//            DD
//            DD
//            DD
//            DD
//            DD DISP=SHR,UNIT=,DSN=&&LOAD
//            DD DISP=SHR,DSN=F11OLS.JADE56.L
//            DD DISP=SHR,UNIT=FAST,DSN=F11GOD.PATRECLD
//            DD DISP=SHR,UNIT=FAST,DSN=F11LHO.JADEGL
//            DD DISP=SHR,UNIT=FAST,DSN=JADELG.LOAD
//            DD DISP=SHR,UNIT=FAST,DSN=F22ALL.JADEMUL
//            DD DISP=SHR,UNIT=FAST,DSN=R01UTL.HBOOK321.L
//            DD DISP=SHR,UNIT=FAST,DSN=R01UTL.CERN.KERNLIB4
//            DD DISP=SHR,UNIT=FAST,DSN=F1EBLO.BOSLIB.L
//LKED.SYSIN DD *
 INCLUDE SYSLIB(JDMAIN0)
 INCLUDE SYSLIB(USER0,LGECORZ)
//* INCLUDE SYSLIB(ENGLOSM)
//* INCLUDE SYSLIB(LGECORM)
//GO.FT22F001 DD UNIT=FAST,DISP=SHR,DSN=F11LHO.AUPDAT1
//*                        INPUT:
//*O.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM86.SUME800
//*O.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM86.SUME600
//GO.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM86.SUME400
//*O.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM86.SUME200A
//*           DD DISP=SHR,DSN=F11OLS.UNIGAM86.SUME200B
//*O.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM86.SUME100A
//*           DD DISP=SHR,DSN=F11OLS.UNIGAM86.SUME100B
//*                        OUTPUT:
//*GO.FT03F001 DD UNIT=DISK,DISP=(,PASS),
//*   DCB=(R01DCB.VBS,LRECL=6236),
//*       SPACE=(CYL,(220,20)),DSN=&&SCRTCH
//*GO.FT03F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*         MSVGP=VTMP,UNIT=3330V
//GO.FT03F001 DD DUMMY,DCB=R01DCB.VBS
//*GO.FT03F001 DD UNIT=FAST,DISP=SHR,DSN=F11OLS.RAETEST
//*GO.FT03F001 DD UNIT=FAST,DISP=(NEW,CATLG,DELETE),DCB=R01DCB.VBS,
//*   SPACE=(TRK,(10,2),RLSE),DSN=F11OLS.BARRIES.PROBLEM
//*GO.FT03F001 DD UNIT=TAPE,DISP=(NEW,CATLG,DELETE),
//*    DCB=(R01DCB.VBS,LRECL=6236,BLKSIZE=24948),
//*   DSN=F11OLS.REDUCTWO.G0216V00
//*
//*  COPY DISK OUTPUT TO TAPE
//*
//*COPY EXEC PGM=IEBGENER
//*SYSUT1 DD UNIT=DISK,DISP=(OLD,DELETE),DSN=&&SCRTCH
//*SYSUT2 DD UNIT=TAPE,DISP=(NEW,CATLG,DELETE),
//*          DCB=(R01DCB.VBS,LRECL=6236,BLKSIZE=24948),
//*           DSN=F11OLS.REDUCTWO.G0196V00
//*SYSPRINT DD SYSOUT=*
//*SYSIN DD DUMMY
C   28/03/97            MEMBER NAME  JBUNIGCW (JADE56.S)    FORTRAN
//F11JOL   JOB 00010622,CLASS=L,MSGLEVEL=(1,0),TIME=(3,40)
//*MAIN RELPRI=MED
//*
//*  FOR GIVEN ENERGY, PLOT VARIOUS DIRECTIONS
//*
// EXEC NEWLIB,PS='F11OLS.JADESR',PL='F11OLS.JADELD',
//  UNIT=FAST,LEVEL=8,OPT=3,FORT=XL
//MACRO DD DSN=F11GOD.PATRECSR,DISP=SHR,UNIT=FAST
./ C USER0
./ I 10200
C END OF JOB, AFTER PRINT OF IGGS,JIGGS AND BEFORE PLOTS
      WRITE(6,8881) PTRUE
8881  FORMAT(' TRUE ENERGY:',E12.4)
C
      DELDD = .01
      DPL =  .6 - .5*DELDD
C
      DO 3535  ID = 1,100
      DPL = DPL + DELDD
C
      RAT = 0.
      ERRHSW(ID) = 0.
      ISUM1= HSHW(ID)
      ISUM2= HNOSHW(ID)
      ISUM = ISUM1 + ISUM2
      SUM = FLOAT(ISUM)
      IF(SUM.GT.0.) RAT = FLOAT(ISUM1)/SUM
      CALL HFILL( 1,DPL,0,RAT)
C
C ERROR ARRAY
C
      SUM1 = FLOAT(ISUM1)
      IF(SUM.GT.0.) ERRHSW(ID) = SQRT(SUM1 +SUM1*SUM1/SUM )/SUM
C
C
C  STANDARD FORMULA, AS USED IN TRKGAM
C
      QPROB=EXP(-DPL*7./9.)
      IF(PTRUE.LT.0.5) QPROB=EXP(-DPL*7./9.*(1.-6.2E-02/SQRT(PTRUE)))
C GO TO 3535 MEANS NO CONVERSION
C     IF(QPROB.GT.RN(DUM)) GO TO 3535
C
      WEIGHT = 1. - QPROB
      CALL HFILL( 2,DPL,0,WEIGHT)
C
3535  CONTINUE
C
      CALL HPAKE(1,ERRHSW(1))
C
./ I 7800
C LEVEL 7
./ I 7400
C LEVEL 6
./ I 6800
C LEVEL 5
./ I 6200
C LEVEL 4
./ I 5600
C LEVEL 3
./ I 5000
C LEVEL 2
      IF((JIGG(2)/100)*100.EQ.JIGG(2))
     $ WRITE(6,2702) JIGG(2),HEAD(18),HEAD(19)
2702  FORMAT(' NNREC RUN EVENT ',3I8)
      IPVECT = IDATA(IBLN('VECT'))
C
C  4-VECTOR PHOTON ENERGY AND COSTHE
C
      LO = IDATA(IPVECT+1)
      LTR = IDATA(IPVECT+2)
      NTRVEC = IDATA(IPVECT+4)
      IP4 = IPVECT + LO
      PTRUE = ADATA(IP4+4)
C
      COSTPV = ADATA(IP4+3)/PTRUE
      CALL TJOCK(COSTPV,XLEN90,XLENDM)
      DDD = XLENDM
C
C ONE PHOTON FOUND, NOW HISTOGRAM AS FUNCTION OF DDD, SHOWER/NOSHOWER
C
      NSF56 = 0
      LSF56 = 0
      IPSF56 = IDATA(IBLN('SF56'))
      IF(IPSF56.LE.0) GO TO 1
      NSF56 = IDATA(IPSF56+1)
      LSF56 = NSF56
      IF(NSF56.GT.10) LSF56 = 10
      NP = IDATA(IPSF56+2)
C
      CALL HFILL(3,NP,0,1.)
      IF(NP.LT.0.OR.NP.GT.200) GO TO 1
C
      DELDD = .01
      DPL =  .6
C
      DO 2525  ID = 1,100
      DPL = DPL + DELDD
      IF(DDD.GT.DPL) GO TO 2525
C
      IF(NP.EQ.1) HNOSHW(ID) = HNOSHW(ID) + 1
      IF(NP.NE.1) HSHW(ID) = HSHW(ID) + 1
      GO TO 2526
2525  CONTINUE
2526  CONTINUE
C
      GO TO 11
./ I 4400
C LEVEL 1
./ I 3800
C LEVEL 0
C  ITH IS CLUSTER THRESHOLD
C   IPHALG STEARS THE READOUT THRESHOLD IN MC EVENTS, DEFAULT NONZERO
      ITH = 25
      IPHALG = 0
      WRITE(6,1221) ITH,IPHALG
1221  FORMAT('  CLUSTER THRESHOLD, READOUT FLAG ',2I5)
C
      ISECLF = 3
      PTRUEO = 0.
      ICOSM = 0
C
      CALL HBOOK1( 1,'RATIO CONV/ALL VS X0, SF5/6 PROG.$',100,0.6,1.6)
      CALL HBOOK1( 2,'RATIO CONV/ALL VS X0, STANDARD$',100,0.6,1.6)
      CALL HBOOK1( 3,'NP FROM SF56',50,-1.5,48.5)
1223  CONTINUE
./ I 1300
C DECLARATIVE LEVEL
      COMMON /CLGPRM/ ITH,MAXCLS,IRLTHD,IRLTH2,IRLTH3
      COMMON / CRDSTA / IMCDUM(12),IPHALG
      COMMON / CTLIM / ISECLF
      DIMENSION HSHW(100), HNOSHW(100), ERRHSW(100)
//*
// EXEC GEP,PARM.LKED='LIST,SIZE=(600K,60K)',REGION.LKED=800K,
//      REGION.GO=1600K,VS=HBOOK,
//      DN='F11OLS.UNIGAMCW.E100'
//LKED.SYSLIB DD
//            DD
//            DD
//            DD
//            DD
//            DD DISP=SHR,UNIT=,DSN=&&LOAD
//            DD DISP=SHR,DSN=F11OLS.JADE56.L
//            DD DISP=SHR,UNIT=FAST,DSN=F11GOD.PATRECLD
//            DD DISP=SHR,UNIT=FAST,DSN=F11LHO.JADEGL
//            DD DISP=SHR,UNIT=FAST,DSN=JADELG.LOAD
//            DD DISP=SHR,UNIT=FAST,DSN=F22ALL.JADEMUL
//            DD DISP=SHR,UNIT=FAST,DSN=R01UTL.HBOOK321.L
//            DD DISP=SHR,UNIT=FAST,DSN=R01UTL.CERN.KERNLIB4
//            DD DISP=SHR,UNIT=FAST,DSN=F1EBLO.BOSLIB.L
//LKED.SYSIN DD *
 INCLUDE SYSLIB(JDMAIN0)
 INCLUDE SYSLIB(USER0,LGECORZ)
//* INCLUDE SYSLIB(ENGLOSM)
//* INCLUDE SYSLIB(LGECORM)
//GO.FT22F001 DD UNIT=FAST,DISP=SHR,DSN=F11LHO.AUPDAT1
//*                        INPUT:
//*O.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM86.SUME800
//*O.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM86.SUME600
//*O.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM86.SUME400
//*O.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM86.SUME200A
//*           DD DISP=SHR,DSN=F11OLS.UNIGAM86.SUME200B
//GO.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM86.SUME100A
//            DD DISP=SHR,DSN=F11OLS.UNIGAM86.SUME100B
//*                        OUTPUT:
//*GO.FT03F001 DD UNIT=DISK,DISP=(,PASS),
//*   DCB=(R01DCB.VBS,LRECL=6236),
//*       SPACE=(CYL,(220,20)),DSN=&&SCRTCH
//*GO.FT03F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*         MSVGP=VTMP,UNIT=3330V
//GO.FT03F001 DD DUMMY,DCB=R01DCB.VBS
//*GO.FT03F001 DD UNIT=FAST,DISP=SHR,DSN=F11OLS.RAETEST
//*GO.FT03F001 DD UNIT=FAST,DISP=(NEW,CATLG,DELETE),DCB=R01DCB.VBS,
//*   SPACE=(TRK,(10,2),RLSE),DSN=F11OLS.BARRIES.PROBLEM
//*GO.FT03F001 DD UNIT=TAPE,DISP=(NEW,CATLG,DELETE),
//*    DCB=(R01DCB.VBS,LRECL=6236,BLKSIZE=24948),
//*   DSN=F11OLS.REDUCTWO.G0216V00
//*
//*  COPY DISK OUTPUT TO TAPE
//*
//*COPY EXEC PGM=IEBGENER
//*SYSUT1 DD UNIT=DISK,DISP=(OLD,DELETE),DSN=&&SCRTCH
//*SYSUT2 DD UNIT=TAPE,DISP=(NEW,CATLG,DELETE),
//*          DCB=(R01DCB.VBS,LRECL=6236,BLKSIZE=24948),
//*           DSN=F11OLS.REDUCTWO.G0196V00
//*SYSPRINT DD SYSOUT=*
//*SYSIN DD DUMMY
C   28/03/97            MEMBER NAME  JBUNIGGP (JADE56.S)    FORTRAN
//F11OLS   JOB CLASS=K,MSGLEVEL=(1,0),TIME=(3,00)
//*MAIN RELPRI=LOW
// EXEC NEWLIB,PS='F11OLS.JADESR',PL='F11OLS.JADELD',
//  UNIT=FAST,LEVEL=8,OPT=3,FORT=XL
//MACRO DD DSN=F11GOD.PATRECSR,DISP=SHR,UNIT=FAST
./ C USER0
./ I 10200
C END OF JOB, AFTER PRINT OF IGGS,JIGGS AND BEFORE PLOTS
      COSM = 0.
      IF(ICOSM.GT.0) COSM = COSMEA/FLOAT(ICOSM)
      WRITE(6,8881) COSM,COSMEA,ICOSM
8881  FORMAT(' MEAN COSTHETA:',E14.6,'  COSMEA ICOSM ',E14.6,I6)
./ I 7800
C LEVEL 7
      IPO = IDATA(IBLN('PATR'))
      IPALGN = IDATA(IBLN('ALGN'))
      IPLGCL = IDATA(IBLN('LGCL'))
      IPVECT = IDATA(IBLN('VECT'))
      IF(IPALGN.LE.0.OR.IPLGCL.LE.0) GO TO 105
      NCLST = IDATA(IPLGCL+7)
      NWPCL = IDATA(IPLGCL+25)
      JUMPCL = IDATA(IPLGCL+2)
C  4-VECTOR PHOTON ENERGY
      LO = IDATA(IPVECT+1)
      LTR = IDATA(IPVECT+2)
      NTRVEC = IDATA(IPVECT+4)
      IP4 = IPVECT + LO
      PTRUE = ADATA(IP4+4)
      GO TO 999
105   WRITE(6,101) IPALGN,IPLGCL
101   FORMAT(' **** WARNING NO ALGN/LGCL BANKS,  POINTERS :',2I8)
      GO TO 1
999   NMOMGM = 0
      IF(NCLST.LE.0) GO TO 1027
      IP = IDATA(IPLGCL+3) + IPLGCL - 1 - NWPCL
      ITP = 0
      DO 522  IGM = 1,NCLST
      IP = IP + NWPCL
      CALL TRACK(IP,ITP,RMIN,NHT,FI,THE,PTOT,PT,PL,PX,PY,PZ)
      IF(IDATA(IP+8).NE.0) GO TO 522
      NMOMGM = NMOMGM + 1
      PTOTGM = PTOT
      PSEEN = ADATA(IP+NWPCL)
      COSTHG = COS(THE)
522   CONTINUE
1027  CONTINUE
      IF(NMOMGM.NE.1) GO TO 1
      COSMEA = COSMEA + COSTHG
      ICOSM = ICOSM + 1
C
C ONE PHOTON FOUND, NOW COMPARE ENERGY
C
      IF(PTRUE.NE.PTRUEO) IPRIN = 0
      IF(IPRIN.EQ.0) WRITE(6,4010) PTRUE
4010  FORMAT('      TRUE PHOTON ENERGY: ',E12.4)
      IF(IPRIN.EQ.0) PTRUEO = PTRUE
      IF(IPRIN.EQ.0) IPRIN = 1
C
      RATIO1 = PSEEN / PTRUE
      RATIO2 = PTOTGM / PTRUE
C
      IFILST = 0
      IF(PTRUE.GT..150) IFILST = IFILST + 10
      IF(PTRUE.GT..250) IFILST = IFILST + 10
      IF(PTRUE.GT..450) IFILST = IFILST + 10
      IF(PTRUE.GT..650) IFILST = IFILST + 10
C
      CALL HFILL(IFILST + 1,RATIO1,0,1.)
      CALL HFILL(IFILST + 2,RATIO2,0,1.)
      GO TO 11
./ I 7400
C LEVEL 6
./ I 6800
C LEVEL 5
./ I 6200
C LEVEL 4
./ I 5600
C LEVEL 3
./ I 5000
C LEVEL 2
      IF((JIGG(2)/100)*100.EQ.JIGG(2))
     $ WRITE(6,2702) JIGG(2),HEAD(18),HEAD(19)
2702  FORMAT(' NNREC RUN EVENT ',3I8)
./ I 4400
C LEVEL 1
./ I 3800
C LEVEL 0
C  ITH IS CLUSTER THRESHOLD
C   IPHALG STEARS THE READOUT THRESHOLD IN MC EVENTS, DEFAULT NONZERO
      ITH = 25
      IPHALG = 1
      WRITE(6,1221) ITH,IPHALG
1221  FORMAT('  CLUSTER THRESHOLD, READOUT FLAG ',2I5)
C
      ISECLF = 3
      IPRIN = 0
      PTRUEO = 0.
      COSMEA = 0.
      ICOSM = 0
C
      CALL HBOOK1( 1,' SEEN ENERGY/TRUE ENERGY, E100 $',100,0.,2.)
      CALL HBOOK1( 2,' CORR ENERGY/TRUE ENERGY, E100 $',100,0.,2.)
      CALL HBOOK1(11,' SEEN ENERGY/TRUE ENERGY, E200 $',100,0.,2.)
      CALL HBOOK1(12,' CORR ENERGY/TRUE ENERGY, E200 $',100,0.,2.)
      CALL HBOOK1(21,' SEEN ENERGY/TRUE ENERGY, E400 $',100,0.,2.)
      CALL HBOOK1(22,' CORR ENERGY/TRUE ENERGY, E400 $',100,0.,2.)
      CALL HBOOK1(31,' SEEN ENERGY/TRUE ENERGY, E600 $',100,0.,2.)
      CALL HBOOK1(32,' CORR ENERGY/TRUE ENERGY, E600 $',100,0.,2.)
      CALL HBOOK1(41,' SEEN ENERGY/TRUE ENERGY, E800 $',100,0.,2.)
      CALL HBOOK1(42,' CORR ENERGY/TRUE ENERGY, E800 $',100,0.,2.)
1223  CONTINUE
./ I 1300
C DECLARATIVE LEVEL
      COMMON /CLGPRM/ ITH,MAXCLS,IRLTHD,IRLTH2,IRLTH3
      COMMON / CRDSTA / IMCDUM(12),IPHALG
      COMMON / CTLIM / ISECLF
//*
// EXEC GEP,PARM.LKED='LIST,SIZE=(600K,60K)',REGION.LKED=800K,
//      REGION.GO=1600K,VS=HBOOK,
//      DN='F11OLS.UNIGAMGX.DIRB.SF6T1.ENGLOS'
//LKED.SYSLIB DD
//            DD
//            DD
//            DD
//            DD
//            DD DISP=SHR,UNIT=,DSN=&&LOAD
//            DD DISP=SHR,DSN=F11OLS.JADE56.L
//            DD DISP=SHR,UNIT=FAST,DSN=F11GOD.PATRECLD
//            DD DISP=SHR,UNIT=FAST,DSN=F11LHO.JADEGL
//            DD DISP=SHR,UNIT=FAST,DSN=JADELG.LOAD
//            DD DISP=SHR,UNIT=FAST,DSN=F22ALL.JADEMUL
//            DD DISP=SHR,UNIT=FAST,DSN=R01UTL.HBOOK321.L
//            DD DISP=SHR,UNIT=FAST,DSN=R01UTL.CERN.KERNLIB4
//            DD DISP=SHR,UNIT=FAST,DSN=F1EBLO.BOSLIB.L
//LKED.SYSIN DD *
 INCLUDE SYSLIB(JDMAIN0)
 INCLUDE SYSLIB(USER0,LGECORX)
//*  INCLUDE SYSLIB(ENGLOSM)
//* INCLUDE SYSLIB(LGECORM)
//GO.FT22F001 DD UNIT=FAST,DISP=SHR,DSN=F11LHO.AUPDAT1
//*                        INPUT:
//GO.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM.E100DB.SF6
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E200DB.SF6
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400DB.SF6A
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400DB.SF6B
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E600DB.SF6A
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E600DB.SF6B
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E800DB.SF6A
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E800DB.SF6B
//*                        OUTPUT:
//*GO.FT03F001 DD UNIT=DISK,DISP=(,PASS),
//*   DCB=(R01DCB.VBS,LRECL=6236),
//*       SPACE=(CYL,(220,20)),DSN=&&SCRTCH
//*GO.FT03F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*    DSN=F11OLS.TIMEOUT1,
//*         MSVGP=VTMP,UNIT=3330V
//GO.FT03F001 DD DUMMY,DCB=R01DCB.VBS
//*GO.FT03F001 DD UNIT=FAST,DISP=SHR,DSN=F11OLS.RAETEST
//*GO.FT03F001 DD UNIT=FAST,DISP=(NEW,CATLG,DELETE),DCB=R01DCB.VBS,
//*   SPACE=(TRK,(10,2),RLSE),DSN=F11OLS.BARRIES.PROBLEM
//*GO.FT03F001 DD UNIT=TAPE,DISP=(NEW,CATLG,DELETE),
//*    DCB=(R01DCB.VBS,LRECL=6236,BLKSIZE=24948),
//*   DSN=F11OLS.REDUCTWO.G0216V00
//*
//*  COPY DISK OUTPUT TO TAPE
//*
//*COPY EXEC PGM=IEBGENER
//*SYSUT1 DD UNIT=DISK,DISP=(OLD,DELETE),DSN=&&SCRTCH
//*SYSUT2 DD UNIT=TAPE,DISP=(NEW,CATLG,DELETE),
//*          DCB=(R01DCB.VBS,LRECL=6236,BLKSIZE=24948),
//*           DSN=F11OLS.REDUCTWO.G0196V00
//*SYSPRINT DD SYSOUT=*
//*SYSIN DD DUMMY
C   28/03/97            MEMBER NAME  JBUNIGHP (JADE56.S)    FORTRAN
//F11OLS   JOB 00010622,CLASS=L,MSGLEVEL=(1,0),TIME=(7,00)
//*MAIN RELPRI=MED
//*
//*  FOR GIVEN ENERGY, PLOT VARIOUS DIRECTIONS
//*
// EXEC NEWLIB,PS='F11OLS.JADESR',PL='F11OLS.JADELD',
//  UNIT=FAST,LEVEL=8,OPT=3,FORT=XL
//MACRO DD DSN=F11GOD.PATRECSR,DISP=SHR,UNIT=FAST
./ C USER0
./ I 10200
C END OF JOB, AFTER PRINT OF IGGS,JIGGS AND BEFORE PLOTS
      WRITE(6,8881) PTRUE
8881  FORMAT(' TRUE ENERGY:',E12.4)
./ I 7800
C LEVEL 7
      IPO = IDATA(IBLN('PATR'))
      IPALGN = IDATA(IBLN('ALGN'))
      IPLGCL = IDATA(IBLN('LGCL'))
      IPVECT = IDATA(IBLN('VECT'))
      IF(IPALGN.LE.0.OR.IPLGCL.LE.0) GO TO 105
      NCLST = IDATA(IPLGCL+7)
      NWPCL = IDATA(IPLGCL+25)
      JUMPCL = IDATA(IPLGCL+2)
C  4-VECTOR PHOTON ENERGY
      LO = IDATA(IPVECT+1)
      LTR = IDATA(IPVECT+2)
      NTRVEC = IDATA(IPVECT+4)
      IP4 = IPVECT + LO
      PTRUE = ADATA(IP4+4)
      GO TO 999
105   WRITE(6,101) IPALGN,IPLGCL
101   FORMAT(' **** WARNING NO ALGN/LGCL BANKS,  POINTERS :',2I8)
      GO TO 1
999   NMOMGM = 0
      IF(NCLST.LE.0) GO TO 1027
      IP = IDATA(IPLGCL+3) + IPLGCL - 1 - NWPCL
      ITP = 0
      DO 522  IGM = 1,NCLST
      IP = IP + NWPCL
      CALL TRACK(IP,ITP,RMIN,NHT,FI,THE,PTOT,PT,PL,PX,PY,PZ)
      IF(IDATA(IP+8).NE.0) GO TO 522
      NMOMGM = NMOMGM + 1
      PTOTGM = PTOT
      PSEEN = ADATA(IP+NWPCL)
      COSTHG = COS(THE)
522   CONTINUE
1027  CONTINUE
      IF(NMOMGM.NE.1) GO TO 1
C
C ONE PHOTON FOUND, NOW COMPARE ENERGY
C
C
      RATIO1 = PSEEN / PTRUE
      RATIO2 = PTOTGM / PTRUE
C
      IFILST = 0
      IF(COSTHG.GT..090) IFILST = IFILST + 10
      IF(COSTHG.GT..180) IFILST = IFILST + 10
      IF(COSTHG.GT..280) IFILST = IFILST + 10
      IF(COSTHG.GT..390) IFILST = IFILST + 10
      IF(COSTHG.GT..485) IFILST = IFILST + 10
      IF(COSTHG.GT..580) IFILST = IFILST + 10
      IF(COSTHG.GT..650) IFILST = IFILST + 10
      IF(COSTHG.GT..705) IFILST = IFILST + 10
      IF(COSTHG.GT..750) IFILST = IFILST + 10
      IF(COSTHG.GT..788) IFILST = IFILST + 10
C
      CALL HFILL(IFILST + 1,RATIO1,0,1.)
      CALL HFILL(IFILST + 2,RATIO2,0,1.)
      GO TO 11
./ I 7400
C LEVEL 6
./ I 6800
C LEVEL 5
./ I 6200
C LEVEL 4
./ I 5600
C LEVEL 3
./ I 5000
C LEVEL 2
      IF((JIGG(2)/100)*100.EQ.JIGG(2))
     $ WRITE(6,2702) JIGG(2),HEAD(18),HEAD(19)
2702  FORMAT(' NNREC RUN EVENT ',3I8)
./ I 4400
C LEVEL 1
./ I 3800
C LEVEL 0
C  ITH IS CLUSTER THRESHOLD
C   IPHALG STEARS THE READOUT THRESHOLD IN MC EVENTS, DEFAULT NONZERO
      ITH = 25
      IPHALG = 1
      WRITE(6,1221) ITH,IPHALG
1221  FORMAT('  CLUSTER THRESHOLD, READOUT FLAG ',2I5)
C
      ISECLF = 3
      PTRUEO = 0.
      ICOSM = 0
C
      CALL HBOOK1( 1,' SEEN ENERGY/TRUE ENERGY, D1 $',100,0.,2.)
      CALL HBOOK1( 2,' CORR ENERGY/TRUE ENERGY, D1 $',100,0.,2.)
      CALL HBOOK1(11,' SEEN ENERGY/TRUE ENERGY, D2 $',100,0.,2.)
      CALL HBOOK1(12,' CORR ENERGY/TRUE ENERGY, D2 $',100,0.,2.)
      CALL HBOOK1(21,' SEEN ENERGY/TRUE ENERGY, D3 $',100,0.,2.)
      CALL HBOOK1(22,' CORR ENERGY/TRUE ENERGY, D3 $',100,0.,2.)
      CALL HBOOK1(31,' SEEN ENERGY/TRUE ENERGY, D4 $',100,0.,2.)
      CALL HBOOK1(32,' CORR ENERGY/TRUE ENERGY, D4 $',100,0.,2.)
      CALL HBOOK1(41,' SEEN ENERGY/TRUE ENERGY, D5 $',100,0.,2.)
      CALL HBOOK1(42,' CORR ENERGY/TRUE ENERGY, D5 $',100,0.,2.)
      CALL HBOOK1(51,' SEEN ENERGY/TRUE ENERGY, D6 $',100,0.,2.)
      CALL HBOOK1(52,' CORR ENERGY/TRUE ENERGY, D6 $',100,0.,2.)
      CALL HBOOK1(61,' SEEN ENERGY/TRUE ENERGY, D7 $',100,0.,2.)
      CALL HBOOK1(62,' CORR ENERGY/TRUE ENERGY, D7 $',100,0.,2.)
      CALL HBOOK1(71,' SEEN ENERGY/TRUE ENERGY, D8 $',100,0.,2.)
      CALL HBOOK1(72,' CORR ENERGY/TRUE ENERGY, D8 $',100,0.,2.)
      CALL HBOOK1(81,' SEEN ENERGY/TRUE ENERGY, D9 $',100,0.,2.)
      CALL HBOOK1(82,' CORR ENERGY/TRUE ENERGY, D9 $',100,0.,2.)
      CALL HBOOK1(91,' SEEN ENERGY/TRUE ENERGY, DA $',100,0.,2.)
      CALL HBOOK1(92,' CORR ENERGY/TRUE ENERGY, DA $',100,0.,2.)
      CALL HBOOK1(101,' SEEN ENERGY/TRUE ENERGY, DB $',100,0.,2.)
      CALL HBOOK1(102,' CORR ENERGY/TRUE ENERGY, DB $',100,0.,2.)
1223  CONTINUE
./ I 1300
C DECLARATIVE LEVEL
      COMMON /CLGPRM/ ITH,MAXCLS,IRLTHD,IRLTH2,IRLTH3
      COMMON / CRDSTA / IMCDUM(12),IPHALG
      COMMON / CTLIM / ISECLF
//*
// EXEC GEP,PARM.LKED='LIST,SIZE=(600K,60K)',REGION.LKED=800K,
//      REGION.GO=1600K,VS=HBOOK,
//      DN='F11OLS.UNIGAMGX.E400.SF6T1.ENGLOSM'
//LKED.SYSLIB DD
//            DD
//            DD
//            DD
//            DD
//            DD DISP=SHR,UNIT=,DSN=&&LOAD
//            DD DISP=SHR,DSN=F11OLS.JADE56.L
//            DD DISP=SHR,UNIT=FAST,DSN=F11GOD.PATRECLD
//            DD DISP=SHR,UNIT=FAST,DSN=F11LHO.JADEGL
//            DD DISP=SHR,UNIT=FAST,DSN=JADELG.LOAD
//            DD DISP=SHR,UNIT=FAST,DSN=F22ALL.JADEMUL
//            DD DISP=SHR,UNIT=FAST,DSN=R01UTL.HBOOK321.L
//            DD DISP=SHR,UNIT=FAST,DSN=R01UTL.CERN.KERNLIB4
//            DD DISP=SHR,UNIT=FAST,DSN=F1EBLO.BOSLIB.L
//LKED.SYSIN DD *
 INCLUDE SYSLIB(JDMAIN0)
 INCLUDE SYSLIB(USER0,LGECORX)
 INCLUDE SYSLIB(ENGLOSM)
//* INCLUDE SYSLIB(LGECORM)
//GO.FT22F001 DD UNIT=FAST,DISP=SHR,DSN=F11LHO.AUPDAT1
//*                        INPUT:
//GO.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D1.SF6A
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D1.SF6B
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D2.SF6A
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D2.SF6B
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D3.SF6A
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D3.SF6B
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D4.SF6A
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D4.SF6B
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D5.SF6A
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D5.SF6B
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D6.SF6A
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D6.SF6B
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D7.SF6A
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D7.SF6B
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D8.SF6A
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D8.SF6B
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D9.SF6A
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400D9.SF6B
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400DA.SF6A
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400DA.SF6B
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400DB.SF6A
//            DD DISP=SHR,DSN=F11OLS.UNIGAM.E400DB.SF6B
//*                        OUTPUT:
//*GO.FT03F001 DD UNIT=DISK,DISP=(,PASS),
//*   DCB=(R01DCB.VBS,LRECL=6236),
//*       SPACE=(CYL,(220,20)),DSN=&&SCRTCH
//*GO.FT03F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*         MSVGP=VTMP,UNIT=3330V
//GO.FT03F001 DD DUMMY,DCB=R01DCB.VBS
//*GO.FT03F001 DD UNIT=FAST,DISP=SHR,DSN=F11OLS.RAETEST
//*GO.FT03F001 DD UNIT=FAST,DISP=(NEW,CATLG,DELETE),DCB=R01DCB.VBS,
//*   SPACE=(TRK,(10,2),RLSE),DSN=F11OLS.BARRIES.PROBLEM
//*GO.FT03F001 DD UNIT=TAPE,DISP=(NEW,CATLG,DELETE),
//*    DCB=(R01DCB.VBS,LRECL=6236,BLKSIZE=24948),
//*   DSN=F11OLS.REDUCTWO.G0216V00
//*
//*  COPY DISK OUTPUT TO TAPE
//*
//*COPY EXEC PGM=IEBGENER
//*SYSUT1 DD UNIT=DISK,DISP=(OLD,DELETE),DSN=&&SCRTCH
//*SYSUT2 DD UNIT=TAPE,DISP=(NEW,CATLG,DELETE),
//*          DCB=(R01DCB.VBS,LRECL=6236,BLKSIZE=24948),
//*           DSN=F11OLS.REDUCTWO.G0196V00
//*SYSPRINT DD SYSOUT=*
//*SYSIN DD DUMMY
C   28/03/97            MEMBER NAME  JBUNIGIP (JADE56.S)    FORTRAN
//F11OLS   JOB 00010622,CLASS=L,MSGLEVEL=(1,0),TIME=(6,30)
//*MAIN RELPRI=MED
//*
//*  FOR GIVEN ENERGY, PLOT VARIOUS DIRECTIONS
//*
// EXEC NEWLIB,PS='F11OLS.JADESR',PL='F11OLS.JADELD',
//  UNIT=FAST,LEVEL=8,OPT=3,FORT=XL
//MACRO DD DSN=F11GOD.PATRECSR,DISP=SHR,UNIT=FAST
./ C USER0
./ I 10200
C END OF JOB, AFTER PRINT OF IGGS,JIGGS AND BEFORE PLOTS
      WRITE(6,8881) PTRUE
8881  FORMAT(' TRUE ENERGY:',E12.4)
./ I 7800
C LEVEL 7
      IPO = IDATA(IBLN('PATR'))
      IPALGN = IDATA(IBLN('ALGN'))
      IPLGCL = IDATA(IBLN('LGCL'))
      IPVECT = IDATA(IBLN('VECT'))
      IF(IPALGN.LE.0.OR.IPLGCL.LE.0) GO TO 105
      NCLST = IDATA(IPLGCL+7)
      NWPCL = IDATA(IPLGCL+25)
      JUMPCL = IDATA(IPLGCL+2)
C  4-VECTOR PHOTON ENERGY
      LO = IDATA(IPVECT+1)
      LTR = IDATA(IPVECT+2)
      NTRVEC = IDATA(IPVECT+4)
      IP4 = IPVECT + LO
      PTRUE = ADATA(IP4+4)
      GO TO 999
105   WRITE(6,101) IPALGN,IPLGCL
101   FORMAT(' **** WARNING NO ALGN/LGCL BANKS,  POINTERS :',2I8)
      GO TO 1
999   NMOMGM = 0
      IF(NCLST.LE.0) GO TO 1027
      IP = IDATA(IPLGCL+3) + IPLGCL - 1 - NWPCL
      ITP = 0
      DO 522  IGM = 1,NCLST
      IP = IP + NWPCL
      CALL TRACK(IP,ITP,RMIN,NHT,FI,THE,PTOT,PT,PL,PX,PY,PZ)
      IF(IDATA(IP+8).NE.0) GO TO 522
      NMOMGM = NMOMGM + 1
      PTOTGM = PTOT
      PSEEN = ADATA(IP+NWPCL)
      COSTHG = COS(THE)
522   CONTINUE
1027  CONTINUE
      IF(NMOMGM.NE.1) GO TO 1
C
C ONE PHOTON FOUND, NOW COMPARE SEEN ENERGY TO TRUE, SHOWER/NOSHOWER
C
      NSF56 = 0
      LSF56 = 0
      IPSF56 = IDATA(IBLN('SF56'))
      IF(IPSF56.LE.0) GO TO 3880
      NSF56 = IDATA(IPSF56+1)
      LSF56 = NSF56
      IF(NSF56.GT.10) LSF56 = 10
3880  CONTINUE
C
      RATIO1 = PSEEN / PTRUE
      RATIO2 = PTOTGM / PTRUE
C
      IFILST = 0
      IF(COSTHG.GT..090) IFILST = IFILST + 10
      IF(COSTHG.GT..180) IFILST = IFILST + 10
      IF(COSTHG.GT..280) IFILST = IFILST + 10
      IF(COSTHG.GT..390) IFILST = IFILST + 10
      IF(COSTHG.GT..485) IFILST = IFILST + 10
      IF(COSTHG.GT..580) IFILST = IFILST + 10
      IF(COSTHG.GT..650) IFILST = IFILST + 10
      IF(COSTHG.GT..705) IFILST = IFILST + 10
      IF(COSTHG.GT..750) IFILST = IFILST + 10
      IF(COSTHG.GT..788) IFILST = IFILST + 10
C
      CALL HFILL(IFILST + 1,RATIO1,0,1.)
      CALL HFILL(IFILST + 2,RATIO2,0,1.)
      IF(LSF56.EQ.0) GO TO 11
C
      NP = IDATA(IPSF56+2)
      IF(NP.GT.1)  CALL HFILL(IFILST + 3,RATIO1,0,1.)
      IF(NP.GT.1)  CALL HFILL(IFILST + 4,RATIO2,0,1.)
      IF(NP.EQ.1)  CALL HFILL(IFILST + 5,RATIO1,0,1.)
      IF(NP.EQ.1)  CALL HFILL(IFILST + 6,RATIO2,0,1.)
C
      GO TO 11
./ I 7400
C LEVEL 6
./ I 6800
C LEVEL 5
./ I 6200
C LEVEL 4
./ I 5600
C LEVEL 3
./ I 5000
C LEVEL 2
      IF((JIGG(2)/100)*100.EQ.JIGG(2))
     $ WRITE(6,2702) JIGG(2),HEAD(18),HEAD(19)
2702  FORMAT(' NNREC RUN EVENT ',3I8)
./ I 4400
C LEVEL 1
./ I 3800
C LEVEL 0
C  ITH IS CLUSTER THRESHOLD
C   IPHALG STEARS THE READOUT THRESHOLD IN MC EVENTS, DEFAULT NONZERO
      ITH = 25
      IPHALG = 1
      WRITE(6,1221) ITH,IPHALG
1221  FORMAT('  CLUSTER THRESHOLD, READOUT FLAG ',2I5)
C
      ISECLF = 3
      PTRUEO = 0.
      ICOSM = 0
C
      CALL HBOOK1( 1,'SEEN ENERGY/TRUE ENERGY, D1 $',100,0.,2.)
      CALL HBOOK1( 2,'CORR ENERGY/TRUE ENERGY, D1 $',100,0.,2.)
      CALL HBOOK1( 3,'SEEN ENERGY/TRUE ENERGY SHOWER, D1 $',100,0.,2.)
      CALL HBOOK1( 4,'CORR ENERGY/TRUE ENERGY SHOWER, D1 $',100,0.,2.)
      CALL HBOOK1( 5,'SEEN ENERGY/TRUE ENERGY NOSHOWER, D1 $',100,0.,2.)
      CALL HBOOK1( 6,'CORR ENERGY/TRUE ENERGY NOSHOWER, D1 $',100,0.,2.)
      CALL HBOOK1(11,'SEEN ENERGY/TRUE ENERGY, D2 $',100,0.,2.)
      CALL HBOOK1(12,'CORR ENERGY/TRUE ENERGY, D2 $',100,0.,2.)
      CALL HBOOK1(13,'SEEN ENERGY/TRUE ENERGY SHOWER, D2 $',100,0.,2.)
      CALL HBOOK1(14,'CORR ENERGY/TRUE ENERGY SHOWER, D2 $',100,0.,2.)
      CALL HBOOK1(15,'SEEN ENERGY/TRUE ENERGY NOSHOWER, D2 $',100,0.,2.)
      CALL HBOOK1(16,'CORR ENERGY/TRUE ENERGY NOSHOWER, D2 $',100,0.,2.)
      CALL HBOOK1(21,'SEEN ENERGY/TRUE ENERGY, D3 $',100,0.,2.)
      CALL HBOOK1(22,'CORR ENERGY/TRUE ENERGY, D3 $',100,0.,2.)
      CALL HBOOK1(23,'SEEN ENERGY/TRUE ENERGY SHOWER, D3 $',100,0.,2.)
      CALL HBOOK1(24,'CORR ENERGY/TRUE ENERGY SHOWER, D3 $',100,0.,2.)
      CALL HBOOK1(25,'SEEN ENERGY/TRUE ENERGY NOSHOWER, D3 $',100,0.,2.)
      CALL HBOOK1(26,'CORR ENERGY/TRUE ENERGY NOSHOWER, D3 $',100,0.,2.)
      CALL HBOOK1(31,'SEEN ENERGY/TRUE ENERGY, D4 $',100,0.,2.)
      CALL HBOOK1(32,'CORR ENERGY/TRUE ENERGY, D4 $',100,0.,2.)
      CALL HBOOK1(33,'SEEN ENERGY/TRUE ENERGY SHOWER, D4 $',100,0.,2.)
      CALL HBOOK1(34,'CORR ENERGY/TRUE ENERGY SHOWER, D4 $',100,0.,2.)
      CALL HBOOK1(35,'SEEN ENERGY/TRUE ENERGY NOSHOWER, D4 $',100,0.,2.)
      CALL HBOOK1(36,'CORR ENERGY/TRUE ENERGY NOSHOWER, D4 $',100,0.,2.)
      CALL HBOOK1(41,'SEEN ENERGY/TRUE ENERGY, D5 $',100,0.,2.)
      CALL HBOOK1(42,'CORR ENERGY/TRUE ENERGY, D5 $',100,0.,2.)
      CALL HBOOK1(43,'SEEN ENERGY/TRUE ENERGY SHOWER, D5 $',100,0.,2.)
      CALL HBOOK1(44,'CORR ENERGY/TRUE ENERGY SHOWER, D5 $',100,0.,2.)
      CALL HBOOK1(45,'SEEN ENERGY/TRUE ENERGY NOSHOWER, D5 $',100,0.,2.)
      CALL HBOOK1(46,'CORR ENERGY/TRUE ENERGY NOSHOWER, D5 $',100,0.,2.)
      CALL HBOOK1(51,'SEEN ENERGY/TRUE ENERGY, D6 $',100,0.,2.)
      CALL HBOOK1(52,'CORR ENERGY/TRUE ENERGY, D6 $',100,0.,2.)
      CALL HBOOK1(53,'SEEN ENERGY/TRUE ENERGY SHOWER, D6 $',100,0.,2.)
      CALL HBOOK1(54,'CORR ENERGY/TRUE ENERGY SHOWER, D6 $',100,0.,2.)
      CALL HBOOK1(55,'SEEN ENERGY/TRUE ENERGY NOSHOWER, D6 $',100,0.,2.)
      CALL HBOOK1(56,'CORR ENERGY/TRUE ENERGY NOSHOWER, D6 $',100,0.,2.)
      CALL HBOOK1(61,'SEEN ENERGY/TRUE ENERGY, D7 $',100,0.,2.)
      CALL HBOOK1(62,' CORR ENERGY/TRUE ENERGY, D7 $',100,0.,2.)
      CALL HBOOK1(63,'SEEN ENERGY/TRUE ENERGY SHOWER, D7 $',100,0.,2.)
      CALL HBOOK1(64,'CORR ENERGY/TRUE ENERGY SHOWER, D7 $',100,0.,2.)
      CALL HBOOK1(65,'SEEN ENERGY/TRUE ENERGY NOSHOWER, D7 $',100,0.,2.)
      CALL HBOOK1(66,'CORR ENERGY/TRUE ENERGY NOSHOWER, D7 $',100,0.,2.)
      CALL HBOOK1(71,' SEEN ENERGY/TRUE ENERGY, D8 $',100,0.,2.)
      CALL HBOOK1(72,' CORR ENERGY/TRUE ENERGY, D8 $',100,0.,2.)
      CALL HBOOK1(73,'SEEN ENERGY/TRUE ENERGY SHOWER, D8 $',100,0.,2.)
      CALL HBOOK1(74,'CORR ENERGY/TRUE ENERGY SHOWER, D8 $',100,0.,2.)
      CALL HBOOK1(75,'SEEN ENERGY/TRUE ENERGY NOSHOWER, D8 $',100,0.,2.)
      CALL HBOOK1(76,'CORR ENERGY/TRUE ENERGY NOSHOWER, D8 $',100,0.,2.)
      CALL HBOOK1(81,' SEEN ENERGY/TRUE ENERGY, D9 $',100,0.,2.)
      CALL HBOOK1(82,' CORR ENERGY/TRUE ENERGY, D9 $',100,0.,2.)
      CALL HBOOK1(83,'SEEN ENERGY/TRUE ENERGY SHOWER, D9 $',100,0.,2.)
      CALL HBOOK1(84,'CORR ENERGY/TRUE ENERGY SHOWER, D9 $',100,0.,2.)
      CALL HBOOK1(85,'SEEN ENERGY/TRUE ENERGY NOSHOWER, D9 $',100,0.,2.)
      CALL HBOOK1(86,'CORR ENERGY/TRUE ENERGY NOSHOWER, D9 $',100,0.,2.)
      CALL HBOOK1(91,' SEEN ENERGY/TRUE ENERGY, DA $',100,0.,2.)
      CALL HBOOK1(92,' CORR ENERGY/TRUE ENERGY, DA $',100,0.,2.)
      CALL HBOOK1(93,'SEEN ENERGY/TRUE ENERGY SHOWER, DA $',100,0.,2.)
      CALL HBOOK1(94,'CORR ENERGY/TRUE ENERGY SHOWER, DA $',100,0.,2.)
      CALL HBOOK1(95,'SEEN ENERGY/TRUE ENERGY NOSHOWER, DA $',100,0.,2.)
      CALL HBOOK1(96,'CORR ENERGY/TRUE ENERGY NOSHOWER, DA $',100,0.,2.)
      CALL HBOOK1(101,' SEEN ENERGY/TRUE ENERGY, DB $',100,0.,2.)
      CALL HBOOK1(102,' CORR ENERGY/TRUE ENERGY, DB $',100,0.,2.)
      CALL HBOOK1(103,'SEEN ENERGY/TRUE ENERGY SHOWER, DB $',100,0.,2.)
      CALL HBOOK1(104,'CORR ENERGY/TRUE ENERGY SHOWER, DB $',100,0.,2.)
      CALL HBOOK1(105,'SEEN ENERGY/TRUE ENERGY NOSHOW, DB $',100,0.,2.)
      CALL HBOOK1(106,'CORR ENERGY/TRUE ENERGY NOSHOW, DB $',100,0.,2.)
1223  CONTINUE
./ I 1300
C DECLARATIVE LEVEL
      COMMON /CLGPRM/ ITH,MAXCLS,IRLTHD,IRLTH2,IRLTH3
      COMMON / CRDSTA / IMCDUM(12),IPHALG
      COMMON / CTLIM / ISECLF
//*
// EXEC GEP,PARM.LKED='LIST,SIZE=(600K,60K)',REGION.LKED=800K,
//      REGION.GO=1600K,VS=HBOOK,
//      DN='F11OLS.UNIGAMGZ.E300.THR1.LGECORWT'
//LKED.SYSLIB DD
//            DD
//            DD
//            DD
//            DD
//            DD DISP=SHR,UNIT=,DSN=&&LOAD
//            DD DISP=SHR,DSN=F11OLS.JADE56.L
//            DD DISP=SHR,UNIT=FAST,DSN=F11GOD.PATRECLD
//            DD DISP=SHR,UNIT=FAST,DSN=F11LHO.JADEGL
//            DD DISP=SHR,UNIT=FAST,DSN=JADELG.LOAD
//            DD DISP=SHR,UNIT=FAST,DSN=F22ALL.JADEMUL
//            DD DISP=SHR,UNIT=FAST,DSN=R01UTL.HBOOK321.L
//            DD DISP=SHR,UNIT=FAST,DSN=R01UTL.CERN.KERNLIB4
//            DD DISP=SHR,UNIT=FAST,DSN=F1EBLO.BOSLIB.L
//LKED.SYSIN DD *
 INCLUDE SYSLIB(JDMAIN0)
 INCLUDE SYSLIB(USER0,LGECORWT)
//GO.FT22F001 DD UNIT=FAST,DISP=SHR,DSN=F11LHO.AUPDAT1
//*                        INPUT:
//*O.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM86.SUME400
//GO.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM87.E300D1
//            DD DISP=SHR,DSN=F11OLS.UNIGAM87.E300D2
//            DD DISP=SHR,DSN=F11OLS.UNIGAM87.E300D3
//            DD DISP=SHR,DSN=F11OLS.UNIGAM87.E300D4
//            DD DISP=SHR,DSN=F11OLS.UNIGAM87.E300D5
//            DD DISP=SHR,DSN=F11OLS.UNIGAM87.E300D6
//            DD DISP=SHR,DSN=F11OLS.UNIGAM87.E300D7
//            DD DISP=SHR,DSN=F11OLS.UNIGAM87.E300D8
//            DD DISP=SHR,DSN=F11OLS.UNIGAM87.E300D9
//            DD DISP=SHR,DSN=F11OLS.UNIGAM87.E300DA
//*                        OUTPUT:
//*GO.FT03F001 DD UNIT=DISK,DISP=(,PASS),
//*   DCB=(R01DCB.VBS,LRECL=6236),
//*       SPACE=(CYL,(220,20)),DSN=&&SCRTCH
//*GO.FT03F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*         MSVGP=VTMP,UNIT=3330V
//GO.FT03F001 DD DUMMY,DCB=R01DCB.VBS
//*GO.FT03F001 DD UNIT=FAST,DISP=SHR,DSN=F11OLS.RAETEST
//*GO.FT03F001 DD UNIT=FAST,DISP=(NEW,CATLG,DELETE),DCB=R01DCB.VBS,
//*   SPACE=(TRK,(10,2),RLSE),DSN=F11OLS.BARRIES.PROBLEM
//*GO.FT03F001 DD UNIT=TAPE,DISP=(NEW,CATLG,DELETE),
//*    DCB=(R01DCB.VBS,LRECL=6236,BLKSIZE=24948),
//*   DSN=F11OLS.REDUCTWO.G0216V00
//*
//*  COPY DISK OUTPUT TO TAPE
//*
//*COPY EXEC PGM=IEBGENER
//*SYSUT1 DD UNIT=DISK,DISP=(OLD,DELETE),DSN=&&SCRTCH
//*SYSUT2 DD UNIT=TAPE,DISP=(NEW,CATLG,DELETE),
//*          DCB=(R01DCB.VBS,LRECL=6236,BLKSIZE=24948),
//*           DSN=F11OLS.REDUCTWO.G0196V00
//*SYSPRINT DD SYSOUT=*
//*SYSIN DD DUMMY
C   28/03/97            MEMBER NAME  JBUNIGTH (JADE56.S)    FORTRAN
//F11OLS   JOB 00010221,CLASS=L,MSGLEVEL=(1,0),TIME=(9,00)
//*MAIN RELPRI=MED
//*
//*  FOR GIVEN ENERGY, PLOT VARIOUS DIRECTIONS
//*
// EXEC NEWLIB,PS='F11OLS.JADESR',PL='F11OLS.JADELD',
//  UNIT=FAST,LEVEL=8,OPT=3,FORT=XL
//MACRO DD DSN=F11GOD.PATRECSR,DISP=SHR,UNIT=FAST
./ C USER0
./ I 10200
C END OF JOB, AFTER PRINT OF IGGS,JIGGS AND BEFORE PLOTS
      WRITE(6,8881) PTRUE
8881  FORMAT(' TRUE ENERGY:',E12.4)
C
      DO 1217 IP = 1,20
      ZST = 0.
      DO 1216 I = 1,100
C
      XM = 0.
      XM2 = 0.
      SG2 = 0.
      IF(IN(I,IP).GT.0) XM = YN(I,IP)/FLOAT(IN(I,IP))
      IF(IN(I,IP).GT.0) XM2 = YN2(I,IP)/FLOAT(IN(I,IP))
      INM = IN(I,IP) - 1
      IF(IN(I,IP).GT.1)
     $ SG2 = (XM2-XM**2)/FLOAT(INM)
      IF(SG2.GT.0.) SG2 = SQRT(SG2)
      YMEAN(I,IP) = XM
      ERR(I,IP) = SG2
C
      ZST = ZST + DZMN
      CALL HFILL(40+IP,ZST,0,XM)
C
1216  CONTINUE
C
      CALL HPAKE(40+IP,ERR(1,IP))
1217  CONTINUE
C
./ I 7800
C LEVEL 7
      IPO = IDATA(IBLN('PATR'))
      IPALGN = IDATA(IBLN('ALGN'))
      IPLGCL = IDATA(IBLN('LGCL'))
      IPVECT = IDATA(IBLN('VECT'))
      IF(IPALGN.LE.0.OR.IPLGCL.LE.0) GO TO 105
      NCLST = IDATA(IPLGCL+7)
      NWPCL = IDATA(IPLGCL+25)
      JUMPCL = IDATA(IPLGCL+2)
C  4-VECTOR PHOTON ENERGY
      LO = IDATA(IPVECT+1)
      LTR = IDATA(IPVECT+2)
      NTRVEC = IDATA(IPVECT+4)
      IP4 = IPVECT + LO
      PTRUE = ADATA(IP4+4)
      COSTRU = ADATA(IP4+3)/PTRUE
      ZTRUE = 1100.*COSTRU/SQRT(1.-COSTRU*COSTRU)
      IF(JIGG(2).LT.10) WRITE(6,4661) ZTRUE
4661  FORMAT(' ZTRUE ',E12.4)
C
      GO TO 999
105   WRITE(6,101) IPALGN,IPLGCL
101   FORMAT(' **** WARNING NO ALGN/LGCL BANKS,  POINTERS :',2I8)
      GO TO 1
999   NMOMGM = 0
      IF(NCLST.LE.0) GO TO 1027
      IP = IDATA(IPLGCL+3) + IPLGCL - 1 - NWPCL
      ITP = 0
      DO 522  IGM = 1,NCLST
      IP = IP + NWPCL
      CALL TRACK(IP,ITP,RMIN,NHT,FI,THE,PTOT,PT,PL,PX,PY,PZ)
      IF(IDATA(IP+8).NE.0) GO TO 522
      NMOMGM = NMOMGM + 1
      PTOTGM = PTOT
      PSEEN = ADATA(IP+NWPCL)
      COSTHG = COS(THE)
522   CONTINUE
1027  CONTINUE
      IF(NMOMGM.NE.1) GO TO 1
C
C  COMPARE PSEEN WITH TOTAL ENERGY IN LEADGLASS
C
      ETOT = 0.
      IPJ=IDATA(IBLN('ALGN'))
      NWO=IDATA(IPJ)
      IF(NWO.LE.3) GO TO 1
      IPJ=2*IPJ + 8
      NWO=IPJ+2*NWO-8
      DO 5004 IJK=IPJ,NWO,2
      IAD=HDATA(IJK-1)
      NAMP = HDATA(IJK)
      ETOT=ETOT+FLOAT(NAMP)*.001
C
C     WRITE(6,2243) IJK,IAD,NAMP,ETOT
C243  FORMAT(' 5004: IJK IAD NAMP ETOT ',3I6,3E12.4)
C
5004  CONTINUE
C
      DELTAS = ETOT - PSEEN
      IF(DELTAS.NE.0.) CALL HFILL(1,DELTAS,0,1.)
C
      IPHALG = 1
      CALL RDALGN
      IPHALG = 0
C
C  GET NEW ETOT
C
      ETOT1 = 0.
      IPJ=IDATA(IBLN('ALGN'))
      NWO=IDATA(IPJ)
      IF(NWO.LE.3) GO TO 1
      IPJ=2*IPJ + 8
      NWO=IPJ+2*NWO-8
      DO 5005 IJK=IPJ,NWO,2
      IAD=HDATA(IJK-1)
      NAMP = HDATA(IJK)
      ETOT1=ETOT1+FLOAT(NAMP)*.001
C
C     WRITE(6,2244) IJK,IAD,NAMP,ETOT1
C244  FORMAT(' 5005: IJK IAD NAMP ETOT1 ',3I6,3E12.4)
C
5005  CONTINUE
C
      DELTAE = ETOT - ETOT1
C
C     WRITE(6,2233) ETOT,ETOT1,DELTAE
C233  FORMAT(' AFTER RDALGN, ETOT ETOT1 DELTAE ',3E12.4)
C
C
C   NEW CLUSTER ANALYSIS AND LGCL RECONSTRUCTION
C
C RDALGN DELETES ALSO LGCL
C     CALL BDLS('LGCL',1)
C
      CALL LGANAL
      IPPATR = IDATA(IBLN('PATR'))
      IPALGN = IDATA(IBLN('ALGN'))
      IPLGCL = IDATA(IBLN('LGCL'))
C
C     WRITE(6,2234) IPPATR,IPALGN,IPLGCL
C234  FORMAT(' AFTER LGANAL, IPPATR,ALGN LGCL ',3I10)
C
      IF(IPPATR.LE.0.OR.IPALGN.LE.0.OR.IPLGCL.LE.0) GO TO 1
      CALL LGCDIR(IPPATR,IPALGN,IPLGCL)
C
C  NEW PHOTON FINDING
C
      IPLGCL = IDATA(IBLN('LGCL'))
C
C     WRITE(6,2235) IPLGCL
C235  FORMAT(' AFTER LGCDIR IPLGCL ',3I10)
C
      NCLST = IDATA(IPLGCL+7)
      IF(NCLST.LE.0) GO TO 1
      NWPCL = IDATA(IPLGCL+25)
      JUMPCL = IDATA(IPLGCL+2)
      NMOMGM = 0
      IP = IDATA(IPLGCL+3) + IPLGCL - 1 - NWPCL
      ITP = 0
      DO 422  IGM = 1,NCLST
      IP = IP + NWPCL
      CALL TRACK(IP,ITP,RMIN,NHT,FI,THE,PTOT,PT,PL,PX,PY,PZ)
      IF(IDATA(IP+8).NE.0) GO TO 422
      NMOMGM = NMOMGM + 1
      PTOTG1 = PTOT
      PSEEN1= ADATA(IP+NWPCL)
      COSTG1 = COS(THE)
422   CONTINUE
      IF(NMOMGM.NE.1) GO TO 1
C
      CALL HFILL(2,PSEEN1,PSEEN,1.)
C
C ONE PHOTON FOUND AGAIN, AFTER READOUT SUPRESSION
C
      NSF56 = 0
      LSF56 = 0
      IPSF56 = IDATA(IBLN('SF56'))
      IF(IPSF56.LE.0) GO TO 3880
      NSF56 = IDATA(IPSF56+1)
      LSF56 = NSF56
      IF(NSF56.GT.10) LSF56 = 10
3880  CONTINUE
C
      ZST = 0.
C
      DO 1333  I = 1,100
      ZST = ZST + DZMN
      IF(ZTRUE.GT.ZST) GO TO 1333
C
      IP = PSEEN1/.05 + 1
      IF(IP.GT.20) IP = 20
C
      IN(I,IP) = IN(I,IP) + 1
      YN(I,IP) = YN(I,IP) + DELTAE
      YN2(I,IP) = YN2(I,IP) + DELTAE**2
      CALL HFILL(10+IP,ZTRUE,DELTAE,1.)
      GO TO 1334
C
1333  CONTINUE
C
1334  CONTINUE
C
      GO TO 11
./ I 7400
C LEVEL 6
./ I 6800
C LEVEL 5
./ I 6200
C LEVEL 4
./ I 5600
C LEVEL 3
./ I 5000
C LEVEL 2
      IF((JIGG(2)/100)*100.EQ.JIGG(2))
     $ WRITE(6,2702) JIGG(2),HEAD(18),HEAD(19)
2702  FORMAT(' NNREC RUN EVENT ',3I8)
./ I 4400
C LEVEL 1
./ I 3800
C LEVEL 0
C  ITH IS CLUSTER THRESHOLD
C   IPHALG STEARS THE READOUT THRESHOLD IN MC EVENTS, DEFAULT NONZERO
      ITH = 25
      IPHALG = 0
      WRITE(6,1221) ITH,IPHALG
1221  FORMAT('  CLUSTER THRESHOLD, READOUT FLAG ',2I5)
C
      ISECLF = 3
      PTRUEO = 0.
C
      DZMN = 15.
C
      DO 1077 K = 1,20
      DO 1077 I = 1,100
      IN(I,K) = 0
      ERR(I,K) = 0.
      YN(I,K) = 0.
      YN2(I,K) = 0.
      YMEAN(I,K) = 0.
1077  CONTINUE
C
      CALL ACORE(40000)
C
      CALL HBOOK1( 1,'DELTA PSEEN - ETOT $',100,0.,.05)
      CALL HBOOK2( 2,'PSEEN VS PSEEN1 $',100,0.,1.,100,0.,1.)
C
      CALL HBOOK1(41,'MEAN50 VS Z $',100,0.,1500.)
      CALL HBOOK1(42,'MEAN100 VS Z $',100,0.,1500.)
      CALL HBOOK1(43,'MEAN150 VS Z $',100,0.,1500.)
      CALL HBOOK1(44,'MEAN200 VS Z $',100,0.,1500.)
      CALL HBOOK1(45,'MEAN250 VS Z $',100,0.,1500.)
      CALL HBOOK1(46,'MEAN300 VS Z $',100,0.,1500.)
      CALL HBOOK1(47,'MEAN350 VS Z $',100,0.,1500.)
      CALL HBOOK1(48,'MEAN400 VS Z $',100,0.,1500.)
      CALL HBOOK1(49,'MEAN450 VS Z $',100,0.,1500.)
      CALL HBOOK1(50,'MEAN500 VS Z $',100,0.,1500.)
      CALL HBOOK1(51,'MEAN550 VS Z $',100,0.,1500.)
      CALL HBOOK1(52,'MEAN600 VS Z $',100,0.,1500.)
      CALL HBOOK1(53,'MEAN650 VS Z $',100,0.,1500.)
      CALL HBOOK1(54,'MEAN700 VS Z $',100,0.,1500.)
      CALL HBOOK1(55,'MEAN750 VS Z $',100,0.,1500.)
      CALL HBOOK1(56,'MEAN800 VS Z $',100,0.,1500.)
      CALL HBOOK1(57,'MEAN850 VS Z $',100,0.,1500.)
      CALL HBOOK1(58,'MEAN900 VS Z $',100,0.,1500.)
      CALL HBOOK1(59,'MEAN950 VS Z $',100,0.,1500.)
      CALL HBOOK1(60,'MEAN1000 VS Z $',100,0.,1500.)
C
      CALL HBOOK2(11,'DELTASUPR VS Z, 50$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(12,'DELTASUPR VS Z, 100$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(13,'DELTASUPR VS Z, 150$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(14,'DELTASUPR VS Z, 200$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(15,'DELTASUPR VS Z, 250$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(16,'DELTASUPR VS Z, 300$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(17,'DELTASUPR VS Z, 350$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(18,'DELTASUPR VS Z, 400$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(19,'DELTASUPR VS Z, 450$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(20,'DELTASUPR VS Z, 500$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(21,'DELTASUPR VS Z, 550$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(22,'DELTASUPR VS Z, 600$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(23,'DELTASUPR VS Z, 650$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(24,'DELTASUPR VS Z, 700$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(25,'DELTASUPR VS Z, 750$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(26,'DELTASUPR VS Z, 800$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(27,'DELTASUPR VS Z, 850$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(28,'DELTASUPR VS Z, 900$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(29,'DELTASUPR VS Z, 950$',100,0.,1500.,100,0.,0.5)
      CALL HBOOK2(30,'DELTASUPR VS Z,1000$',100,0.,1500.,100,0.,0.5)
1223  CONTINUE
./ I 1300
C DECLARATIVE LEVEL
      COMMON /CLGPRM/ ITH,MAXCLS,IRLTHD,IRLTH2,IRLTH3
      COMMON / CRDSTA / IMCDUM(12),IPHALG
      COMMON / CTLIM / ISECLF
C
      DIMENSION ERR(100,20)
      DIMENSION YN(100,20)
      DIMENSION YMEAN(100,20)
      DIMENSION YN2(100,20)
      DIMENSION IN(100,20)
C
//*
// EXEC GEP,PARM.LKED='LIST,SIZE=(600K,60K)',REGION.LKED=800K,
//      REGION.GO=1800K,VS=HBOOK,
//      DN='F11OLS.UNIGAMTH.EF300'
//LKED.SYSLIB DD
//            DD
//            DD
//            DD
//            DD
//            DD DISP=SHR,UNIT=,DSN=&&LOAD
//            DD DISP=SHR,DSN=F11OLS.JADE56.L
//            DD DISP=SHR,UNIT=FAST,DSN=F11GOD.PATRECLD
//            DD DISP=SHR,UNIT=FAST,DSN=F11LHO.JADEGL
//            DD DISP=SHR,UNIT=FAST,DSN=JADELG.LOAD
//            DD DISP=SHR,UNIT=FAST,DSN=F22ALL.JADEMUL
//            DD DISP=SHR,UNIT=FAST,DSN=R01UTL.HBOOK321.L
//            DD DISP=SHR,UNIT=FAST,DSN=R01UTL.CERN.KERNLIB4
//            DD DISP=SHR,UNIT=FAST,DSN=F1EBLO.BOSLIB.L
//LKED.SYSIN DD *
 INCLUDE SYSLIB(JDMAIN0)
 INCLUDE SYSLIB(USER0,LGECORWT)
//* INCLUDE SYSLIB(ENGLOSM)
//* INCLUDE SYSLIB(LGECORM)
//GO.FT22F001 DD UNIT=FAST,DISP=SHR,DSN=F11LHO.AUPDAT1
//*                        INPUT:
//*O.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUMEA00
//*           DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUMFA00
//*O.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUME800
//*           DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUMF800
//*O.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUME700
//*           DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUMF700
//*O.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUME600
//*           DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUMF600
//*O.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUME500
//*           DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUMF500
//*O.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUME400
//*           DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUMF400
//GO.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUME300
//            DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUMF300
//*O.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUME200A
//*           DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUME200B
//*           DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUMF200A
//*           DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUMF200B
//*O.FT02F001 DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUME100A
//*           DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUME100B
//*           DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUMF100A
//*           DD DISP=SHR,DSN=F11OLS.UNIGAM87.SUMF100B
//*                        OUTPUT:
//*GO.FT03F001 DD UNIT=DISK,DISP=(,PASS),
//*   DCB=(R01DCB.VBS,LRECL=6236),
//*       SPACE=(CYL,(220,20)),DSN=&&SCRTCH
//*GO.FT03F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*         MSVGP=VTMP,UNIT=3330V
//GO.FT03F001 DD DUMMY,DCB=R01DCB.VBS
//*GO.FT03F001 DD UNIT=FAST,DISP=SHR,DSN=F11OLS.RAETEST
//*GO.FT03F001 DD UNIT=FAST,DISP=(NEW,CATLG,DELETE),DCB=R01DCB.VBS,
//*   SPACE=(TRK,(10,2),RLSE),DSN=F11OLS.BARRIES.PROBLEM
//*GO.FT03F001 DD UNIT=TAPE,DISP=(NEW,CATLG,DELETE),
//*    DCB=(R01DCB.VBS,LRECL=6236,BLKSIZE=24948),
//*   DSN=F11OLS.REDUCTWO.G0216V00
//*
//*  COPY DISK OUTPUT TO TAPE
//*
//*COPY EXEC PGM=IEBGENER
//*SYSUT1 DD UNIT=DISK,DISP=(OLD,DELETE),DSN=&&SCRTCH
//*SYSUT2 DD UNIT=TAPE,DISP=(NEW,CATLG,DELETE),
//*          DCB=(R01DCB.VBS,LRECL=6236,BLKSIZE=24948),
//*           DSN=F11OLS.REDUCTWO.G0196V00
//*SYSPRINT DD SYSOUT=*
//*SYSIN DD DUMMY
C   28/03/97            MEMBER NAME  JBMWFS   (JADE56.S)    FORTRAN
//F11OLS   JOB CLASS=E,MSGLEVEL=(1,0),TIME=(0,15)
//*MAIN RELPRI=LOW
// EXEC FCLG,PARM.FORT=XL
C   READ FAST FILE AND WRITE IT OUT AGAIN
C    LTYP HAS TO BE SPECIFIED ************
C                             J.OLSSON   12.3.81
C   LAST CHANGE  24.02.83
C
      IMPLICIT INTEGER*2 (H)
#include "creso.for"
#include "cintrg.for"
#include "ctrgmn.for"
#include "cextra.for"
      DIMENSION IWRIT(999),WRIT(999),HWRIT(1998)
      EQUIVALENCE (IWRIT(1),WRIT(1)),(HWRIT(1),IWRIT(1))
      COMMON/CWORK1/IER,NTRR,TRES(10,60)
      DIMENSION ITRES(10,60),CWARR(602)
      EQUIVALENCE (TRES(1,1),ITRES(1,1)),(CWARR(1),IER)
      COMMON/CWORK/TOFDUM(984),INFM(4),IRELT(14,50)
      DIMENSION IW(704),RELT(14,50),HELP(2)
      EQUIVALENCE (IW(1),INFM(1))
      EQUIVALENCE (RELT(1,1),IRELT(1,1)),(IHELP,HELP(1))
      COMMON /COSNEU/ ISWAP,HQUAL(2),MERR
      DATA ITALC /Z7/,HELP/0,0/
C---
      IEVCN = 0
      IVRIG = 0
      LTYP = 22
      IEVMIN = 0
      IEVMAX = 27000
100   READ(2,ERR=1005,END=90) KKK,(WRIT(I),I=1,KKK)
C FILL THE ARRAYS
1213  IF(IUHR(ISECLF).NE.1) GO TO 1212
      IEVCN = IEVCN + 1
      IF((IEVCN/500)*500.EQ.IEVCN) WRITE(6,3411) IEVCN
3411  FORMAT('  RECORDS READ ',I8)
      IF(IEVCN.LT.IEVMIN) GO TO 100
      IF(IEVCN.GT.IEVMAX) GO TO 9090
      GO TO 1214
1212  IF(IUHR(ISECLF).EQ.1) GO TO 1213
      WRITE(6,3554)
3554  FORMAT(' ********** ------>>>>> *****')
      WRITE(6,3555) IEVCN,IVRIG
3555  FORMAT(' TIME LIMIT TIME LIMIT   RECORDS READ ',I8,'WRITTEN ',I8)
      WRITE(6,3554)
      GO TO 9090
C
1214  IF(IEVCN.GT.5) GO TO 3000
      IF(LTYP.LT.10) GO TO 1900
      HRUN = HWRIT(1)
      HEVENT = HWRIT(2)
      NCLST = IWRIT(2)
      NTR = IWRIT(3)
      NMOM = IWRIT(4)
      NMOMTR = IWRIT(5)
      NMTR1 = NMOMTR + 1
      NMOMGM = IWRIT(6)
      NMOMBD = IWRIT(7)
      ICOL = IWRIT(8)
      IMDEAD = IWRIT(9)
      IMIDTG = IWRIT(10)
      IMGCLO = IWRIT(11)
      IER = IWRIT(12)
      IFTG = IWRIT(13)
      HELPTR = HWRIT(27)
      HTALC = HWRIT(28)
      JELL = IWRIT(15)
      IVX = IWRIT(16)
      NRVVX = IWRIT(17)
      ETOT = WRIT(18)
      TDIF = WRIT(19)
      TOFSUM = WRIT(20)
      EBM = WRIT(21)
      ZETSUM = WRIT(22)
      ZGRZ = WRIT(23)
      COSEPL = WRIT(24)
      VXX = WRIT(25)
      VXY = WRIT(26)
      VXZ = WRIT(27)
      BKG = WRIT(28)
      HNBLO = HWRIT(57)
      III = 30
      DO 5123  I = 1,NMOM
      XMAS(I) = WRIT(III+I)
      PETRA(I) = WRIT(III+NMOM+I)
      PETOT(I) = WRIT(III+2*NMOM+I)
      FITRA(I) = WRIT(III+3*NMOM+I)
5123  TETRA(I) = WRIT(III+4*NMOM+I)
      III = III + 5*NMOM - 4
      DO 5124  I = 1,NMOM
      III = III + 4
      DO 5125  J = 1,4
5125  PAR(I,J) = WRIT(III+J)
5124  CONTINUE
      DO 5126  I = 1,NMOM
      III = III + 4
      DO 5127  J = 1,4
5127  PARCOR(I,J) = WRIT(III+J)
5126  CONTINUE
      III = III + 4
      DO 5223  I = 1,NMOMTR
      KKKTR(I) = IWRIT(III+I)
      RMN(I) = WRIT(III+NMOMTR+I)
      ZTRCK(I) = WRIT(III+2*NMOMTR+I)
      COSTR(I) = WRIT(III+3*NMOMTR+I)
      SGN(I) = WRIT(III+4*NMOMTR+I)
      ARPHIX(I) = WRIT(III+5*NMOMTR+I)
      SLOPES(I) = WRIT(III+6*NMOMTR+I)
      CRVE(I) = WRIT(III+7*NMOMTR+I)
      NRHIT(I) = IWRIT(III+8*NMOMTR+I)
5223  ITYPP(I) = IWRIT(III+9*NMOMTR+I)
      III = III + 10*NMOMTR
      DO 5741  I = 1,NMTR1
      PCHA(I) = WRIT(III+I)
5741  ECHA(I) = WRIT(III+NMTR1+I)
      III = III + 2*NMTR1 - 10
      DO 5232 J = 1,NTR
      III = III + 10
      DO 5278 I = 1,10
5278  TRES(I,J) = WRIT(III+I)
5232  CONTINUE
      III = III + 10 - 14
      DO 5233 J = 1,NTR
      III = III + 14
      DO 5279 I = 1,14
5279  RELT(I,J) = WRIT(III+I)
5233  CONTINUE
      III = III + 14 - 10
      DO 5234  I = 1,NMOMTR
      III = III + 10
      DO 5235  J = 1,10
5235  PARM(J,I) = WRIT(III + J)
5234  CONTINUE
      III = III + 10 - 4
      DO 5236  I = 1,NMOMTR
      III = III + 4
      DO 5237  J = 1,4
5237  PARM2(J,I) = WRIT(III + J)
5236  CONTINUE
      IM = HNBLO
      IF(IM.LE.0) IM = 1
      III = III + 4
      IIIH = 2*III
      DO 5339  I = 1,IM
5339  HBLOCK(I) = HWRIT(IIIH + I)
      IIIH = IIIH + IM
      III = (IIIH+1)/2
      DO 5340  I = 1,IM
5340  EBLOCK(I) = WRIT(III+I)
      III = III + IM
      III = III + 1
      NRBTOF = IWRIT(III)
      IF(NRBTOF.EQ.0) GO TO 1281
      DO 1275  JJ = 1,NRBTOF
      JJJ = 2*III + JJ
      HNTOF(JJ) = HWRIT(JJJ)
1275  CONTINUE
      III = III + (NRBTOF-1)/2 + 1
1281  III = III + 1
      NLOWHT = IWRIT(III)
      III = III + 1
      NLOWP = IWRIT(III)
      IF(NMOMGM.EQ.0) GO TO 999
C *******************************************************
C      EVENTS WITH PHOTONS IN ADDITION TO CHARGED TRACKS
C *******************************************************
C FILL PHOTON ARRAYS
      DO 5423  I = 1,NMOMGM
      KGGTR(I) = IWRIT(III+I)
5423  DPETOT(I) = WRIT(III+NMOMGM+I)
      III = III + 2*NMOMGM
      IIIH = 2*III
      DO 5431  I = 1,NMOMGM
      HGMTYP(I) = HWRIT(IIIH+I)
5431  HIBLOC(I) = HWRIT(IIIH + NMOMGM + I)
      IIIH = IIIH + 2*NMOMGM
      III = (IIIH+1)/2
      GO TO 999
C *******************************************************
C      EVENTS WITH ONLY PHOTONS
C *******************************************************
1900  CONTINUE
C ENTER HERE FOR EVENTS WITH ONLY PHOTONS
      HRUN = HWRIT(1)
      HEVENT = HWRIT(2)
      NCLST = IWRIT(2)
      NTR = IWRIT(3)
      NMOM = IWRIT(4)
      NMOMTR = IWRIT(5)
      NMTR1 = NMOMTR + 1
      NMOMGM = IWRIT(6)
      NMOMBD = IWRIT(7)
      ICOL = IWRIT(8)
      IMDEAD = IWRIT(9)
      IMIDTG = IWRIT(10)
      IMGCLO = IWRIT(11)
      IER = IWRIT(12)
      IFTG = IWRIT(13)
      HELPTR = HWRIT(27)
      HTALC = HWRIT(28)
      JELL = IWRIT(15)
      IVX = IWRIT(16)
      NRVVX = IWRIT(17)
      ETOT = WRIT(18)
      TDIF = WRIT(19)
      TOFSUM = WRIT(20)
      EBM = WRIT(21)
      ZETSUM = WRIT(22)
      ZGRZ = WRIT(23)
      COSEPL = WRIT(24)
      VXX = WRIT(25)
      VXY = WRIT(26)
      VXZ = WRIT(27)
      BKG = WRIT(28)
      HNBLO = HWRIT(57)
      III = 30
      DO 7123  I = 1,NMOM
      XMAS(I) = WRIT(III+I)
      PETRA(I) = WRIT(III+NMOM+I)
      PETOT(I) = WRIT(III+2*NMOM+I)
      FITRA(I) = WRIT(III+3*NMOM+I)
7123  TETRA(I) = WRIT(III+4*NMOM+I)
      III = III + 5*NMOM - 4
      DO 7124  I = 1,NMOM
      III = III + 4
      DO 7125  J = 1,4
7125  PAR(I,J) = WRIT(III+J)
7124  CONTINUE
      DO 7126  I = 1,NMOM
      III = III + 4
      DO 7127  J = 1,4
7127  PARCOR(I,J) = WRIT(III+J)
7126  CONTINUE
      III = III + 4
      DO 7741  I = 1,NMTR1
      PCHA(I) = WRIT(III+I)
7741  ECHA(I) = WRIT(III+NMTR1+I)
      III = III + 2*NMTR1
      IF(NTR.LE.0) GO TO 7771
      DO 7232 J = 1,NTR
      DO 7278 I = 1,10
7278  TRES(I,J) = WRIT(III+I)
      III = III + 10
7232  CONTINUE
      DO 7233 J = 1,NTR
      DO 7279 I = 1,14
7279  RELT(I,J) = WRIT(III+I)
      III = III + 14
7233  CONTINUE
7771  IM = HNBLO
      IF(IM.LE.0) IM = 1
      IIIH = 2*III
      DO 7339  I = 1,IM
7339  HBLOCK(I) = HWRIT(IIIH + I)
      IIIH = IIIH + IM
      III = (IIIH+1)/2
      DO 7340  I = 1,IM
7340  EBLOCK(I) = WRIT(III+I)
      III = III + IM
      III = III + 1
      NRBTOF = IWRIT(III)
      IF(NRBTOF.EQ.0) GO TO 7281
      DO 7275  JJ = 1,NRBTOF
      JJJ = 2*III + JJ
      HNTOF(JJ) = HWRIT(JJJ)
7275  CONTINUE
      III = III + (NRBTOF-1)/2 + 1
7281  III = III + 1
      NLOWHT = IWRIT(III)
      III = III + 1
      NLOWP = IWRIT(III)
C FILL PHOTON ARRAYS
      IF(IENT.LT.3) WRITE(6,2636) NMOMGM,III
2636  FORMAT(' ENTRY 2636 NMOMGM III ',3I8)
      DO 7423  I = 1,NMOMGM
      KGGTR(I) = IWRIT(III+I)
7423  DPETOT(I) = WRIT(III+NMOMGM+I)
      III = III + 2*NMOMGM
      IIIH = 2*III
      DO 7431  I = 1,NMOMGM
      HGMTYP(I) = HWRIT(IIIH+I)
7431  HIBLOC(I) = HWRIT(IIIH + NMOMGM + I)
      IIIH = IIIH + 2*NMOMGM
      III = (IIIH+1)/2
      III = III + 1
      IDHITS = IWRIT(III)
      III = III + 1
      MUTREF = IWRIT(III)
C *******************************************************
C  END OF EVENTS WITH ONLY PHOTONS
C  COME HERE FOR ALL TYPES OF EVENTS, BRANCH ON MONTE CARLO TYPE
C *******************************************************
999   NTRR = NTR
      INFM(1) = NTR
      IF(HRUN.GT.99) GO TO 1111
C**********************************************************
C SPECIAL ENTRY FOR MC EVENTS
C***********************************************************
      DO 1471  I = 1,6
1471  DDUM(I) = 0.
      DO 1472  I = 1,12
1472  DDUN(I) = 0.
      IF(III.GE.KKK) GO TO 1111
      III = III + 1
C MASS WWW
      DDUM(1) = WRIT(III)
      IF(III.GE.KKK) GO TO 1111
      III = III + 1
C TOF WIDTH IWI
      JDUM(2) = IWRIT(III)
      IF(III.GE.KKK) GO TO 1111
C
C   LTYP = 21  MONTE CARLO
C
      IF(LTYP.NE.21)  GO TO 997
      III = III + 1
C ENERGY GAMMA1
      DDUM(3) = WRIT(III)
      IF(III.GE.KKK) GO TO 1111
      III = III + 1
C BLOCK NR1 GAMMA1
      JDUM(4) = IWRIT(III)
      GO TO 1111
C
C   LTYP = 22 AND LTYP = 2     MONTE CARLO
C
997   IF(LTYP.NE.22.AND.LTYP.NE.2)  GO TO 886
      III = III + 1
C ENERGY GAMMA1
      DDUM(3) = WRIT(III)
      IF(III.GE.KKK) GO TO 1111
      III = III + 1
C ENERGY GAMMA2
      DDUM(4) = WRIT(III)
      IF(III.GE.KKK) GO TO 1111
      III = III + 1
C BLOCK NR1 GAMMA1
      JDUM(5) = IWRIT(III)
      IF(III.GE.KKK) GO TO 1111
      III = III + 1
C BLOCK NR1 GAMMA1
      JDUM(6) = IWRIT(III)
      GO TO 1111
C
C   LTYP = 4  MONTE CARLO
C
886   IF(LTYP.NE.4)  GO TO 885
      III = III + 1
C ENERGY GAMMA1
      DDUM(3) = WRIT(III)
      IF(III.GE.KKK) GO TO 1111
      III = III + 1
C ENERGY GAMMA2
      DDUM(4) = WRIT(III)
      IF(III.GE.KKK) GO TO 1111
      III = III + 1
C ENERGY GAMMA3
      DDUM(5) = WRIT(III)
      IF(III.GE.KKK) GO TO 1111
      III = III + 1
C ENERGY GAMMA4
      DDUM(6) = WRIT(III)
      IF(III.GE.KKK) GO TO 1111
      III = III + 1
C PHI GAMMA1
      DDUN(1) = WRIT(III)
      IF(III.GE.KKK) GO TO 1111
      III = III + 1
C PHI GAMMA2
      DDUN(2) = WRIT(III)
      IF(III.GE.KKK) GO TO 1111
      III = III + 1
C PHI GAMMA3
      DDUN(3) = WRIT(III)
      IF(III.GE.KKK) GO TO 1111
      III = III + 1
C PHI GAMMA4
      DDUN(4) = WRIT(III)
      IF(III.GE.KKK) GO TO 1111
      III = III + 1
C BLOCK NR1 GAMMA1
      JDUM(11) = IWRIT(III)
      IF(III.GE.KKK) GO TO 1111
      III = III + 1
C BLOCK NR1 GAMMA2
      JDUM(12) = IWRIT(III)
      IF(III.GE.KKK) GO TO 1111
      III = III + 1
C BLOCK NR1 GAMMA3
      JDUM(13) = IWRIT(III)
      IF(III.GE.KKK) GO TO 1111
      III = III + 1
C BLOCK NR1 GAMMA4
      JDUM(14) = IWRIT(III)
      IF(III.GE.KKK) GO TO 1111
      GO TO 1111
885   CONTINUE
C
C**********************************************************
C  END OF SECTION FOR MC EVENTS
C  COME HERE FOR ALL TYPES OF EVENTS, BRANCH ON ITYP82
C********************************************************
C
1111  ITYP82 = 0
      IF(III.EQ.KKK) GO TO 3000
      III = III + 1
      ITYP82 = IWRIT(III)
      IF(ITYP82.EQ.0) GO TO 2802
      IIIH = 2*III + 1
      HLPTR2 = HWRIT(IIIH)
      III = (IIIH+1)/2
      IF(III.GE.KKK) GO TO 2801
      III = III + 1
      LGFLA = IWRIT(III)
      III = III + 1
      BLGTOF = WRIT(III)
      III = III + 1
      IBADC = IWRIT(III)
2802  IF(III.GE.KKK) GO TO 2801
      DO 2783  I = 1,7
      III = III + 1
      L7(I) = IWRIT(III)
2783  CONTINUE
2801  IF(LTYP.NE.2) GO TO 2812
      IF(III.GE.KKK) GO TO 2812
      IIIH = 2*III+1
      HQUAL(1) = HWRIT(IIIH)
      HQUAL(2) = HWRIT(IIIH+1)
      ISWAP = HWRIT(IIIH+2)
      MERR = HWRIT(IIIH+3)
      III = (IIIH+4)/2
      IF(III.GE.KKK) GO TO 2812
      III = III+1
      NRFWMU = IWRIT(III)
2812  CONTINUE
C  VEC4MC
      IF(HRUN.GT.99) GO TO 3000
      IF(III.GE.KKK) GO TO 3000
      III = III + 1
      MCNFIN = IWRIT(III)
      DO 6031  I = 1,MCNFIN
      DO 6032  J = 1,4
6032  VEC4MC(J,I) = WRIT(III+J)
      III = III + 4
6031  CONTINUE
      IF(III.GE.KKK) GO TO 3000
      IIIH = 2*III
      DO 7576  IJ = 1,42
      IIIH = IIIH + 1
7576  HLGBR(IJ) = HWRIT(IIIH)
      III = IIIH/2
      WRITE (6,2531)
2531  FORMAT(' - - - - - - - - - > > > > > < < < < < - - - - -')
      WRITE(6,73) HRUN,HEVENT,HTALC,HELPTR,IFTG,ITYP82,EBM
73    FORMAT(' WFS: RUN & EV ',2I8,' HTALC HELPTR IFTG ',Z4,1X,Z4,I5,
     $' ITYP82 ',I2,' EBM ',E13.5)
      IF(ITYP82.NE.0) WRITE(6,2532) HLPTR2,LGFLA,IBADC,BLGTOF
2532  FORMAT(' HLPTR2 (T1POSTP.) ',Z4,' LGFLA IBADC BLGTOF ',2I5,F8.1)
      WRITE(6,2582) L7
2582  FORMAT(' LG SEPTANT ENERGIES ',7I5)
      WRITE(6,74)
      WRITE(6,75) KKK,NMOM,NMOMTR,NMOMGM,NMOMBD,ICOL,JELL,
     $ ZETSUM,ZGRZ,COSEPL,NCLST,BKG
74    FORMAT('  KKK  NMOM NMOMTR NMOMGM NMOMBD ICOL JELL    ZETSUM
     $ZGRZ    COSEPL   NCLST    BKG')
75    FORMAT(' ',I5,I5,I7,I7,I7,I5,I5,F10.2,F10.2,F10.4,I5,F10.4)
      WRITE(6,45) IMDEAD,IMIDTG,IMGCLO,NTR,NLOWHT,NLOWP
45    FORMAT(' IMDEAD IMIDTG IMGCLO ',3I5,' NTR ',I4,' NLOWHT NLOWP ',
     $ 2I4)
      WRITE(6,76) ((FITRA(I),TETRA(I),XMAS(I)),I=1,NMOM)
76    FORMAT(' FITRA TETRA XMAS',4(3F8.4,3X))
      WRITE(6,77) ((PETRA(I),PETOT(I)),I=1,NMOM)
77    FORMAT(' PETRA PETOT ',4(2E12.4,3X))
      IF(NMOMTR.EQ.0) GO TO 991
      WRITE(6,78) ((ZTRCK(I),RMN(I)),I=1,NMOMTR)
78    FORMAT(' ZTRCK RMN ',4(2E12.4,3X))
      WRITE(6,79) ((COSTR(I),ARPHIX(I),KKKTR(I)),I=1,NMOMTR)
79    FORMAT(' COSTR ARPHIX KKKTR',4(2F8.4,I4,3X))
      WRITE(6,80) ((SLOPES(I),SGN(I)),I=1,NMOMTR)
80    FORMAT(' SLOPES SIGN ',4(E12.4,F5.0,3X))
      WRITE(6,40) ((NRHIT(I),ITYPP(I),CRVE(I)),I=1,NMOMTR)
40    FORMAT(' NRHIT ITYP CURV ',4(2I3,E15.6,3X))
      WRITE(6,81) ((PCHA(I),ECHA(I)),I=1,NMTR1)
81    FORMAT(' PCHA ECHA',5(2E11.3,1X))
991   WRITE(6,82) ((PAR(I,J),J=1,4),I=1,NMOM)
82    FORMAT(' PAR ',2(4E12.4,3X))
      WRITE(6,63) ((PARCOR(I,J),J=1,4),I=1,NMOM)
63    FORMAT(' PARCOR ',2(4E12.4,3X))
      WRITE(6,754) HNBLO,ETOT,TDIF,TOFSUM
754   FORMAT(' NR OF 1-BLOCK CLUSTERS ',I5,' ETOT TDIF TOFSUM ',3E12.4)
      IF(HNBLO.GT.0) WRITE(6,755) ((HBLOCK(I),EBLOCK(I)),I=1,HNBLO)
755   FORMAT(' BL-NR AND E ',5(I6,E12.4))
      IF(HRUN.GT.100) GO TO 5211
      WRITE(6,2431) DDUM(1),JDUM(2),MCNFIN
2431  FORMAT(' MC TOTAL MASS ',E12.4,' COPL.WIDTH ',I3,' MCNFIN ',I4)
      IF(MCNFIN.GT.0) WRITE(6,6061) ((VEC4MC(J,I),J=1,4),I=1,MCNFIN)
6061  FORMAT(' VEC4MC ',4E12.4)
      IF(LTYP.EQ.21) WRITE(6,2432) DDUM(3),JDUM(4)
2432  FORMAT(' MONTE CARLO PHOTON ENERGY ',E12.4,' BLOCK NR ',I5)
      IF(LTYP.EQ.22.OR.LTYP.EQ.2) WRITE(6,2433) DDUM(3),JDUM(5)
      IF(LTYP.EQ.22.OR.LTYP.EQ.2) WRITE(6,2433) DDUM(4),JDUM(6)
2433  FORMAT(' MONTE CARLO PHOTON ENERGIES ',E12.4,' BLOCK NR ',I5)
      IF(LTYP.EQ.4) WRITE(6,2434) (DDUM(II),II=3,6),(JDUM(II),II=11,14)
2434  FORMAT(' MONTE CARLO PHOTON ENERGIES ',4E12.4,' BLOCKS ',4I5)
      IF(LTYP.EQ.4) WRITE(6,2435) (DDUN(II),II=1,4)
2435  FORMAT(' MONTE CARLO PHOTON FI DIRECTIONS ',4E12.4)
      IF(LTYP.EQ.2) WRITE(6,2436) (HQUAL(JQ),JQ=1,2),ISWAP,MERR
2436  FORMAT(' MUON QUALITY FOR PHOTONS :',2I5,' ISWAP MERR ',2I5)
      WRITE(6,246) (HLGBR(I),I=1,42)
246   FORMAT(' HLGBR ',6I7)
C
5211  WRITE(6,2871) NRBTOF
2871  FORMAT(' NRBTOF ',I4)
      IF(NRBTOF.GT.0) WRITE(6,2872) (HNTOF(I),I=1,NRBTOF)
2872  FORMAT(' TOF COUNTERS ',10I3)
      IF(NMOMTR.EQ.0) GO TO 951
      DO 952  I = 1,NTR
      WRITE(6,83) ITRES(1,I),(TRES(K,I),K=2,7),ITRES(8,I),TRES(9,I)
83    FORMAT(' DEDX ',I4,6E12.4,I5,E12.4)
952   CONTINUE
      DO 953  I = 1,NTR
      WRITE(6,33) (IRELT(K,I),K=1,3),(RELT(K,I),K=4,14)
33    FORMAT(' TOF ',3I3,F8.3,F8.1,4F8.4,4F12.3,E12.4)
953   CONTINUE
      WRITE(6,41) ((PARM(I,J),I=1,10),J=1,NMOMTR)
41    FORMAT(' PARM ',10E12.4)
      WRITE(6,37) ((IPARM2(1,J),PARM2(2,J),IPARM2(3,J),PARM2(4,J)),J=1,
     $ NMOMTR)
37    FORMAT(' PARM2 ',I4,E12.4,I4,E12.4)
951   IF(NMOMGM.EQ.0) GO TO 3247
      WRITE(6,833) (HGMTYP(I),I=1,NMOMGM)
833   FORMAT(' HGMTYP ',10I4)
      WRITE(6,835) (HIBLOC(I),I=1,NMOMGM)
835   FORMAT(' HIBLOC ',10I4)
      WRITE(6,834) (KGGTR(I),I=1,NMOMGM)
834   FORMAT(' KGGTR ',10I4)
      WRITE(6,837) (DPETOT(I),I=1,NMOMGM)
837   FORMAT(' DPETOT ',5E12.4)
      IF(LTYP.LT.10) WRITE(6,3231) IDHITS,MUTREF
3231  FORMAT(' IDHITS AND MUTREF ',2I6)
3247  WRITE(6,3236)
3236  FORMAT('                         * * * *   ')
3000  CONTINUE
C
C       WRITE THE EVENT OUT AGAIN
C
      WRITE(3) KKK,(WRIT(I),I=1,KKK)
      IVRIG = IVRIG + 1
      IF((IVRIG/500)*500.EQ.IVRIG) WRITE(6,3412) IVRIG
3412  FORMAT('  RECORDS WRITTEN ',I8)
      GO TO 100
C
1005  WRITE(6,9104) IEVCN,IVRIG
 9104 FORMAT(' *****  READ ERROR ****  LAST EVENT READ,WRITTEN ',2I8)
      STOP
   90 CONTINUE
      WRITE(6,109) HWRIT(1),HWRIT(2)
  109 FORMAT('   END OF FILE --------->>>>    LAST EVENT : ',2I8)
9090  WRITE(6,1109) IEVCN,IVRIG
 1109 FORMAT('  NR OF EVENTS READ ',I8,'   AND WRITTEN ',I8)
      WRITE(6,1209) HWRIT(1),HWRIT(2)
 1209 FORMAT('   END OF JOB --------->>>>    LAST EVENT : ',2I8)
      STOP
      END
//LKED.SYSLIB DD
//            DD
//            DD DISP=SHR,UNIT=FAST,DSN=F11OLS.JADELD
//*                        INPUT:
//*O.FT02F001 DD UNIT=TAPE,DISP=OLD,DSN=F11OLS.TR12FS.NEU86A
//*   DD UNIT=AFF=FT02F001,DISP=OLD,DSN=F11OLS.MSMUMU80.B167TRGB
//*   DD UNIT=FAST,DISP=SHR,DSN=F11OLS.DISKMSC
//GO.FT02F001 DD DISP=SHR,DSN=F11OLS.TR22FS.JSRFRZ.BBLEAKWT.ALL8386
//*           DD DISP=SHR,DSN=F11OLS.TR22FS.NJS86GL2
//*                        OUTPUT:
//GO.FT03F001 DD DISP=SHR,DSN=F11OLS.TR22FS.BBLEAK.ALL8386
//*O.FT03F001 DD UNIT=FAST,DISP=(NEW,CATLG,DELETE),DCB=R01DCB.VBS,
//*  SPACE=(TRK,(120,50),RLSE),VOL=SER=STOR05,
//*  DSN=F11OLS.TR21FS.TAPE86C
//*GO.FT03F001 DD UNIT=TAPE,DISP=(NEW,CATLG,DELETE),DCB=R01DCB.VBS,
//*   DSN=F11OLS.TR20FS82.TPCUT82A
//*O.FT03F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*  MSVGP=VTMP,UNIT=3330V,DSN=F11OLS.TR21FS.JSRFRZ.TAPE82A
//*GO.FT03F001 DD DISP=(NEW,CATLG,DELETE),DCB=R01DCB.VBS,
//*        SPACE=(TRK,(80,10),RLSE),
//*        UNIT=3350,VOL=SER=FAST06,DSN=F11OLS.MSET80FS.PART4
//*        EXEC BACKUP,PARM='/MOVE'
C   28/03/97            MEMBER NAME  JBMWRIT4V(JADE56.S)    FORTRAN
//F11JOL   JOB 00010622,OLSSON,MSGLEVEL=(1,0),TIME=(0,15),CLASS=E
//*MAIN RELPRI=LOW
// EXEC FCLG,PARM.FORT=XL,REGION.LKED=800K,
//  REGION.GO=800K
C
      COMMON/CPROD/ NR,BEAM,PT,XNC,XNN,ECM,
     *              NP,NC,NN,PP(4,30),XM(30),ICH(30),ITP(30),IP(30,2),
     *              NF,NCF,NNF,PF(4,100),XMF(100),ICF(100),ITF(100),
     *              PSTRT(3,100)
C
      DATA NSEC / 3 /
      DATA NUNIT / 2 /
C
      IEVMIN = 0
      IEVMAX = 100000
      IWRT = 0
      NEVPRT = 5
C
      DO 1001 IEV=1,IEVMAX
      IF( JUHR(NSEC) .EQ. 2 ) GO TO 9000
      IF((IEV/500)*500.EQ.IEV) WRITE(6,4411) IEV
4411  FORMAT(' EVENTS READ  ',I10)
C
      IF(IEV.GE.IEVMIN) GO TO 1151
      READ(3,ERR=1005,END=90) NR
      GO TO 1001
CCC  READ EVENTS FROM TAPE
1151  IF(IEV.GT.IEVMAX) GO TO 90
      READ(3,ERR=1005,END=90) NR,BEAM,PT,XNC,XNN,ECM,
     *     NP,NC,NN,((PP(I4,N),I4=1,4),XM(N),ICH(N),ITP(N),
     *     (IP(N,I2),I2=1,2),N=1,NP),
     *     NF,NCF,NNF,((PF(I4,N2),I4=1,4),XMF(N2),ICF(N2),ITF(N2),
     *     (PSTRT(I3,N2),I3=1,3),N2=1,NF)
C
      IWRT = IWRT + 1
      IF(IWRT.LT.NEVPRT) WRITE(6,9769) NR,BEAM,ECM,NP,NC,NN,NF,NCF,NNF
9769  FORMAT(' NR ',I6,' BEAM ECM ',2E12.4,' NP NC NN  NF NCF NNF ',6I4)
      IF(IWRT.LT.NEVPRT) WRITE(6,9702) (((PP(LI,N),LI=1,4),XM(N),
     $ ICH(N),ITP(N)),N=1,NP)
 9702  FORMAT(' PPROD ',4E12.4,' MS CH TP ',E12.4,2I5)
      IF(IWRT.LT.NEVPRT) WRITE(6,9701) (((PF(I,N),I=1,4),XMF(N),
     $ ICF(N),ITF(N)),N=1,NF)
 9701  FORMAT(' PFIN ',4E12.4,' MS CH TP ',E12.4,2I5)
C
      CALL WRIT4V(2)
      IF((IWRT/500)*500.EQ.IWRT) WRITE(6,4410) IEV,IWRT
4410  FORMAT(' EVENTS READ AND WRITTEN ',2I10)
      GO TO 1001
C
 1005 READ(3)
      WRITE(6,9104) IEV, IWRT
 9104 FORMAT(/' *****  READ ERROR AFTER OR AT EVENT ',I6,
     *        ' EVENTS WRITTEN UP TO NOW ',I6,' *****'/)
C
 1001 CONTINUE
      GO TO 90
C
9000  WRITE(6,9001)
9001  FORMAT('  --->> TIME LIMIT      --->> TIME LIMIT ')
C
   90 CONTINUE
      WRITE(6,109) IWRT, IEV
  109 FORMAT(//2X,I6,' EVENTS HAVE BEEN WRITTEN. LAST EVENT NO.',I8)
      STOP
      END
//LKED.SYSLIB DD
//            DD
//            DD DSN=F11OLS.JADELD,DISP=SHR
//            DD DSN=R01UTL.CERN.KERNLIB4,DISP=SHR
//LKED.SYSIN DD *
 INCLUDE SYSLIB(MTSTRT)
//*                 INPUT
//GO.FT03F001 DD DISP=SHR,DSN=F11OLS.ETAPRIV4.NR8
//*GO.FT03F001 DD UNIT=TAPE,DISP=OLD,
//*     DSN=F11OLS.MCTRACK1.ETAPRI.B15
//*    DD UNIT=AFF=FT03F001,DISP=OLD,DSN=F11OLS.MCVEC442.MUTR2PCT.B16P7
//*                 OUTPUT
//GO.FT02F001 DD DUMMY,DCB=R01DCB.VBS
//*GO.FT02F001 DD DISP=SHR,
//*        DSN=F11OLS.MSXY
//*O.FT02F001 DD DISP=(NEW,CATLG,CATLG),DCB=R01DCB.VBS,
//*   DSN=F11OLS.ETAPRI5V,
//*        MSVGP=VTMP,UNIT=3330V
//*GO.FT02F001 DD UNIT=FAST,DISP=(NEW,CATLG,DELETE),DCB=R01DCB.VBS,
//*  SPACE=(TRK,(50,20),RLSE),DSN=F11OLS.TESTMCA
//*1O.FT02F001 DD UNIT=TAPE,DISP=(,CATLG,DELETE),
//*         DCB=R01DCB.VBS,
//*         DSN=F11OLS.MCVECSUM.MSMU6NR2.B17P3
//*GO.FT02F001 DD UNIT=DISK,DISP=(,PASS),
//*   DCB=(R01DCB.VBS,LRECL=6236),
//*       SPACE=(CYL,(180,20)),DSN=&&SCRTCH
//*
//*  COPY DISK OUTPUT TO TAPE
//*
//*COPY EXEC PGM=IEBGENER
//*SYSUT1 DD UNIT=DISK,DISP=(OLD,DELETE),DSN=&&SCRTCH
//*SYSUT2 DD UNIT=TAPE,DISP=(NEW,CATLG,DELETE),
//*          DCB=(R01DCB.VBS,LRECL=6236,BLKSIZE=24948),
//*          DSN=F11OLS.MSETAPRI.BEAM18.SUM1
//*SYSPRINT DD SYSOUT=*
//*SYSIN DD DUMMY
