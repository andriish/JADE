C   17/09/84 807231820  MEMBER NAME  PRAGAI0  (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PRAGAI(IPRIT)
C-----------------------------------------------------------------------
C
C   AUTHOR    J.OLSSON    28/07/86 :  CHECK "PRINT AGAIN" CONDITION
C        LAST CHANGE      07/11/86 :  TYPE ERRORS CORRECTED
C
C   THE VARIOUS COMMONS FOR DETECTOR SMEARING AND TRIGGER CONDITIONS
C   ARE CHECKED FOR DIFFERENCES, AS COMPARED TO BACKUPS FROM PREVIOUS
C   CALLS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C /////////////////////////////////////////////////////////////////
C
      COMMON /CTRIGG/ IHIST(3),NBPTFW,TAGADC,IDUM1(4),HDUM1,
     *  HLGBRT,HLGBST,HLGQT,HLGTOT(4,2),HECAPT(4),HLGTL,HLGTH,HTGTRG,
     *                HDUM2(9),IWIDBS,NRNBSL,NRNBSH,NTOFBS,IDUM2(10),
     *                NTOFC,NTOFC1,NTOFC2,NTBGC,NTBGC1,NTBGC2,
     *                IWCOLL,IWCOLN,IWMPRG,HFMPRL,HFMPRH,HWCOLN,HMPRON,
     *                IWCTBG,NTFCOL,NTFCLN,NTFBS2,IDUM3(9),
     *  HITCLL(3),HITWLL(3),HITSUM(3),HCHAMB(3),HMASK(16,3),HDEADC(10),
     *  HACC1,HACC2,HACC3,HACC4,HACCM,HDUM6,IWIDT2,HACCB1,HACCB2,
     *  HT1ACC,HT1PSP,IDUM4(8)
C
      COMMON /XTRIGG/ JPRAGA(110)
      DIMENSION KPRAGA(110),XPRAGA(110),YPRAGA(110)
      EQUIVALENCE (KPRAGA(1),IHIST(1),XPRAGA(1))
      EQUIVALENCE (JPRAGA(1),YPRAGA(1))
C
C /////////////////////////////////////////////////////////////////
C
      COMMON / CRDSTA / NDEAD, NCDEAD, HITD(10), HCELLD(10), IPHALG
C
      COMMON /XRDSTA/ JPRAGB(13)
      DIMENSION KPRAGB(13),XPRAGB(13),YPRAGB(13)
      EQUIVALENCE (KPRAGB(1),NDEAD,XPRAGB(1))
      EQUIVALENCE (JPRAGB(1),YPRAGB(1))
C
C /////////////////////////////////////////////////////////////////
C
      COMMON / CJDRCH / DRCH(34),
     +                  PEDES, TZERO(3),
     +                  DRIROT(96,2), SINDRI(96,2), COSDRI(96,2),
     +                  DRIVEL(96,2)
C
      COMMON /XJDRCH/ JPRAGC(38)
      DIMENSION KPRAGC(38),XPRAGC(38),YPRAGC(38)
      EQUIVALENCE (KPRAGC(1),DRCH(1),XPRAGC(1))
      EQUIVALENCE (JPRAGC(1),YPRAGC(1))
C
C /////////////////////////////////////////////////////////////////
C
      COMMON / CJVTXC / VXCC(15),
     +                  DRILOR(24), SNLORA(24), CSLORA(24),
     +                  DRVELO(24)
C
      COMMON /XJVTXC/ JPRAGD(111)
      DIMENSION KPRAGD(111),XPRAGD(111),YPRAGD(111)
      EQUIVALENCE (KPRAGD(1),VXCC(1),XPRAGD(1))
      EQUIVALENCE (JPRAGD(1),YPRAGD(1))
C
C /////////////////////////////////////////////////////////////////
C
      COMMON / CBINV /  DTRSVV, ZRSVV, EFFVV(2), DOUBVV(3), IRNHVV
C
      COMMON /XBINV/ JPRAGE(8)
      DIMENSION KPRAGE(8),XPRAGE(8),YPRAGE(8)
      EQUIVALENCE (KPRAGE(1),DTRSVV,XPRAGE(1))
      EQUIVALENCE (JPRAGE(1),YPRAGE(1))
C
C /////////////////////////////////////////////////////////////////
C
      COMMON / CBIN   / TIME(6),ZOF,ZRS,ZL,ZSC,EPSI(3),DOUB(3),IRN(3),
     +                BINDL8(6),RJITT, DLRSLN(3), DLZSLN(3)
C
      COMMON /XBIN/ JPRAGF(32)
      DIMENSION KPRAGF(32),XPRAGF(32),YPRAGF(32)
      EQUIVALENCE (KPRAGF(1),TIME(1),XPRAGF(1))
      EQUIVALENCE (JPRAGF(1),YPRAGF(1))
C
C /////////////////////////////////////////////////////////////////
C
      COMMON / CBINMC / BINMC(6)
C
      COMMON /XBINMC/ JPRAGG(6)
      DIMENSION KPRAGG(6),XPRAGG(6),YPRAGG(6)
      EQUIVALENCE (KPRAGG(1),BINMC(1),XPRAGG(1))
      EQUIVALENCE (JPRAGG(1),YPRAGG(1))
C
C-/////////////////  C O D E  //////////////////////////////////////////
C
      IPRIT = 0
C
      DO 1  I = 1,110
      IF(JPRAGA(I).EQ.KPRAGA(I)) GO TO 1
C     WRITE(6,3801) I,JPRAGA(I),KPRAGA(I),XPRAGA(I),YPRAGA(I)
C801  FORMAT('  PRAGAI FOUND DIFFERENCE IN /CTRIGG/, ELEMENT ',I3,' VALU
C    $ES OLD-NEW, INT',2I5,' REAL ',2E12.4)
      GO TO 8000
1     CONTINUE
C
      DO 2  I = 1,13
      IF(JPRAGB(I).EQ.KPRAGB(I)) GO TO 2
      WRITE(6,3802) I,JPRAGB(I),KPRAGB(I),XPRAGB(I),YPRAGB(I)
3802  FORMAT('  PRAGAI FOUND DIFFERENCE IN /CRDSTA/, ELEMENT ',I3,' VALU
     $ES OLD-NEW, INT',2I5,' REAL ',2E12.4)
      GO TO 8000
2     CONTINUE
C
      DO 3  I = 1,38
      IF(JPRAGC(I).EQ.KPRAGC(I)) GO TO 3
      WRITE(6,3803) I,JPRAGC(I),KPRAGC(I),XPRAGC(I),YPRAGC(I)
3803  FORMAT('  PRAGAI FOUND DIFFERENCE IN /CJDRCH/, ELEMENT ',I3,' VALU
     $ES OLD-NEW, INT',2I5,' REAL ',2E12.4)
      GO TO 8000
3     CONTINUE
C
      DO 4  I = 1,111
      IF(JPRAGD(I).EQ.KPRAGD(I)) GO TO 4
      WRITE(6,3804) I,JPRAGD(I),KPRAGD(I),XPRAGD(I),YPRAGD(I)
3804  FORMAT('  PRAGAI FOUND DIFFERENCE IN /CJVTXC/, ELEMENT ',I3,' VALU
     $ES OLD-NEW, INT',2I5,' REAL ',2E12.4)
      GO TO 8000
4     CONTINUE
C
      DO 5  I = 1,8
      IF(JPRAGE(I).EQ.KPRAGE(I)) GO TO 5
      WRITE(6,3805) I,JPRAGE(I),KPRAGE(I),XPRAGE(I),YPRAGE(I)
3805  FORMAT('  PRAGAI FOUND DIFFERENCE IN /CBINV/, ELEMENT ',I3,' VALU
     $ES OLD-NEW, INT',2I5,' REAL ',2E12.4)
      GO TO 8000
5     CONTINUE
C
      DO 6  I = 1,32
      IF(JPRAGF(I).EQ.KPRAGF(I)) GO TO 6
      WRITE(6,3806) I,JPRAGF(I),KPRAGF(I),XPRAGF(I),YPRAGF(I)
3806  FORMAT('  PRAGAI FOUND DIFFERENCE IN /CBIN/, ELEMENT ',I3,'  VALU
     $ES OLD-NEW, INT',2I5,' REAL ',2E12.4)
      GO TO 8000
6     CONTINUE
C
      DO 7  I = 1,6
      IF(JPRAGG(I).EQ.KPRAGG(I)) GO TO 7
      WRITE(6,3807) I,JPRAGG(I),KPRAGG(I),XPRAGG(I),YPRAGG(I)
3807  FORMAT('  PRAGAI FOUND DIFFERENCE IN /CBINMC/, ELEMENT ',I3,' VALU
     $ES OLD-NEW, INT',2I5,' REAL ',2E12.4)
      GO TO 8000
7     CONTINUE
C
      GO TO 9000
C
 8000 IPRIT = 1
C
C     NEW BACKUP, AFTER DIFFERENCE WAS FOUND
C
      DO 11  I = 1,110
      JPRAGA(I) = KPRAGA(I)
11    CONTINUE
C
      DO 12  I = 1,13
      JPRAGB(I) = KPRAGB(I)
12    CONTINUE
C
      DO 13  I = 1,38
      JPRAGC(I) = KPRAGC(I)
13    CONTINUE
C
      DO 14  I = 1,111
      JPRAGD(I) = KPRAGD(I)
14    CONTINUE
C
      DO 15  I = 1,8
      JPRAGE(I) = KPRAGE(I)
15    CONTINUE
C
      DO 16  I = 1,32
      JPRAGF(I) = KPRAGF(I)
16    CONTINUE
C
      DO 17  I = 1,6
      JPRAGG(I) = KPRAGG(I)
17    CONTINUE
C
 9000 CONTINUE
      RETURN
      END
      BLOCK DATA BL4            ! PMF 01/07/99 add name
C
C  DATA SETTING OF BACKUP COMMONS FOR PRAGAI CHECKS
C
      COMMON /XTRIGG/ JPRAGA(110)
      DATA JPRAGA /110 * -1/
C
C /////////////////////////////////////////////////////////////////
C
      COMMON /XRDSTA/ JPRAGB(13)
      DATA JPRAGB /13 * -1/
C
C /////////////////////////////////////////////////////////////////
C
      COMMON /XJDRCH/ JPRAGC(38)
      DATA JPRAGC /38 * -1/
C
C /////////////////////////////////////////////////////////////////
C
      COMMON /XJVTXC/ JPRAGD(111)
      DATA JPRAGD /111 * -1/
C
C /////////////////////////////////////////////////////////////////
C
      COMMON /XBINV/ JPRAGE(8)
      DATA JPRAGE /8 * -1/
C
C /////////////////////////////////////////////////////////////////
C
      COMMON /XBIN/ JPRAGF(32)
      DATA JPRAGF /32 * -1/
C
C /////////////////////////////////////////////////////////////////
C
      COMMON /XBINMC/ JPRAGG(6)
      DATA JPRAGG /6 * -1/
C
      END
