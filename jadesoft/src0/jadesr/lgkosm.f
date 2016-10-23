C   12/02/82 403240433  MEMBER NAME  LGKOSM   (JADESR)      FORTRAN
C     *************************************************
C     *    COSMIC RAY REDUCTION IN BARREL LEAD GLASS  *
C     *          ORIGINAL VERSION 09-11-79 S.K.       *
C     *          FINAL VERSION    05-12-79 S.K.       *
C     *          CODED AGAIN BY J.KANZAKI 12-02-82    *
C     *          MODIFIED BY S.YAMADA  13-02-82       *
C     *                IT USES ONLY /CWORK/ NOW       ******************
C     *  ERROR: UNINITIALIZED VARIABLE IPATR HAS BEEN FIXED, 24.3.1984 *
C     *                           J.OLSSON            ******************
C     *************************************************
C-----------------------------------------------------------------------
C
      SUBROUTINE LGKOSM(KFLAG,EAB,NHITA,NHITB,COSA,COSB,COSD,DNORM)
C
C----------------------------------------------------------------------+
C             KFLAG=0 : NORMAL BARREL-NEUTRAL EVENT                    |
C                                                                      |
C             KFLAG=16: THE EVENT HAS AT LEAST 1 RECONSTRUCTED TRACK   |
C                                                 IN THE JET CHMBER    |
C             KFLAG=8 : NO ENERGY DEPOSITION IN LEAD GLASS COUNTERS    |
C                                                                      |
C             KFLAG=4 : NO ENERGY DEPOSITION IN THE BARREL PART        |
C                       BUT ETOT IS NOT ZERO                           |
C             KFLAG=2 : ENERGY DEPOSITION IS DOMINATED IN THE END CAP  |
C                                                                      |
C             KFLAG=1 : THE MAX ENERGY CLUSTER IS IN THE END CAP PART  |
C             ---------------------------------------------------------+
C                                                                      |
C          EAB(1,1) : LARGER EIGEN-VALUE OF THE CLUSTER                |
C                          WITH MAX ENERGY IN THE BARREL PART          |
C          EAB(1,2) : SMALLER EIGEN-VALUE OF THE CLUSTER               |
C                          WITH MAX ENERGY IN THE BARREL PART          |
C          EAB(1,3) : MAXIMUM CLUSTER ENERGY                           |
C                                     IN THE BARREL LEAD GLASS         |
C          EAB(2,1) : LARGER EIGEN-VALUE OF THE CLUSTER                |
C                          WITH 2ND ENERGY IN THE BARREL PART          |
C          EAB(2,2) : SMALLER EIGEN-VALUE OF THE CLUSTER               |
C                          WITH 2ND ENERGY IN THE BARREL PART          |
C          EAB(2,3) : 2ND MAXIMUM CLUSTER ENERGY                       |
C                                     IN THE BARREL LEAD GLASS         |
C             TA    : ANGLE BETWEEN PHI & EIGEN-VECTOR OF CLUSTER A    |
C                                                                      |
C             TB    : ANGLE BETWEEN PHI & EIGEN-VECTOR OF CLUSTER B    |
C                                                                      |
C             TD    : ANGLE BETWEEN TWO CLUSTERS                       |
C                                                                      |
C            NHITA  : ADC HIT OF CLUSTER A                             |
C                                                                      |
C            NHITB  : ADC HIT OF CLUSTER B                             |
C                                                                      |
C----------------------------------------------------------------------+
      IMPLICIT INTEGER *2 (H)
      COMMON /BCS/ IW(24000)
      DIMENSION RW(24000)
      INTEGER*2 HW(48000)
      EQUIVALENCE (IW(1),RW(1)),(IW(1),HW(1))
      COMMON /MAXCLS/EIG1,EIG2,EIGSUM,EIGDIV,MAXHIT,ECMAX,MAXCL
      COMMON /EIGAB/CSD(2),CSA(2),CSB(2),
     &        EIGA1,EIGA2,EIGB1,EIGB2,ENEA,ENEB,NA,NB,DNOR
      DIMENSION EAB(2,3),COSD(2),COSA(2),COSB(2)
#include "clgwork1.for"
C-----------------------------------------------------------------------
      KFLAG=0
      DO 5 L=1,3
      EAB(1,L)=0.
      EAB(2,L)=0.
    5 CONTINUE
      COSD(1)=0.
      COSD(2)=0.
      COSA(1)=0.
      COSB(1)=0.
      COSA(2)=0.
      COSB(2)=0.
      NHITA=0
      NHITB=0
      DNORM=0.
C-----------------------------------------------------------------------
      CALL LGANAK
      EAB(1,1)=EIGA1
      EAB(1,2)=EIGA2
      EAB(2,1)=EIGB1
      EAB(2,2)=EIGB2
      EAB(1,3)=ENEA
      EAB(2,3)=ENEB
      COSD(1)=CSD(1)
      COSD(2)=CSD(2)
      COSA(1)=CSA(1)
      COSB(1)=CSB(1)
      COSA(2)=CSA(2)
      COSB(2)=CSB(2)
      NHITA=NA
      NHITB=NB
      DNORM=DNOR
C-----------------------------------------------------------------------
C  CHECK NUMBER OF RECONSTRUCTED TRACKS
C
      INPATR=IW(IBLN('PATR'))
      IF(INPATR.NE.0) GO TO 16
C     WRITE(6,2000)
C2000 FORMAT(' ******* IBLN(PPATR) ***** ERROR **** IN LGKOSM')
      GO TO 17
C  16 INPATR=IW(IPATR)
   16 CONTINUE
      NTRACK=IW(INPATR+2)
      IF(NTRACK.NE.0) KFLAG=KFLAG+16
   17 CONTINUE
C-----------------------------------------------------------------------
C  CHECK 'END CAP BHABHA' EVENTS
C
CC         WRITE(6,6001) ETOT
C6001      FORMAT(' ETOT=',4F10.3)
      IF(ETOT(1).EQ.0.) GO TO 2
C-----------------------------------------------------------------------
C---  THE MAX CLUSTER IN THE BARREL PART IS COMPARED WITH
C---  THE MAX CLUSTER IN THE END CAP
C
      IFL=0
      IB=0
      EM=0.
      DO 10 N=1,NCLST
      ENECL=CLSPRP(2,N)
      IF(EM.GT.ENECL) GO TO 10
      EM=ENECL
      NM=N
   10 CONTINUE
      IF(ICLSPR(1,NM).EQ.0) GO TO 11
      IFL=1
      EAB(1,3)=EM
      NHITA=HMAPCL(2,NM)-HMAPCL(1,NM)+1
C-----------------------------------------------------------------------
   11 CONTINUE
      KFLAG=KFLAG+IFL
      IF(ETOT(2).LE.0.) KFLAG=KFLAG+4
      FRCBRL=ETOT(2)/ETOT(1)
      IF(FRCBRL.LT.0.5) KFLAG=KFLAG+2
      GO TO 7
C-----------------------------------------------------------------------
    2 KFLAG=KFLAG+8
    7 CONTINUE
      RETURN
      END
