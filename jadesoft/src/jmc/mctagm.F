C   23/08/85 903061127  MEMBER NAME  MCTAGM   (S)           FORTRAN
C   23/08/85 508231713  MEMBER NAME  ALLFOR   (S)           FORTRAN
C   23/08/85 508231710  MEMBER NAME  MCTAG    (S)           FORTRAN

C   13/03/84 403232324  MEMBER NAME  MCTAGM   (S)           FORTRAN
C
C THIS ROUTINE IS AN ATTEMPT TO PRODUCE A STANDARD TRACKING
C ROUTINE FOR JADE TAGGERS
C
C EXPLANATION OF SOME TERMS USED:
C
C  MARKMC      DATE      INMARK                         MARK AS USED IN
C
C   1      1979/1980       0                                     1
C   2      EARLY 1981      1     ( NO LEAD SNOUTS )              2
C   3      1981/1982       1     ( WITH LEAD SNOUTS )            2
C   4      1983-1986       2                                     3
C
C INMARK IS WRITTEN IN TO THE ATAG BANKS PRODUCED TO ENABLE TAGAN TO
C             KNOW WHICH SIMULATION HAS BEEN PERFORMED.

C
C
C INPUT - PARTICLE INFO IN PV AND R0 ARRAYS
C
C   PV( 1 ) =   X COMPONENT OF PARTICLES MOMENTUM
C   PV( 2 ) =   Y COMPONENT OF PARTICLES MOMENTUM
C   PV( 3 ) =   Z COMPONENT OF PARTICLES MOMENTUM
C   PV( 4 ) =   ENERGY
C   PV( 5 ) =   MASS
C   PV( 6 ) =   MAGNITUDE OF MOMENTUM
C   PV( 7 ) =   CHARGE
C   PV( 8 ) =   TYPE CODE
C   PV( 9 ) =
C   PV( 10) =
C
C A.J.FINCH 13/3/84
C PROPER RADIATION LENGTH FOR LEAD-SCINT. SANDWICH,  2/3-89   J.O.
C UPDATE PV-VECTOR FOR THOSE PARTICLES THAT HIT TAGGER  6/3-89   J.O.
C
      SUBROUTINE MCTAGM(PV,R0)
C
C
      IMPLICIT INTEGER*2 (H)
C
#include "cmctag.for"
C
C
C
      DIMENSION PV(10),R0(3)
      REAL*4 THMINM(10),THMINP(10),THMAX(10),LENGTH(10)
      REAL*4 ZMIN(10)
      REAL*4 RADLEN(10),AGG(200),SCALE(10)
C
C HERE ARE SET ALL THE IMPORTANT PARAMETERS
C
C 1) THMIN,THMAX - ACCEPTANCE LIMITS DETERMINED BY BEAM PIPE
C                  AND LEAD ETC.
C
C    THMINM - -Z
C    THMINP - +Z
C
      DATA THMINM/0.0335,0.0361,0.0410,7*0.0320/
      DATA THMINP/0.0335,0.0361,0.041,7*0.0320/
      DATA THMAX/0.1052,0.082,0.082,0.075,6*0.082/
C     DATA THSM/0.002/
C
C 2) RADLEN - RADIATION LENGTH OF MATERIAL OF DETECTOR
C    3 FIRST VALUES ARE FOR SF5 LEADGLASS
C    LATTER VALUES CALCULATED FROM SANDWICH 2MM PB, 10 MM SCINTILLATOR
C
      DATA RADLEN/3*22.39,7*25.3/
C
C 3) ZMIN - DISTANCE FROM INTERACTION POINT TO FACE OF BLOCKS
C
      DATA ZMIN/4850.0,9*2950.0/
C
C 4) LENGTH - LENGTH OF BLOCKS IN MM
C
      DATA LENGTH/3*450.0,7*500.0/
C
C 5) ECRIT - CRITICAL ENERGY OF LEAD
C
      DATA ECRIT/6.90/
C
C 6) SCALE - CORRECT FOR    CALIBRATION
C
      DATA SCALE /10*1.05/
C
      REAL LOGTWO/0.69314718/
      DATA TLIM/5.0/
      DATA IWRITE/0/
      DATA IWLIM/10/
C
C
C ======== CODE ======
C     EXTRACT RELEVANT INFO FROM INPUT ARRAYS
C
      IT = PV(8)+0.5
C
C---  THROW OUT PARTICLES WITHOUT TYPE-CODE
C
      IF (IT.LE.0) RETURN
C
C FIND X,Y,Z DIRECTION COSINES
C
      DX = (PV(1))/PV(6)
      DY = (PV(2))/PV(6)
      DZ = (PV(3))/PV(6)
C
C FIND Z SIGN ( +/- 1 )
      ISIGNZ=1
      IF(PV(3).LT.0)ISIGNZ=-1
C
C AND ENERGY OF PARTICLE
C
      ENHIT = PV(4)
C
C---  CHECK WHETHER PARTICLE TRACK LIES INSIDE THETA-ACCEPTANCE
C       28 MR < THETA < 105.2 MR  OR  3.0364 MR < THETA < 3.1136 MR
C
      THETA = ARCOS(ABS(DZ))
      THMIN=THMINM(MARKMC)
      IF(ISIGNZ.EQ.1)THMIN=THMINP(MARKMC)
C
C
      IF (THETA.LT.THMIN.OR. THETA.GT.THMAX(MARKMC)) GOTO 1000
C
C BRANCH HERE ON WHETHER SHOWERING OR NON SHOWERING PARTICLE
C
      IF (IT.GT.2) GOTO 1000
C
C THESE ARE SHOWERING PARTICLES
C
      IHIT = IHIT + 1
C
C---  PHOTONS AND ELECTRONS
C
C QUESTION ? - WHERE IS THE SHOWER MAXIMUM (LONGITUDINAL)
C            - AND IS IT CONTAINED IN THE DETECTOR ?
C
C FOR AN ANSWER USE THE SIMPLE MODEL DESRIBED IN PERKINS
C FOR LONGITUDINAL DEVELOPMENT OF SHOWERS
C
C
C DMAX IS THE DISTANCE IN MM TO SHOWER MAXIMUM
C    RADLEN CONVERTS FROM RADIATION LENGTHS TO  MM
C    ECRIT = 6.9 MEV FOR LEAD
C
      ARG = (ENHIT*1000.0)/ECRIT
      IF(ARG.GT.0)GOTO 123
           ARG = 1
           IWRITE = IWRITE + 1
           IF(IWRITE.LT.IWLIM)WRITE(6,6001)ARG,ENHIT,ECRIT
6001       FORMAT(' ERROR IN MCTAGM ARG=ENHIT/ECRIT ',3(2X,F8.3))
C
123   A=LOG(ARG)
C
      DMAX = (A/LOGTWO)*RADLEN(MARKMC)
C
C ZT - Z AT WHICH SHOWER IS MAXIMUM
C AZT - ABSOLUTE VALUE OF ZT
C
      AZT = ZMIN(MARKMC) + (DMAX*ABS(DZ))
      ZT=ISIGNZ*AZT
C
C     CHECK FOR SHOWER CONTAINMENT LONGITUDINAL- HOW CLOSE IS MAX
C     TO BACK OF DETECTOR IN RADIATION LENGTHS ?
C
      ESHOW = ENHIT*SCALE(MARKMC)
      TBACK = (ZMIN(MARKMC)+LENGTH(MARKMC)-AZT)/RADLEN(MARKMC)
      IF(TBACK.GE.TLIM)GOTO 300
C
C SIMPLE MINDED TREATMENT OF SHOWERS LEAVING BACK OFDETECTOR
C
       ESHOW=(ENHIT/TLIM)*TBACK
C
C NOW SIMULATE TRANSVERSE SPREAD OF SHOWER
C
C  FIRST FIND X,Y AT WHICH SHOWER IS MAXIMUM
C
 300      CALL MCTAGC(DX,DY,DZ,XSH,YSH,ZT)
C      WRITE(6,679)XSH,YSH,ESHOW
 679   FORMAT(' HIT AT ',F8.2,F8.2,' ENERGY ',F8.2)
C
C
       CALL VZERO(AGG,200)
C
       CALL MCTAGE(MARKMC,XSH,YSH,ZT,AGG,ESHOW)
C
       DO 200 K = 1,192
 200   IF(AGG(K).GT.0)HGG(K) = HGG(K) + AGG(K)
C
C
C
C
C----- END OF TREATMENT OF SHOWERING PARTICLES
2000   PV(1) = 0.
       PV(2) = 0.
       PV(3) = 0.
       PV(4) = 0.
C
C HERE FOR NON SHOWERING PARTICLES
C
1000  RETURN
      END
