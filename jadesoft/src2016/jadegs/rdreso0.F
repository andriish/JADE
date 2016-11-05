C   01/11/84 712141933  MEMBER NAME  RDRESO0  (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDRESO( BINMC )
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN    25/04/79 :  SIMULATION OF SPACE RESOLUTION
C                                     IN JET CHAMBER
C        MOD  E. ELSEN    30/05/79 :  ?
C        MOD  J. HAGEMANN 12/01/83 :  DRIFT TIME: A HALF BIN WIDTH HAS
C             R. RAMCKE               BEEN REMOVED
C        MOD  J. OLSSON   12/01/83 :  DRIFT TIME: NEW SMEARING SCHEME
C                                                 AS USED BY W. BARTEL
C        MOD  J. HAGEMANN 07/06/83 :  NOW NO SHIFT IN Z-DIRECTION
C             R. RAMCKE
C        MOD  J. OLSSON   04/08/83 :  NOW ONLY ONE ACTUAL PARAMETER
C        MOD  J. HAGEMANN 08/02/84 :  DRIFT TIME: IMPROVED SMEARING
C             J. OLSSON               SCHEME. RESOLUTION NOW DRIFT
C                                     TIME DEPENDENT
C                                     PARAMETER (0.677E-3) GIVEN BY
C                                     J.SPITZER RESEARCH
C        MOD  J. HAGEMANN 11/10/84 :  FORBID NEGATIVE DRIFT TIMES
C   LAST MOD  E ELSEN   10/12/87   :  R-PHI RESOLUTION (2 GAUSS SCHEME)
C
C  THE SMEARING ACC. TO BARTEL IS NOW MODIFIED (BY J.HAGEMANN) TO OMIT
C    THE DL8 BINNING. THE SMEARING IS THEREFORE GIVEN ONLY BY RJITT. IN
C    THIS WAY A BETTER AGREEMENT WITH MOMENTUM RESOLUTION, CHISQ. AND
C    VERTEX DISTRIBUTION IS OBTAINED.
C
C   THE VARIABLES IN COMMON/CBIN/....,BINDL8,RJITT
C   ARE BLOCKDATA SET IN SUBROUTINE RDMTCO.
C   SCALE DRIFT TIMES AND COMPUTE Z-AMPLITUDES FROM RAW DATA
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL*1 LJUMP
C
#include "cmubcs.for"
C
      COMMON / CADMIN / DUMM(7), ISMEAR
      COMMON / CJDRCH / DRCH(34)
      COMMON / CBIN   / TIME(6),ZOF,ZRS,ZL,ZSC,EPSI(3),DOUB(3),IRN(3),
     +                BINDL8(6),RJITT, DLRSLN(3)
C
      DIMENSION BINMC(6)
C
      DATA  ICALL /0/
C
C------------------  C O D E  ------------------------------------------
C
      IF( ICALL.EQ.1 ) GO TO 1
         ICALL = 1
         ISMEAR = 1
    1 CONTINUE
C
      IPJ = IDATA( IBLN('JETC') )
      IF( IPJ .EQ. 0 ) RETURN
C
C
      LJUMP = .FALSE.
      IF(BINDL8(1) .EQ. .005 .AND. RJITT .EQ. .0) LJUMP = .TRUE.
      IL4 = IPJ*2 + 101
      IH4 = IL4 + HDATA(IPJ*2+99 ) - 4
C
C                       LOOP OVER ALL WIRES AND SET RESOLUTIONS
      DO 100  J = IL4, IH4, 4
         IF( LJUMP ) GO TO 50
            NWIR   = HDATA(J) / 8
            IR     = MIN0( NWIR/384+1 , 3 )
            ICL    = IR * 2
            IF( MOD(NWIR,16).LT. 8 ) ICL = ICL - 1
C
C                       SET DRIFT TIMES
            IDRI   = HDATA(J+3)
            DRIINT = FLOAT(IDRI)*BINMC(ICL)
            CALL NVERT( 1., 0., G1R )
C                       DRIFT TIME DEPENDENT SMEARING
            RJITT1 = DRIINT*.677E-3 + RJITT
C                                           DOUBLE GAUSSIAN
            IF( RN(DUM).GT.DLRSLN(1) ) GO TO 10
              RJITT1 = G1R*RJITT1
              GO TO 20
   10       CONTINUE
C                                           SHIFTED GAUSSIAN
              RJITT1 = (G1R*DLRSLN(3)-DLRSLN(2))*RJITT1
   20       CONTINUE
            DRIINT = DRIINT + RJITT1
C---------------------------------------------------------------------
            HDATA(J+3) = HFIX(ABS(DRIINT/BINMC(ICL)))
C
C                       SET LEFT AND RIGHT AMPLITUDES
CCCC          XL = HDATA(J+1) - DRCH(27)
   50    IAL  = HDATA(J+1)
         XL   = FLOAT(IAL)
         CALL NVERT( DRCH(28), XL, XLL )
         XL   = XLL / DRCH(30)
         IAR  = HDATA(J+2)
         XR   = FLOAT(IAR)*DRCH(31)
         HDATA(J+2) = HFIX((XL+.5)*XR)
         HDATA(J+1) = HFIX((-XL+.5)*XR)
  100 CONTINUE
      RETURN
      END
