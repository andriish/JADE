C   14/12/81 808021744  MEMBER NAME  LGECOR9  (SOURCE)      FORTRAN
C
C
C****************************************************************
      SUBROUTINE LGECOR( RDATA, DEP, NRBLOC, IFLAG)
C****************************************************************
C
C  THIS PROGRAM CALCULATES THE INCIDENT ENERGY
C  FROM THE MEASURED ENERGY.
C
C      05/3/1979     HIROSHI TAKEDA
C           MODIFICATION   22/09/79
C           MODIFICATION   04/10/79 Y.WATANABE
C           MODIFICATION   09/02/80 H.TAKEDA(TYPED BY Y.WATANABE)
C         IMPLEMENTED ON 21/2/80 18:00
C           MODIFICATION   08/09/80 Y.W. LGPOSC FOR BARREL.
C           MODIFICATION   14/12/81 H.T. ENGLOS FOR END-CAP.
C           MODIFICATION   14/12/81 H.T. THICKNESS FOR BARREL MATERIALS
C           MODIFICATION   06/04/82 S.ODAKA
C                          BRLGN FOR BARREL INSTEAD OF LGPOSC
C
C    CHANGED 02.08.88 D.PITZL: CALL ENCORR FOR PHOTONS
C                     T.OEST : READOUT THRESHOLD CORRECTION AS
C                              FUNCTION OF # OF BLOCKS IN CLUSTER
C                              NRBLOC IS ARGUMENT NOW
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  RDATA : DATA BUFFER OF THE FOLLOWING PARAMETERS.
C
C    IDATA(1)   0   BARREL
C                    -1   BOTTOM END CAP
C                    +1   TOP    END CAP
C    RDATA(2)   CLUSTER ENERGY IN GEV
C    RDATA(3)   SIGMA(ENERGY)
C    RDATA(4)   WEIGHTED AVERAGE PHI        BARREL
C                                       X        END CAP
C    RDATA(5)   WEIGHTED AVERAGE  Z         BARREL
C                                       Y        END CAP
C    RDATA(6)   SIGMA PHI (WEIGHTED)        BARREL
C                            X                   END CAP
C    RDATA(7)   SIGMA  Z (WEIGHTED)         BARREL
C                            Y                   END CAP
C    IDATA(8)   NUMBER OF CORRESPONDING INNER TRACKS
C    RDATA(9--11) DIRECTION COSIGNS CORRECTED FOR SHOWER DEPTH
C
C  DEP  :  SHOWER DEPTH IN MM
C  IFLAG   0    NORMAL RETURN
C          1    ITERATION OVERFLOW
C          2    HIT OUTSIDE END-CAP
C          3    TOO MUCH ENERGY LOSS
C          4    OTHER ILLEGAL RETURNS
C
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
C
***PMF      DIMENSION RDATA(2)
      DIMENSION RDATA(11)
      COMMON /CLGDMS/  X0,RADIUS(6),RADSX0(6),
     1  THX0(4),ZEND(2),ZENDX0(2),
     2  ZWID(2),ZGAP(2),PHWID(2),
     3  ZECAP(4),ZECAPX(4),THECPX(2),
     4  ECXLST(24),ECYLST(24)
C
C  THECPX(1)   THICKNESS OF END CAP COUNTER IN MM
C        (2)                                   X0
C  RADIUS(1,2) DISTANCE TO THE COIL SURFACE (INNER AND OUTER)
C        (3,4) DISTANCE TO THE LEAD GLASS SURFACE (TYPE A)
C  ZEND(1)     END POINT OF BARREL PART COUNTERS  (-)
C      (2)                                        (+)
C  ZGAP(1)     COUNTER GAP OF BARREL PART IN MM
C  THX0(1)     MAGNET COIL THICKNESS IN X0
C              X0 = 8.9 CM FOR ALUMINUM
C
      EQUIVALENCE (PHI,XCO),(ZCO,YCO),(IBE,BE)
      EQUIVALENCE ( RIDTRK, NIDTRK )
      DATA ETHRE/0.01/,PAI2/6.283185/
C
C
      DATA ICALLS / 0 /
C
      IHEAD = IDATA ( IBLN ( 'HEAD' ) )
      NRUN  = HDATA ( 2 * IHEAD + 10 )
      NEVE  = HDATA ( 2 * IHEAD + 11 )
      MCTYP = HDATA ( 2 * IHEAD + 17 )
C
      ICALLS = ICALLS+1

      IF ( ICALLS .EQ. 1  .AND.  NRUN .GT. 100 ) PRINT 3075
 3075 FORMAT( T2,'JADELG.LOAD (LGECOR9) VERSION 02.08.88 CALLED',
     +   ' FOR DATA' )

      IF ( ICALLS .EQ. 1  .AND.  NRUN .LT. 100 .AND. MCTYP .LE. 1 )
     +   PRINT 3076
 3076 FORMAT( T2,'JADELG.LOAD (LGECOR9) VERSION 02.08.88 CALLED',
     +   ' FOR MEIER-MAGNUSSEN MC-DATA' )

      IF ( ICALLS .EQ. 1  .AND.  NRUN .LT. 100 .AND. MCTYP .GT. 1 )
     +   PRINT 3077
 3077 FORMAT( T2,'JADELG.LOAD (LGECOR9) VERSION 02.08.88 CALLED',
     +   ' FOR SHOWER-MC' )
C
C  INITIAL SET UP
C
      BE     = RDATA(1)
      ENERGY = RDATA(2)
C
      COSX   = RDATA(9)
      COSY   = RDATA(10)
      COSZ   = RDATA(11)
      PHI    = RDATA(4)
      ZCO    = RDATA(5)
      RIDTRK = RDATA(8)
C     WRITE(6,900) BE,ENERGY,COSX,COSY,COSZ,PHI,ZCO,DEP
C900  FORMAT(10F10.3)
      IFLAG=0
C
      IF(IBE.EQ.0) GO TO 1
CCCCCCCCCCCCCCCCCCCCC
C
C  END CAP COUNTERS
C
CCCCCCCCCCCCCCCCCCCCC
C
C  EXTRAPOLATION TO THE FACE OF THE LIGHT GUIDE
C
      R=(THECPX(1)-DEP)/COSZ
      R=ABS(R)
      XX=XCO+R*COSX
      YY=YCO+R*COSY
C
      THETA=ACOS(COSZ)
      THETA=ABS(THETA)*57.2958
      IF(IBE.EQ.-1) THETA=180.0-THETA
C     WRITE(6,900) R,XX,YY,THETA
C
C   ITERATION FOR INCIDENT ENERGY
C
      ETEMP1=ENERGY
      ITERA=1
      JTERA=1
C
C
C  DISTANCE FROM THE CENTER OF LIGHT GUIDE
C
      X=XX/X0
      Y=YY/X0
      CALL LGNMEC(X,Y,ID)
C
C   CHECK IF HIT IS INSIDE THE COUNTER
C
C
      IF(ID.GT.0) GO TO 60
      X=XCO/X0
      Y=YCO/X0
      CALL LGNMEC(X,Y,ID)
C
      IF(ID.GT.0) GO TO 60
      IFLAG=2
      RETURN
C
   60 MB=(ID-1)/24 +1
      ID=MOD(ID-1,24)+1
      U=ECXLST(ID)
      V=ECYLST(ID)
      GO TO (4,5,6,7),MB
    4 X=U
      Y=V
      GO TO 8
    5 X=-V
      Y=U
      GO TO 8
    6 X=-U
      Y=-V
      GO TO 8
    7 X=V
      Y=-U
    8 R=0.1*SQRT((X-XX)*(X-XX)+(Y-YY)*(Y-YY))
      DDR=SQRT(XX*XX+YY*YY)-SQRT(X*X+Y*Y)
      IF(ABS(R).GT.1.E-10) DDR=0.1*DDR/R
C
C     ENERGY LOSS
C
      THICK=1.17*8.9/ABS(COSZ)
   10 CALL ENGLOS (ETEMP1,THICK,DELTAE,*80)
      ETEMP3=ETEMP1-DELTAE
      IF(ETEMP3.GT.0.) GO TO 32
      ETEMP1=ETEMP1+ETHRE
      GO TO 10
C
C     POS DEP
C
   32 CALL POSEND(ETEMP3,R,DDR,FAC,*80)
C
      ETEMP2=ENERGY/FAC+DELTAE
      EDIF=ABS(ETEMP2-ETEMP1)
      IF(EDIF.LE.ETHRE) GO TO 9
      ITERA=ITERA+1
      IF(ITERA.GE.10) GO TO 40
      ETEMP1=ETEMP2
      GO TO 10
C
   40 ITERA=1
      ETEMP1=0.5*(ETEMP1+ETEMP2)
      JTERA=JTERA+1
      IF(JTERA.LT.5) GO TO 10
      IFLAG=1
      RETURN
C
    9 RDATA(2)=ETEMP2
C      WRITE(6,940) ITERA,JTERA,ETEMP1,FAC,X,Y,R,DDR
C940   FORMAT(2I4,10F10.3)
      RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  BARREL PART COUNTERS
C
CCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  EXTRAPOLATION TO THE FACE OF LIGHT GUIDE
C
    1 R=RADIUS(3)+DEP
      COSPHI=COS(PHI)
      SINPHI=SIN(PHI)
C       X Y POSITION AT THE SHOWER MEDIAN.
      X=R*COSPHI
      Y=R*SINPHI
C
C     DISTANCE FROM THE SHOWER MAX POINT TO THE EXTRAPOLATED POINT
C           AT THE END FACE OF LEAD GLASS
      A=COSX*COSX+COSY*COSY
      B=X*COSX+Y*COSY
      C=X*X+Y*Y-RADIUS(4)*RADIUS(4)
      RDIF=(SQRT(B*B-A*C)-B)/A
C      X Y Z POSITION AT THE END FACE OF LG
      X=X+RDIF*COSX
      Y=Y+RDIF*COSY
CCC   Z=ZCO+RDIF*COSZ
C     WRITE(6,900) COSPHI,SINPHI,X,Y,Z,A,B,C,RDIF
C
C  DISTANCE FROM THE CENTER OF LIGHT GUIDE
C
CCC   Z=Z-ZEND(1)+0.5*ZGAP(1)
CCC   PERIOD=ZWID(1)+ZGAP(1)
CCC   IZ=Z/PERIOD+1
CCC   IF(IZ.LE.1) IZ=2
CCC   IF(IZ.GE.32) IZ=31
CCC   Z=Z-PERIOD*(IZ-0.5)
C
      THETA=ATAN2(Y,X)
      IF(THETA.LT.0.) THETA=THETA+PAI2
      STEP=PAI2/84.
      ITH=THETA/STEP
CCC   THETA=(THETA-(ITH+0.5)*STEP)*RADIUS(4)
C
CCC   R=0.1*SQRT(Z*Z+THETA*THETA)
CCCCCCC    CCC ARE NEEDED ONLY FOR POSITION CORRECTION.....
C
C       INCIDENT ANGLE
      ANGNOR=(ITH+0.5)*STEP
      COSNOR=COS(ANGNOR)
      SINNOR=SIN(ANGNOR)
C---     COSX = COSPHI*SINTET, COSNOR = COSPHI
C---     COSY = SINPHI*SINTET, COSNOR = SINPHI
      THETA=COSNOR*COSX+SINNOR*COSY
C        THETA = SIN(THETA) HERE

C     ABSORBER THICKNESS TRAVERSED BY THE PARTICLE.
C     D=7.88/ABS(THETA)    <--- OLD VERSION
      D=0.97*8.9/ABS(THETA)
      THETA=ACOS(THETA)*57.2958
C---       THETA = PI/2 - THETA = ANGLE WITH NORMAL OF CATHODE PLANE
C     WRITE(6,910) ITH,ANGNOR,COSNOR,SINNOR,THETA,D
C910  FORMAT(I4,10F12.4)
      IF(D.LT.50.) GO TO 70
      IFLAG=3
      RETURN
 70   CONTINUE
C
C---     CALCULATE RING # USING CORRECTED COS THETA
C
      ZHELP = COSZ / SQRT(1. - COSZ**2) * 1100.
      ZDIST = ZHELP - ZEND(1)
      NRING = INT ( ZDIST / ( ZGAP(1) + ZWID(1) ) )
C
C  - - - - - - - - - - - ITERATION - - - - - - - - - - - - - - -
C
      ITERA = 1
      EINC  = ENERGY

C---     ITERATION LOOP:

 21   CONTINUE

C===                                 SEPERATE PHOTONS: NO TRACK LINKED
      IF ( NIDTRK .EQ. 0 ) GO TO 23
C---                                        CHARGED CLUSTERS HERE
C
C---                              ENERGY LOSS OF ELECTRONS IN THE COIL :
C
      CALL ENGLOS ( EINC, D, DELTAE, *80 )
C
      EEXP=EINC-DELTAE
      IF ( EEXP .GT. 0. ) GO TO 22
      EINC = EINC + ETHRE
      GO TO 21
C
 22   CONTINUE
C              ANGLE DEPENDENCE
C
      AFAC = 1.
      CALL ANGBAR ( EEXP, THETA, AFAC, *80 )
      EEXP = EEXP * AFAC
C
C              FINE POSITION DEPENDENCE FOR BHABHAS
C
      PFAC = 1.
      PFAC = BRLGN ( EINC, RDATA(11), RDATA(4) )
      EEXP = EEXP * PFAC
      GO TO 26

C---                                       PHOTONS HERE
 23   CONTINUE
C---                                  BRANCH FOR MC: SHOWER VS. MEI-MAG
      IF ( NRUN .LT. 100 .AND. MCTYP .LE. 1 ) GOTO 24
C---                                            DATA AND SHOWER-MC HERE
      EEXP = EINC

      EFAC   = 1.0
      FACTOR = 1.0
C---                ENCORR FOR COIL, LEAKAGE, ANGULAR DEPENDENCE,
C                              READOUT THRESAHOLD FOR PHOTONS
      CALL ENCORR ( EINC, COSZ, EFAC  )
C--                        DISTINGUISH 1-BLOCK FROM MANY-BLOCK PHOTONS:
      CALL THCOFA ( 1000. * EINC, NRING, NRBLOC, FACTOR )

      EEXP = EEXP * EFAC * FACTOR

      GOTO 26
C
 24   CONTINUE
C---                    MEI MAG MC HERE
C
C---                              ENERGY LOSS IN THE COIL :
C
      CALL ENGLOS ( EINC, D, DELTAE, *80 )
C
      EEXP=EINC-DELTAE
      IF ( EEXP .GT. 0. ) GO TO 25
      EINC = EINC + ETHRE
      GO TO 24
C
 25   CONTINUE
C
C
C              CORRECTION FOR READOUT THRESHOLD
C              RTH = ENERGY LOST / ENERGY SEEN
C
      CALL THCORR ( EINCLG, NRING, RTH )
      EEXP = EEXP * ( 1. - RTH )
C---                           1-BLOCK-PHOTONS EXTRA:
      FACTOR = 1.
      CALL THCOFA ( EINCLG, NRING, NRBLOC, FACTOR )
      EEXP = EEXP * FACTOR
C
C              ANGLE DEPENDENCE
C
      AFAC = 1.
      CALL ANGBAR(EEXP,THETA,AFAC,*80)
      EEXP= EEXP*AFAC
C

 26   CONTINUE
C------------------------
C              ITERATION CONVERGENT ?
C
      EDIF = ENERGY - EEXP
      IF ( EINC .GT. 0.350  .AND.  ABS(EDIF).LE.ETHRE) GO TO 20
      IF ( EINC .LE. 0.350  .AND.  ABS(EDIF) / EINC .LE. 0.03 ) GOTO 20

C
C              MAX 10 ITERATIONS
C
      ITERA=ITERA+1
      IF(ITERA.GT.10) GO TO 50
C
C              NEXT ITERATION STEP
C
      EINC = EINC + EDIF
      GO TO 21
C
C  - - - - - - - - - - - END OF ITERATION - - - - - - - -
C
   50 CONTINUE
      PRINT 7122, ENERGY, EINC, COSZ, NRUN, NEVE
 7122 FORMAT (T2,'ITERATION OVERFLOW IN LGECOR: ESEEN, ECALC = ',
     +   2F10.4, ' ,COS THETA = ',F7.3,
     +    ' ,RUN ',0P,I5, ' ,EVENT ',I5)
      IFLAG=1
      RETURN
C
C              ITERATION SUCCESSFULL:
C
 20   CONTINUE
C
      RDATA(2)=EINC
C
CC      WRITE(6,10100) ENERGY,RDATA(4),RDATA(11),D,
CC     *  EINC,DELTAE,AFAC,PFAC,ITERA
CC10100 FORMAT(' <LGECOR> E',F8.3,', PHI',F6.3,', COSZ',F6.3,', D',F8.3,
CC     *  ', EINC',F8.3,', ELOSS',F6.3,', AFAC',F6.3,', PFAC',F6.3,
CC     *  ', ITERA',I3)
      RETURN
C
C     ILLEGAL RETURN
C
   80 RDATA(2)=ENERGY
      RETURN
C************************************
      ENTRY LGECR0(NVR)
C
C  VERSION NUMBER (DATE)   87  5 22
C
      DATA IVR/870522/
      NVR=IVR
      RETURN
      END