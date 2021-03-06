C   11/06/79 C9100601   MEMBER NAME  LGECOR   (SOURCE)      FORTRAN
      SUBROUTINE LGECOR(ADATA,DEP,IFLAG)
C
C  THIS PROGRAM CALCULATE THE INCIDENT ENERGY
C  FROM THE MEASURED ENERGY.
C
C      05/3/1979     HIROSHI TAKEDA
C      LAST MODIFICATION   22/09/79
C      LAST MODIFICATION   04/10/79 Y.WATANABE
C       (END CAP CORRECTION IS RESTORED EXECPT FOR A FACTOR)
C  ADATA : DATA BUFFER OF THE FOLLOWING PARAMETERS.
C
C    IDATA(1)   0   BARREL
C                    -1   BOTTOM END CAP
C                    +1   TOP    END CAP
C    ADATA(2)   CLUSTER ENERGY IN GEV
C    ADATA(3)   SIGMA(ENERGY)
C    ADATA(4)   WEIGHTED AVERAGE PHI        BARREL
C                                       X        END CAP
C    ADATA(5)   WEIGHTED AVERAGE  Z         BARREL
C                                       Y        END CAP
C    ADATA(6)   SIGMA PHI (WEIGHTED)        BARREL
C                            X                   END CAP
C    ADATA(7)   SIGMA  Z (WEIGHTED)         BARREL
C                            Y                   END CAP
C    IDATA(8)   NUMBER OF CORRESPONDING INNER TRACKS
C    ADATA(9--11) DIRECTION COSIGNS CORRECTED FOR SHOWER DEPTH
C
C  DEP  :  SHOWER DEPTH IN MM
C  IFLAG   0    NORMAL RETURN
C          1    ITERATION OVERFLOW
C          2    HIT OUTSIDE END-CAP
C          3    TOO MUCH ENERGY LOSS
C
      DIMENSION ADATA(2)
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
      DATA ETHRE/0.01/ ,STEP/104.72/
C
C  INITIAL SET UP
C
      BE   =ADATA(1)
      ENERGY=ADATA(2)
      COSX  =ADATA(9)
      COSY  =ADATA(10)
      COSZ  =ADATA(11)
      PHI   =ADATA(4)
      ZCO   =ADATA(5)
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
      XXCO=XCO+R*COSX
      YYCO=YCO+R*COSY
C
      THETA=ARCOS(COSZ)
      THETA=ABS(THETA)*57.2958
      IF(IBE.EQ.-1) THETA=180.0-THETA
C
C   ITERATION FOR INCIDENT ENERGY
C
      ETEMP1=ENERGY
      ITERA=1
      JTERA=1
C
C  IMAGINARY DISPLACEMENT OF COORDINATES
C
   10 RDIS=0.0
      IF(THETA.LE.10.0) GO TO 30
      RDIS=24.0-6.3*ETEMP1
      IF(RDIS.LE.0.0) RDIS=0.0
      RDIS=RDIS/SQRT(1.0-COSZ*COSZ)
   30 XX=XXCO-RDIS*COSX
      YY=YYCO-RDIS*COSY
C
C  DISTANCE FROM THE CENTER OF LIGHT GUIDE
C
      X=XX/X0
      Y=YY/X0
      CALL LGNMEC(X,Y,ID)
C
C   CHECK IF HIT IS INSIDE THE COUNTER
C
      IF(ID.GT.0) GO TO 60
      X=XXCO/X0
      Y=YYCO/X0
      CALL LGNMEC(X,Y,ID)
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
    8 R=SQRT((X-XX)*(X-XX)+(Y-YY)*(Y-YY))
C
C  CORRECTION FACTOR
C
      IF(ETEMP1.GE.1.5) GO TO 2
      FAC=0.1518*ETEMP1+0.8926
      FAC=FAC-R*(0.002067*ETEMP1+0.000236)
      GO TO 3
C
2     FAC=0.0861*ETEMP1+0.9949
      FAC=FAC-R*(0.00092*ETEMP1+0.00209)
      IF(ETEMP1.LE.5.0) GO TO 3
C
C -- ENERGY CORRECTION FOR E > 5 GEV
C    ROUGHLY ESTIMATED FROM BHABHA ENERGY DISTRIBUTION
C       AT 11 GEV AND 15 GEV.
C
      EXX=ETEMP1-5.0
      FAC=FAC*(1.0-EXX*(0.058426-0.002238*EXX))
C
    3 ETEMP2=ENERGY/FAC
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
    9 ADATA(2)=ETEMP2
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
      X=R*COSPHI
      Y=R*SINPHI
C
      A=COSX*COSX+COSY*COSY
      B=X*COSX+Y*COSY
      C=X*X+Y*Y-RADIUS(4)*RADIUS(4)
      RDIF=(-B+SQRT(B*B-A*C))/A
C
      X=X+RDIF*COSX
      Y=Y+RDIF*COSY
      Z=ZCO+RDIF*COSZ
C
C  DISTANCE FROM THE CENTER OF LIGHT GUIDE
C
      Z=Z-ZEND(1)+0.5*ZGAP(1)
      PERIOD=ZWID(1)+ZGAP(1)
      IZ=Z/PERIOD+1
      IF(IZ.LE.1) IZ=2
      IF(IZ.GE.32) IZ=31
      Z=Z-PERIOD*(IZ-0.5)
C
      THETA=ATAN(Y/X)
      THETA=ABS(THETA)*RADIUS(4)
      ITH=THETA/STEP
      THETA=THETA-(ITH+0.5)*STEP
C
      R=SQRT(Z*Z+THETA*THETA)
      R=0.1*R
C
      THETA=COSPHI*COSX+SINPHI*COSY
      D=7.88/THETA
      THETA=ARCOS(THETA)*57.2958
C
      ALMA=3.43*D-11.7
C
      IF(ALMA.LT.1000.0) GO TO 70
      IFLAG=3
      RETURN
C
   70 ALMB=8.14*D-5.75
C
C  ITERATION
C
      ETEMP1=ENERGY
      ITERA=1
      JTERA=1
C
C  ENERGY LOSS IN ALUMINUM COIL
C
   21 DELTAE=(ALMA*ETEMP1+ALMB)*0.001
      IF(DELTAE.LE.0.0) DELTAE=0.0
      ETEMP3=ETEMP1-DELTAE
      IF(ETEMP3.GT.0.0) GO TO 22
      ETEMP1=ETEMP1+ETHRE
      GO TO  21
C
   22 THCRI=3.75*ETEMP3+16.25
      THETA1=THETA
      IF(THETA.GE.THCRI) THETA1=THCRI
C
      ATH=-0.000404*ETEMP3+0.0023
      BTH=-0.0000015*ETEMP3+0.0000715
      FAC=(ATH+BTH*THETA1)*THETA1
      IF(FAC.LE.0.0) FAC=0.0
      FAC=1-FAC
C-----
C  WE HAVE NO DATA OF THE POSITION DEPENDENCE
C  AT THE ENERGIES MORE THAN 5 GEV.
C  THE DATA AT 5 GEV IS USED HERE.
C
      ETEMP4=ETEMP3
      IF(ETEMP4.GE.5.0) ETEMP4=5.0
C
      FAC=FAC*(1-0.0139/(THETA+1.95)*ETEMP4*R)
C
      ETEMP2=ENERGY/FAC+DELTAE
      EDIF=ABS(ETEMP2-ETEMP1)
      IF(EDIF.LE.ETHRE) GO TO 20
      ITERA=ITERA+1
      IF(ITERA.GE.10) GO TO 50
      ETEMP1=ETEMP2
      GO TO 21
C
   50 ITERA=1
      ETEMP1=0.5*(ETEMP1+ETEMP2)
      JTERA=JTERA+1
      IF(JTERA.LT.5) GO TO 21
      IFLAG=1
      RETURN
C
   20 ADATA(2)=ETEMP2
      RETURN
C************************************
      ENTRY LGECR0(NVR)
C
C  VERSION NUMBER (DATE)   79 09 22
C
      DATA IVR/791004/
      NVR=IVR
      RETURN
      END
