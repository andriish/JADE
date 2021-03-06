C   17/02/82 311291152  MEMBER NAME  PLLIGD   (S)           FORTRAN
      SUBROUTINE PLLIGD(PL,X1,Y1,Z1,X2,Y2,Z2,IFLG)
C
C     PROGRAM WRITTEN BY KANZAKI
C     OUTPUT ;
C       PL = PATH LENGTH IN THE LIGHT GUIDE
C     INPUT  ;
C       X1,Y1,Z1 = COORDINATES OF THE HIT POINT AT THE FRONT SURFACE OF
C                    THE LEAD GLASS
C       X2,Y2,Z2 = COORDINATES OF THE HIT POINT AT THE BACK SURFACE OF
C                    THE LEAD GLASS
C       IFLG = 1 FOR IMPACT POINT IN ENDCAPS (INCLUDING GAP ENDCAP-CYL)
C       IFLG = 2 FOR IMPACT POINT IN CYLINDER
C
C       --->  YOU CAN GET ABOVE PARAMETERS BY RUTRCK IN JADEGS
C       --->  COPIED FROM F22MIN.LGMC.S   BY W.BARTEL ON 29/11/83
C
      DATA ZWID1/ 103.2/,PHWID1,PHWID2/ 80.0, 102.8/
      DATA PHWID0/ 0.0748/
      DATA RADI1,RADI2/ 1100.0, 1400.0/
      DATA ZEND1/ 1705.45/,ZGAP1/ 3.5/
      DATA PI/ 3.1415927/
C
      DATA ALLIGD,RLIGD/ 60., 37.5/
C
C
      PL=0.
C
      IF(IFLG.NE.1 .AND. IFLG.NE.2) RETURN
      IF(IFLG.NE.2) RETURN
C
      PHI=ATAN2(Y2,X2)
      IF(PHI.LT.0.) PHI=PHI+2.*PI
C     RL2=RADI2*PHI
      IC2=INT(PHI/PHWID0)+1
      ZWIDG=ZWID1+ZGAP1
      IR2=INT((Z2+ZEND1)/ZWIDG)+1
C
CCC   WRITE(6,630) IR2,IC2
  630 FORMAT(1H ,'RING,COUTER # = ',2I5)
C
      PHI0=(FLOAT(IC2-1)+0.5)*PHWID0-0.5*PI
      IF(PHI0.LT.0.) PHI0=PHI0+2.*PI
      Z0=(FLOAT(IR2-1)+0.5)*ZWIDG-ZEND1
CCC   WRITE(6,640) PHI0,Z0
  640 FORMAT(1H ,'PHI0,Z0 = ',2E15.7)
C
      R1=SQRT(X1**2+Y1**2)
      R2=SQRT(X2**2+Y2**2)
CCC   WRITE(6,650) R1,R2
  650 FORMAT(1H ,'R1,R2 = ',2E15.7)
C
      X0=-RADI2*SIN(PHI0)
      Y0=RADI2*COS(PHI0)
CCC   WRITE(6,660) X0,Y0
  660 FORMAT(1H ,'X0,Y0 = ',2E15.7)
C
C***
      XR0=X0*COS(PHI0)+Y0*SIN(PHI0)
      YR0=-X0*SIN(PHI0)+Y0*COS(PHI0)-RADI2
CCC   WRITE(6,670) XR0,YR0
  670 FORMAT(1H ,'XR0,YR0 = ',2E15.7)
C***
C
      XR1=X1*COS(PHI0)+Y1*SIN(PHI0)
      YR1=-X1*SIN(PHI0)+Y1*COS(PHI0)-RADI2
      ZR1=Z1-Z0
C
      XR2=X2*COS(PHI0)+Y2*SIN(PHI0)
      YR2=-X2*SIN(PHI0)+Y2*COS(PHI0)-RADI2
      ZR2=Z2-Z0
C
CCC   WRITE(6,600) XR1,YR1,ZR1,XR2,YR2,ZR2
  600 FORMAT(1H ,'**** AFTER COORDINATE TRANSFORMATION ****'/
     *       1H ,'XR1,YR1,ZR1 = ',3E15.7/
     *       1H ,'XR2,YR2,ZR2 = ',3E15.7)
C
      A=ZR2-ZR1
      B=XR1-XR2
      C=XR1*ZR2-XR2*ZR1
C
      D=RLIGD**2*(A**2+B**2)-C**2
C
CCC   WRITE(6,680) A,B,C,D
  680 FORMAT(1H ,'A,B,C = ',3E15.7/
     *       1H ,'D     = ',E15.7)
C
      IF(D.LE.0.) RETURN
C
      XCRS1=(A*C+ABS(B)*SQRT(D))/(A**2+B**2)
      ZCRS1=(B*C+ABS(A)*SQRT(D))/(A**2+B**2)
      YCRS1=(YR2-YR1)*(XCRS1-XR1)/(XR2-XR1)+YR1
C
      XCRS2=(A*C-ABS(B)*SQRT(D))/(A**2+B**2)
      ZCRS2=(B*C-ABS(A)*SQRT(D))/(A**2+B**2)
      YCRS2=(YR2-YR1)*(XCRS2-XR1)/(XR2-XR1)+YR1
C
CCC   WRITE(6,620) XCRS1,YCRS1,ZCRS1,XCRS2,YCRS2,ZCRS2
  620 FORMAT(1H ,'**** SOLUTION !!! ****'/
     *       1H ,'XCRS1,YCRS1,ZCRS1 = ',3E15.7/
     *       1H ,'XCRS2,YCRS2,ZCRS2 = ',3E15.7)
C
      IF((YCRS1.LT.0. .AND. YCRS2.LT.0.) .OR.
     *   (YCRS1.GT.ALLIGD .AND. YCRS2.GT.ALLIGD)) RETURN
C
      IF(YCRS1.LE.YCRS2) GO TO 100
      XTMP=XCRS1
      YTMP=YCRS1
      ZTMP=ZCRS1
      XCRS1=XCRS2
      YCRS1=YCRS2
      ZCRS1=ZCRS2
      XCRS2=XTMP
      YCRS2=YTMP
      ZCRS2=ZTMP
C
  100 IF(YCRS1.GE.0.) GO TO 110
      XCRS1=-YR1*(XR2-XR1)/(YR2-YR1)+XR1
      YCRS1=0.
      ZCRS1=-YR1*(ZR2-ZR1)/(YR2-YR1)+ZR1
C
  110 IF(YCRS2.LT.ALLIGD) GO TO 120
      XCRS2=(ALLIGD-YR1)*(XR2-XR1)/(YR2-YR1)+XR1
      YCRS2=ALLIGD
      ZCRS2=(ALLIGD-YR1)*(ZR2-ZR1)/(YR2-YR1)+ZR1
C
  120 PL=SQRT((XCRS2-XCRS1)**2+(YCRS2-YCRS1)**2+(ZCRS2-ZCRS1)**2)
C
      RETURN
      END
