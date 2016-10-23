C   31/08/78 704251604  MEMBER NAME  NEWDIR   (S)           FORTRAN
      SUBROUTINE NEWDIR(DIR,TH)
C
C---- CHANGE DIRECTION COSIGNS FOR BENDING ANGLE OF TH.
C
C     A.SATO   XX-09-77
C     LAST MODIFICATION  19-11-81  13:30   S.YAMADA
C
      DIMENSION DIR(3),A(3),B(3)
      REAL *8 A,B,AL,AM,AN,CONRVS,CON,ALAM2,ALAM,ALMN(3)
      EQUIVALENCE (ALMN(1),AL),(ALMN(2),AM),(ALMN(3),AN)
C
      CTH=COS(TH)
      STH=SIN(TH)
      GO TO 1
C
C*******
C      MODIFIED  18-05-78  17:20
C
      ENTRY NEWDR1(DIR,CTH,STH)
C
C---- MODIFIED  18-11-81   13:25
C     CHECK CTH AND STH
      IF( ABS(CTH).GT.1.0 .OR. ABS(STH).GT.1.0 ) GO TO 92
      CS2 = CTH*CTH+STH*STH
      IF( CS2.LT.0.999 .OR. CS2.GT.1.001 ) STH = SQRT(1.0-CTH*CTH)
C
C**GET RANDOM AZIMUTHAL COS, SIN
    1 XR1=RN(DAM)
      XR2=RN(DAM)
      IF(XR1.EQ.0. .AND. XR2.EQ.0.) GO TO 90
      XR1=2.*(XR1-.5)
      X=XR1*XR1+XR2*XR2
      IF(X-1.) 2,2,1
    2 C=(XR1*XR1-XR2*XR2)/X
      S=2.*XR1*XR2/X
      GO TO 8
C***********************************************************
      ENTRY NEWDR2(DIR,CTH,STH,C,S)
C
C     CHECK CTH,STH, C AND S
      IF( ABS(CTH).GT.1.0 .OR. ABS(STH).GT.1.0 ) GO TO 93
      CS2 = CTH*CTH+STH*STH
      IF( CS2.LT.0.999 .OR. CS2.GT.1.001 ) STH = SQRT(1.0-CTH*CTH)
      CS2 = C*C+S*S
      IF( CS2.LT.0.999 .OR. CS2.GT.1.001 ) GO TO 94
C
    8 AL=DIR(1)
      AM=DIR(2)
      ALAM2 = AL**2+AM**2
      IF( ALAM2.LE.1.D0 ) GO TO 5
          ALAM = DSQRT(ALAM2)
          AL = AL/ALAM
          AM = AM/ALAM
          AN = 0.D0
          ALAM2 = 1.D0
          GO TO 6
    5 AN=DSQRT(1.D0-ALAM2)
      IF(DIR(3).LT.0.) AN = -AN
C---- CHECK ALMOST BACKWARD CASES
    6 CONRVS = AN+1.
      IF( CONRVS.GT.0.02) GO TO 3
      CONRVS = 0.5*ALAM2
      IF( CONRVS.GT.0. ) GO TO 3
      A(1) = 1.0
      A(2) = 0.
      A(3) = 0.
      B(1) = 0.
      B(2) = 1.
      B(3) = 0.
      GO TO 4
C
    3 CON=1./CONRVS
      A(1)=AL*AL*CON-1.
      A(2)=AL*AM*CON
      A(3)=AL
      B(1)=A(2)
      B(2)=AM*AM*CON-1.
      B(3)=AM
    4 SUM = 0.
        DO 10 I=1,3
        DIR(I)=ALMN(I)*CTH+(C*A(I)+S*B(I))*STH
   10   SUM = SUM+DIR(I)**2
C
      IF(SUM.GE.0.99 .AND. SUM.LE.1.01) GO TO 100
C
C*****  CORRECT THE ROUNDING ERROR  *******************
      DATA IERR/0/
      IERR = IERR+1
      TH2 = CTH**2+STH**2
      CS2 = C*C+S*S
C     IF(IERR.LE.10 .OR. SUM.LE.0. .OR. SUM.GT.1.1 )
      IF(IERR.LE.3 .AND.( SUM.LE.0. .OR. SUM.GT.1.1 ))
     $          WRITE(6,6000) DIR,SUM,AL,AM,AN,A,B,CTH,STH,TH2,C,S,CS2
 6000 FORMAT('0 ****  ERROR ,DIR=',3F10.4,'  SUM2=',E10.3,
     1       /'   AL,AM,AN=',3F10.4,'  A,B=',6E10.3,
     2       /'   CTH,STH,TH2,C,S,CS2=',6E11.3)
C
      IF(SUM.LE.0. .OR. CS2.LE.0.) GO TO 91
      SUM = SQRT(SUM)
        DO 20 K=1,3
   20   DIR(K) = DIR(K)/SUM
      GO TO 100
C*************************************************
C
C---- WARNING
   92 WRITE( 6,692 ) CTH,STH
  692 FORMAT('0 WRONG ANGLE FOR NEWDR1; CTH,STH=',2E12.4,'  <===ERROR=')
      GO TO 100
   93 WRITE( 6,693 ) CTH,STH
  693 FORMAT('0 WRONG ANGLE FOR NEWDR2; CTH,STH=',2E12.4,'  <===ERROR=')
      GO TO 100
   94 WRITE( 6,694 ) C,S
  694 FORMAT('0 WRONG ANGLE FOR NEWDR2; C,S=',2E12.4,'  <===ERROR=')
C
  100 RETURN
C
   90 WRITE(6,690)
  690 FORMAT('0**** 2 SUCCESSIVE RANDOM NUMBERS ARE 0.IN NEWDR1.***',
     1        /'  EMERGENCY STOP')
      GO TO 200
   91 WRITE(6,691) SUM,C,S,CS2
  691 FORMAT('0**SUM=0 OR  WRONG INPUT C,S FOR NEWDR2 ********',
     1       /' SUM=',E12.4,'   C,S,C*C+S*S=',3E12.4,
     2       /'   EMERGENCY STOP')
  200 STOP
      END
