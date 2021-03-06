C   19/02/80 808021752  MEMBER NAME  ENLOSG   (SOURCE)      FORTRAN
C
C      PARAMETERS CALCULATED BY EGS3          D.PITZL 21.5.87
C
C***************************************************************
      SUBROUTINE  ENLOSG ( E, D, DE, * )
C***************************************************************
C
C      ENERGY LOSS OF PHOTONS IN ALUMINIUM
C
C     INPUT  : E  = INPUT ENERGY IN GEV
C              D  = THICKNESS OF ALUMINUM ABSORBER IN CENTIMETER
C     OUTPUT : DE = ENERGY LOSS IN GEV
C
C                        WRITTEN BY H.TAKEDA  15/02/80
C                        TYPED BY Y.WATANABE  19/02/80
C
C    VARIABLES: BENG = LIST OF INCIDENT ENERGIES IN GEV
C               BTHK = LIST OF ALU THICKNESSES IN CM
C               HCOR = MATRIX OF ENERGY LOSS IN MEV FOR EACH
C                      THICKNESS AND ENERGY
C                      20 ENERGIES FOR EACH THICKNESS
C
C
C
      DIMENSION BENG(20),BTHK(7)
      INTEGER*2 HCOR(20,7)
      DATA BENG/ 0.0,  0.1,  0.2,  0.3,  0.5,  0.7,  1.0,  2.0,  3.0,
     1           4.0,  5.0,  6.0,  7.0,  9.0, 11.0, 13.0, 15.0, 17.0,
     2          19.0, 99.0/
      DATA BTHK/ 0.0,  7.0,  9.0, 11.0, 13.0, 15.0, 99.0/
C
      DATA HCOR/ 20 * 0 ,
     2    0, 12, 14, 16, 17, 19, 19, 21, 22, 23,
     2   23, 23, 23, 23, 26, 26, 26, 27, 30,  0,
     3    0, 18, 22, 26, 28, 32, 32, 36, 39, 40,
     3   42, 42, 42, 44, 47, 47, 47, 52, 55,  0,
     4    0, 25, 32, 39, 39, 48, 49, 57, 62, 65,
     4   68, 69, 70, 72, 77, 78, 78, 89, 90,  0,
     6    0, 31, 41, 52, 54, 67, 70, 84, 93, 97,
     6  103,105,108,112,124,124,124,141,141,  0,
     6    0, 37, 52, 65, 71, 89, 94,116,130,139,
     6  148,152,158,164,183,184,185,209,212,  0,
     7   20 * 0 /
C
      DATA IERR1,IERR2,IENTRY,IERCNT,IBTHK,IBENG/3*0,5,7,20/
C     EXTRAPOLATE THE THICKNESS OF ALUMINUM
      DE=0.
C
C---- INITIALIZE
C     ENERGY LOSS FOR THICKNESS #7 = 99CM CALCULATED BY LINEAR
C     EXTRAPOLATION. NOT USED AT JADE, JUST FOR SAFETY.
C
      IF(IENTRY.GE.1) GO TO 2
      DO 1 I=1,IBENG
      F=(BTHK(IBTHK)-BTHK(IBTHK-2))/(BTHK(IBTHK-1)-BTHK(IBTHK-2))
      HCOR(I,IBTHK)=HCOR(I,IBTHK-2)+F*(HCOR(I,IBTHK-1)-HCOR(I,IBTHK-2))
1     CONTINUE
C     WRITE(6,900) HCOR
C900  FORMAT(20 I6)
      PRINT 9123
 9123 FORMAT ( T2,'JADELG.LOAD (ENLOSG) CALLED FROM LGECOR' )
      IENTRY=1
C
C       CHECK BAD INPUTS
2     IF (E .GE. 0.0 .AND. E .LE. 99.) GO TO 3
      IERR1=IERR1+1
      IF (IERR1 .GT. IERCNT) RETURN1
      WRITE(6,600) E
600   FORMAT('0 **BAD  ENERGY INTO LGECOR(ENGLOS) E=',F10.3)
      RETURN1
C
3     IF (D .GE. 0.0 .AND. D .LE. 99.) GO TO 4
      IERR2=IERR2+1
      IF(IERR2.GT.IERCNT) RETURN1
      WRITE(6,610) D
610   FORMAT('0 **BAD THICKNESS INTO LGECOR(ENGLOS) D=',F10.3)
      RETURN1
C
4     DO 5 I=2,IBENG
      IF(E.LT.BENG(I)) GO TO 6
5     CONTINUE
6     DO 7 J=2,IBTHK
      IF(D.LT.BTHK(J)) GO TO 8
7     CONTINUE
C
8     EPORT=(E-BENG(I-1))/(BENG(I)-BENG(I-1))
      TPORT=(D-BTHK(J-1))/(BTHK(J)-BTHK(J-1))
C
      FACT1=HCOR(I-1,J-1)+EPORT*(HCOR(I,J-1)-HCOR(I-1,J-1))
      FACT2=HCOR(I-1,J  )+EPORT*(HCOR(I,J  )-HCOR(I-1,J  ))
C
      DE=FACT1+TPORT*(FACT2-FACT1)
      DE=DE/1000.
      IF(DE.GT.E) DE=E
      RETURN
      END
