C   19/02/80 808021752  MEMBER NAME  ENGLOS   (SOURCE)      FORTRAN
C
C
C    PARAMETERS FOR 100-700 MEV ELECTRONS FROM EGS4 ADDED
C                                                      D.PITZL 30.06.87
C**********************************************************************
      SUBROUTINE ENGLOS ( E, D, DE, * )
C**********************************************************************
C
C     ENERGY LOSS OF ELECTRONS IN ALUMINIUM.
C
C     INPUT  : E  = INPUT ENERGY IN GEV
C              D  = THICKNESS OF ALUMINUM ABSORBER IN CENTIMETER
C     OUTPUT : DE = ENERGY LOSS IN GEV
C
C                        WRITTEN BY H.TAKEDA  15/02/80
C                        TYPED BY Y.WATANABE  19/02/80
C
      DIMENSION  BENG(20) , BTHK(7)
      INTEGER*2 HCOR(20,7)
C                           INCIDENT ENERGIES IN GEV
      DATA  BENG / 0.0, 0.1, 0.2,  0.3,  0.5,  0.7,  1.0,  2.0,   3.0,
     1   4.0, 5.0, 6.0, 7.0, 9.0, 11.0, 13.0, 15.0, 17.0, 19.0, 100.0/
C                            THICKNESS ALU IN CM
      DATA  BTHK / 0.0, 7.0, 9.0, 11.0, 13.0, 15.0, 100.0/
C                             ENERGY LOSS IN MEV
      DATA  HCOR / 20 * 0,
     2  0,  38,  42,  44,  48,  49,  55,  65,  70,  78,
     +                          84,  90,  95,             7*  96,
     3  0,  49,  57,  62,  69,  72,  76,  90, 102, 112,
     +                         118, 126, 130, 134,        6* 136,
     4  0,  59,  72,  80,  92,  97, 108, 130, 146, 157,
     +                         168, 180, 190, 196, 200,   5* 202,
     5  0,  64,  87,  99, 118, 126, 134, 170, 194, 212,
     +                         228, 238, 245, 256, 264,   5* 268,
     6  0,  71,  99, 117, 144, 156, 172, 218, 258, 278,
     +                         300, 320, 336, 350, 360,   5* 362,
     7   20 * 0/
      DATA IERR1,IERR2,IENTRY,IERCNT,IBTHK,IBENG/3*0,5,7,20/
C     EXTRAPOLATE THE THICKNESS OF ALUMINUM
      DE=0.
C---- INITIALIZE
      IF(IENTRY.GE.1) GO TO 2
      DO 1 I=1,IBENG
      F=(BTHK(IBTHK)-BTHK(IBTHK-2))/(BTHK(IBTHK-1)-BTHK(IBTHK-2))
      HCOR(I,IBTHK)=HCOR(I,IBTHK-2)+F*(HCOR(I,IBTHK-1)-HCOR(I,IBTHK-2))
1     CONTINUE
C     WRITE(6,900) HCOR
C900  FORMAT(16I6)
      PRINT 9123
 9123 FORMAT ( T2, 'JADELG.LOAD (ENGLOS) CALLED FROM LGECOR' )
      IENTRY=1
C
C       CHECK BAD INPUTS
2     IF(E.GE.0. .AND. E.LE.100.) GO TO 3
      IERR1=IERR1+1
      IF(IERR1.GT.IERCNT) RETURN1
      WRITE(6,600) E
600   FORMAT('0 **BAD  ENERGY INTO LGECOR(ENGLOS) E=',F10.3)
      RETURN1
C
3     IF(D.GE.0. .AND. D.LE.100.) GO TO 4
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
