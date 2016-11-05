C   10/01/82 206251143  MEMBER NAME  WFIT     (S)           FORTRAN
      SUBROUTINE FCN(NPAR,G,CHI2,X,IFLAG)
      DIMENSION G(1),X(1)
      DIMENSION S(20),CTH(20),Y(20),ERR(20)
      DIMENSION ST(20),R(20),ERRT(20)
C
      GO TO (10,20,30,40),IFLAG
C
   10 CONTINUE
      BCHI = 4.49E-5
      ZMZ0 = 90.
      DO 11 I=1,100
      READ(5,501) S(I),CTH(I),Y(I),ERR(I)
  501 FORMAT(2F10.0,2F10.3)
      IF(Y(I).LT.0.) GO TO 12
      ERR(I) = ERR(I)/Y(I)
      Y(I) = Y(I)/5.18
      ERR(I) = ERR(I)*Y(I)
      PRINT 502, S(I),CTH(I),Y(I),ERR(I)
  502 FORMAT('  DATA READ', 2F10.0,2F10.3)
   11 CONTINUE
   12 NMAX=I-1
C
C   READ TOTAL CROSS SECTION
      PRINT 504
  504 FORMAT(//'  NOW TOTAL CROSS SECTION '/)
      DO   13   I=1,3
      READ(5,501) ST(I),CZER,R(I),ERRT(I)
      ERRT(I) = ERRT(I)/R(I)
      R(I) = R(I)/86.86
      ERRT(I) = ERRT(I)*R(I)
      PRINT 502, ST(I),CZER,R(I),ERRT(I)
   13 CONTINUE
      PRINT 506
  506 FORMAT(//)
C
      X(1) = 0.006
      X(2) = 1.
      DO 15 I=1,NMAX
      PROP = 1./(S(I)/ZMZ0**2-1.)
      ACHI = S(I)*BCHI*PROP
      CTH1 = 1+CTH(I)**2
      F1 = 1.+2.*X(1)*ACHI+(X(1)+X(2))**2*ACHI*ACHI
      F2 =    4.*X(2)*ACHI+8.*X(1)*X(2)*ACHI*ACHI
      FUNK = F1*CTH1+F2*CTH(I)
      CHI=FUNK-Y(I)
      PRINT 503,I,CTH(I),Y(I),PROP,ACHI,F1,F2,FUNK,CHI
  503 FORMAT(' FUNK',I5,4F10.3,2E12.5,4F10.5)
      CHI2=CHI2+CHI**2/ERR(I)**2
   15 CONTINUE
C
      DO   16   I=1,3
      PROP = 1./(ST(I)/ZMZ0**2-1.)
      ACHI = ST(I)*BCHI*PROP
      FUNK = 1.+2.*X(1)*ACHI+(X(1)+X(2))**2*ACHI*ACHI
      CHI=FUNK-R(I)
      PRINT 505,I,ST(I),R(I),ACHI,PROP,FUNK,CHI
  505 FORMAT(' TOT CROS',I5,2F10.3,2E12.5,2F10.3)
   16 CONTINUE
      RETURN
C
   20 RETURN
C
   40 CONTINUE
      CHI2=0.
      DO 41 I=1,NMAX
      PROP = 1./(S(I)/ZMZ0**2-1.)
      ACHI = S(I)*BCHI*PROP
      CTH1 = 1+CTH(I)**2
      F1 = 1.+2.*X(1)*ACHI+(X(1)+X(2))**2*ACHI*ACHI
      F2 =    4.*X(2)*ACHI+8.*X(1)*X(2)*ACHI*ACHI
      FUNK = F1*CTH1+F2*CTH(I)
      CHI=FUNK-Y(I)
      CHI2=CHI2+CHI**2/ERR(I)**2
   41 CONTINUE
C
      DO   45   I=1,3
      PROP = 1./(ST(I)/ZMZ0**2-1.)
      ACHI = ST(I)*BCHI*PROP
      RT = 1.+2.*X(1)*ACHI+(X(1)+X(2))**2*ACHI*ACHI
      CHI=RT-R(I)
      CHI2=CHI2+CHI**2/ERRT(I)**2
   45 CONTINUE

      RETURN
C
   30 CONTINUE
C
      DO 31 I=1,NMAX
      PROP = 1./(S(I)/ZMZ0**2-1.)
      ACHI = S(I)*BCHI*PROP
      CTH1 = 1+CTH(I)**2
      F1 = 1.+2.*X(1)*ACHI+(X(1)+X(2))**2*ACHI*ACHI
      F2 =    4.*X(2)*ACHI+8.*X(1)*X(2)*ACHI*ACHI
      CALC = F1*CTH1+F2*CTH(I)
      WRITE(6,300) CTH(I),CALC,Y(I),ERR(I)
  300 FORMAT(10E11.3)
   31 CONTINUE
      RETURN
      END
