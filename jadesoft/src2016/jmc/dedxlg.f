C   18/03/81 103201253  MEMBER NAME  DEDXLG   (S)           FORTRAN
      FUNCTION DEDXLG(PMAG)
      IMPLICIT INTEGER*2 (H)
      DIMENSION COEFFS(3,6)
      DATA COEFFS/
     1   0.15717411, 0.00000000, 0.84282607,
     1   0.00230000, 0.03209798, 0.96560210,
     1   0.31167048, 0.00000000, 0.68833023,
     1   0.29285443, 0.29387236, 0.41327393,
     1   0.40096831, 0.17275548, 0.42627639,
     1   0.52804971, 0.12447029, 0.34748018/
      DATA P0/0.9500/
      DATA S0/0.2375/
      DATA S1/0.6000/
C---
      XMAX=PMAG/400.
      IF(PMAG.GT.600.) GO TO 1001
C---
C---     UNDER 600 MEV/C
C---
      DEDX=XMAX*(1.-SQRT(RN(DUMMY)))
      GO TO 2000
 1001 CONTINUE
      IBIN=1
      IF(PMAG.GT. 700.) IBIN=2
      IF(PMAG.GT. 900.) IBIN=3
      IF(PMAG.GT.1250.) IBIN=4
      IF(PMAG.GT.1950.) IBIN=5
      IF(PMAG.GT.2850.) IBIN=6
      IF(PMAG.GT.3750.) IBIN=7
   10 CONTINUE
      GO TO (1,2,3,4,5,6,7),IBIN
    1 CONTINUE
C---
C---     600 TO 700 MEV/C
C---
      P1=0.4
      C2=(PMAG-600.)*COEFFS(2,1)/100.
      C3=(PMAG-600.)*COEFFS(3,1)/100.
      C1=1.-C2-C3
      GO TO 500
    2 CONTINUE
C---
C---     700 TO 900 MEV/C
C---
      P1=0.4
      C2=COEFFS(2,1)+(PMAG-700.)*(COEFFS(2,2)-COEFFS(2,1))/200.
      C3=COEFFS(3,1)+(PMAG-700.)*(COEFFS(3,2)-COEFFS(3,1))/200.
      C1=1.-C2-C3
      GO TO 500
    3 CONTINUE
C---
C---     900 TO 1250 MEV/C
C---
      P1=0.4+0.8*(PMAG-900.)/1050.
      C2=COEFFS(2,2)+(PMAG-900.)*(COEFFS(2,3)-COEFFS(2,2))/350.
      C3=COEFFS(3,2)+(PMAG-900.)*(COEFFS(3,3)-COEFFS(3,2))/350.
      C1=1.-C2-C3
      GO TO 500
    4 CONTINUE
C---
C---     1250 TO 1950 MEV/C
C---
      P1=0.4+0.8*(PMAG-900.)/1050.
      C2=COEFFS(2,3)+(PMAG-1250.)*(COEFFS(2,4)-COEFFS(2,3))/700.
      C3=COEFFS(3,3)+(PMAG-1250.)*(COEFFS(3,4)-COEFFS(3,3))/700.
      C1=1.-C2-C3
      GO TO 500
    5 CONTINUE
C---
C---     1950 TO 2850 MEV/C
C---
      P1=1.2
      C2=COEFFS(2,4)+(PMAG-1950.)*(COEFFS(2,5)-COEFFS(2,4))/900.
      C3=COEFFS(3,4)+(PMAG-1950.)*(COEFFS(3,5)-COEFFS(3,4))/900.
      C1=1.-C2-C3
      GO TO 500
    6 CONTINUE
C---
C---     2850 TO 3750 MEV/C
C---
      P1=1.2
      C2=COEFFS(2,5)+(PMAG-2850.)*(COEFFS(2,6)-COEFFS(2,5))/900.
      C3=COEFFS(3,5)+(PMAG-2850.)*(COEFFS(3,6)-COEFFS(3,5))/900.
      C1=1.-C2-C3
      GO TO 500
    7 CONTINUE
C---
C---     OVER 3750 MEV/C
C---
      P1=1.2
      C2=COEFFS(2,6)
      C3=COEFFS(3,6)
      C1=1.-C2-C3
C---
  500 CONTINUE
      X=RN(DUMMY)
      IF(X.GT.C1) GO TO 501
      DEDX=XMAX*(1.-SQRT(RN(DUMMY)))
      GO TO 1000
  501 CONTINUE
      IG=0
      G=0.
  600 CONTINUE
      IG=IG+1
      G=G+RN(DUMMY)
      IF(IG.LT.12) GO TO 600
      G=G-6.
      IF(X.GT.(C1+C2)) GO TO 502
      DEDX=P0+S0*G
      GO TO 1000
  502 CONTINUE
      DEDX=P1+S1*G
 1000 CONTINUE
      IF(DEDX.LT.0) GO TO 10
      IF(DEDX.GT.XMAX) GO TO 10
 2000 CONTINUE
      DEDXLG=DEDX
      RETURN
      END