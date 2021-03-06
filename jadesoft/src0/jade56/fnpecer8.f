C   23/06/78 704211041  MEMBER NAME  FNPECER8 (S)           FORTRAN
      SUBROUTINE NPECER(JBE,Z,COST,PENUM,A)
C
CCC   S.YAMADA    12-12-77
CCC   LAST MODIFICATION  13-11-78  10:50
CCC   ----- NEW PARAMETERS CALCULATED BY A.SATO ARE USED NOW -----
CCC   ----- SEE THE TELEX 13.NOV.78 BY A.SATO. -----
C
C   UPDATED WITH RESULT FROM CERSF56.NR2    18.4.87    J.O.
C
C   DATA FROM CERTST5.NR1     DONE WITH N=1.6725
C    (NEW BARREL DATA ONLY, B-ARRAY LATER PART FOR ENDCAP STAYS SAME)
C   THE OLD EMPIRICAL FORMULA IS ONLY USED IN ENDCAPS
C   FOR THE BARREL AN INTERPOLATION IS USED, IS MORE ACCURATE
C
C
C---- CALCULATE NUMBER OF PHOTOELECTRONS BY CERECKOV LIGHT.
C
C INPUT
C     JBE=1 FOR BARREL PART,  =2 FOR END CAP
C     Z=DEPTH OF THE SOURCE TRACK IN THE LEAD GLASS( RAD.LENGTH )
C     COST=COS(THETA) OF THE SOURCE TRACK W.R.TO THE NORMAL OF
C          THE CATHODE SURFACE.
C
C  THIS IS A SPECIAL VERSION WITH AN EXTRA ARGUMENT, FOR PLOTTING A1-2
C
C OUTPUT
C     PENUM=NO.OF PHOTOELECTRONS/1RAD.LENGTH LONG TRACK.
C
C---- AN EMPIRICAL FORMULA IS USED.
C---- PHOTOMULTIPLIER AREA IS ASSUMED TO COVER THE WHOLE SURFACE.
C---- TO GET THE REAL PHOTOELECTRON NUMBER PENUM SHOULD BE MULTIPLIED
C     BY THE RATIO (CATHODE AREA/LEAD GLASS CROSS SECTION).
C
      DIMENSION A(2),B(8,2,2)
      DATA B/54.90,48.55,-1180.1, 4484.0, -5803.2,657.17,3772.8,-1950.8,
     1 0.3566,0.624,-1.1146, 9.2361, 2.1321, -22.445,-1.0606, 14.955,
     2   57.43, 18.43, -967.99, 3892.1,-5084.3, 367.12, 3683.6,-1880.3,
     3  0.7039,2.9466,-5.5495,-6.5502, 19.755, 19.114,-17.413,-16.228/
C
      DIMENSION AFIT(21,2)
      DATA AFIT/
     $ 83.344, 53.083, 48.599, 45.092, 42.589, 40.442, 48.708, 52.659,
     $ 53.865, 55.566, 55.503, 55.939, 54.140, 52.271, 46.991, 40.710,
     $ 42.327, 45.090, 48.847, 53.213, 83.832,
     $  2.685,  1.480,  1.260,  1.253,  1.186,  1.158,  0.826,  0.607,
     $  0.544,  0.436,  0.369,  0.197,  0.136, -0.054,  0.002, -0.662,
     $ -0.627, -0.728, -0.811, -0.869, -1.529/
C
C  ////////////////////////////////////////
C
      PENUM = 0.
C---- CHECK POSITION
      IF(Z.LT.0. .OR. Z.GT.13.4 ) GO TO 90
      IF(ABS(COST).GT.1.0) GO TO 90
C---- DETERMINE A
      JBEW = JBE
C---- PROTECTION AGAINST FAULS JBE INPUT.
      IF(JBEW.LT.1) JBEW = 1
      IF(JBEW.GT.2) JBEW = 2
C
      IF(JBEW.EQ.2) GO TO 55
C
        COSS = -1.
        DO 22  I = 1,20
        COSL = COSS
        COSS = COSS + .1
        IF(COST.GT.COSS) GO TO 22
C
        FRAC = (COST - COSL) / .10
        J = 22 - I - 1
        K = 22 - I
C
        A(1) = AFIT(K,1) + FRAC*( AFIT(J,1) - AFIT(K,1) )
        A(2) = AFIT(K,2) + FRAC*( AFIT(J,2) - AFIT(K,2) )
C
       GO TO 33
C
   22   CONTINUE
       GO TO 33
C
55     CONTINUE
        DO 1 IA=1,2
        X = COST
        IF(IA.EQ.1) X = X*X
        F = B(8,IA,JBEW)
          DO 2 J=2,8
          JR = 9-J
          F = F*X+B(JR,IA,JBEW)
    2     CONTINUE
        A(IA) = F
    1   CONTINUE
C---- FUNCTN=A(1)+CC(COST)*EXP(0.55*Z)+A(2)*Z
C
33    CC = ((0.8751*COST-0.8362)*COST+0.3924)*COST-2.0
      CC = 10.**CC
      IF(JBEW.EQ.2) CC = 1.5*CC
      IF(JBEW.EQ.1.AND.COST.LT.-.35) CC = 0.
      EXPARG = EXP(0.55*Z)
      PENUM = A(1)+CC*EXPARG+A(2)*Z
      RETURN
C
      DATA MESS/0/
   90 IF(MESS.EQ.0) WRITE(6,600) JBE,Z,COST
  600 FORMAT(' ***** WRONG INPUT FOR NPECER *****',I6,2E12.3)
      MESS = 1
      RETURN
      END
