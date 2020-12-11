C   03/08/79 702121839  MEMBER NAME  SPTHAK   (S)           FORTRAN77
      SUBROUTINE zSPTHAK( NL,NH,ITH,THR,ISPH,SPHR,IAKO,AKO,IER ) ! PMF 11/04/00 SPTHAK->zSPTHAK
C-----------------------------------------------------------
C   VERSION OF 03/08/79 E.ELSEN      LAST MOD 14/11/86    M.ZIMMER
C   EXTENDED VERSION OF 14/11/86   M.ZIMMER
C   ALSO AKOPLANARITY CALCULATION IS INCLUDED
C   COMPUTE THRUST THR AND SPHERICITY SPHR OF MOMENTA STORED
C   IN LOCATIONS NL TO NH IN BLANK COMMON P(10,400)
C   THE AXIS ARE RETURNED IN ITH AND ISPH, ISPH+1, ISPH+2, IAKO
C   THE THREE NORMALIZED EIGENVALUES OF SPHERICITY ARE STORED IN
C   LOCATIONS 4 OF THE CORRESPONDING AXIS
C   ITHR=0 OR ISPH=0 OR IAKO=0 SUPRESSES CORRESPONDING CALCULATION
C-----------------------------------------------------------
C
      COMMON P(10,400)
      DIMENSION PC(4,100)
      DIMENSION AXISSP(9), AXISTH(3), SPH(3),AXISAK(3)
C
      IER = 2
      NMOM = NH - NL + 1
      IF( NMOM .LT. 1 ) RETURN
      IER = 0
C                                           COPY VECTORS
      K = 0
      DO 200 J = NL, NH
      K = K + 1
      DO 100 I = 1,3
  100 PC(I,K) = P(I,J)
  200 PC(4,K) = P(6,J)
C
C                                           SPHERICITY
      IF( ISPH.NE.0 ) CALL zSPHRCY( PC, NMOM, SPH, AXISSP ) ! PMF 11/04/00 SPHRCY->zSPHRCY
C                                           THRUST
      IF( ITH.NE.0 ) CALL THRUST( PC, NMOM, 15, THR, AXISTH, IER )
C                                      AKOPLANARITY
      IF( IAKO.NE.0 ) CALL AKOP( PC, NMOM, 15, AKO, AXISAK, IER )
C
C                                           COPY TO LOCATIONS IN P
      IF( ISPH .EQ. 0 ) GO TO 310
      DO 300 J=1,3
      P(J,ISPH) = AXISSP(J)
      P(J,ISPH+1) = AXISSP(J+3)
  300 P(J,ISPH+2) = AXISSP(J+6)
C                                           STORE NORM. EIGENVALUES
      SPHR = SPH(1)
      P(4,ISPH) = SPH(1)
      P(4,ISPH+1) = SPH(2)
      P(4,ISPH+2) = SPH(3)
C                                           STORE NORM. EIGENVALUES
  310 IF( ITH .EQ. 0 ) GOTO 320
      DO 400 I=1,3
  400 P(I,ITH) = AXISTH(I)
      P(4,ITH) = THR
C                                           STORE NORM. EIGENVALUES
  320 IF( IAKO .EQ. 0 ) RETURN
      DO 410 I=1,3
  410 P(I,IAKO) = AXISAK(I)
      P(4,IAKO) = AKO
      RETURN
      END
