C   24/10/83 402220043  MEMBER NAME  MCHTRB   (JADEGS)      FORTRAN
C   24/10/83 402220031  MEMBER NAME  MCHTRB   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MCHTRB( I4VECT, NFOUND, P9VECT, IPALL, IFLAVR,IQG, IPN)
C-----------------------------------------------------------------------
C
C   AUTHOR    C. BOWDERY  19/10/83 :  TRACEBACK FROM 'VECT' TO 'PALL'
C
C        MOD  C. BOWDERY  26/10/83 :  P9VECT(30,9) --> P9VECT(9,30)
C   LAST MOD  C. BOWDERY  22/02/84 :  BUG IN DO 22 FIXED
C
C
C   GIVEN A 'VECT'/0 PARTICLE NUMBER, I4VECT, LOCATE IDENTICAL ENTRY
C   IN 'PALL' BANK AND TRACE BACKWARDS TO FIND ITS ANCESTOR PARTICLES.
C
C   INPUT:  I4VECT      =  'VECT'/0 PARTICLE NUMBER
C   OUTPUT: NFOUND      =  NUMBER OF 9-VECTORS TRACED IN 'PALL'
C           P9VECT      =  ARRAY OF 9 VECTORS FOR TRACED PARTICLES
C           IPALL       =  ARRAY OF POSITIONS IN 'PALL' FOR EACH 9-VECT
C           IFLAVR      =  QUARK FLAVOUR OF THE EVENT (IF APPROPRIATE)
C           IQG         =  POSITION OF PARTON IN P9VECT
C           IPN         =  PARTON ORDER NUMBER ( 1 = 1ST PARTON ETC.)
C
C-----------------------------------------------------------------------
C
C        THE 9-VECTORS ARE THE FOLLOWING:
C
C  PX, PY, PZ, E, M, CHARGE, TYPE, PARENT_IN_PALL, PARTON_NUMBER
C
C  ALL THESE QUANTITIES ARE REAL*4
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C----------START OF MACRO CMUBCS----------------------------------------
      COMMON /BCS/IDATA(1)
      DIMENSION HDATA(1),ADATA(1)
      EQUIVALENCE (HDATA(1),ADATA(1),IDATA(1))
C----------END OF MACRO CMUBCS------------------------------------------
C
      DIMENSION  P9VECT(9,30), PP(5), PV(5), IPALL(30)
C
      DATA IEFL, IEFL2, IEFL3 / 0 , 0 , 0 /
C
C-------------------  C O D E  -----------------------------------------
C
C                  INITIALISE OUTPUT VALUES TO ZERO
C
      IPN    = 0
      IQG    = 0
      IFLAVR = 0
      CALL VZERO( P9VECT, 270 )
      CALL VZERO( IPALL ,  30 )
C
C                  FIND 'VECT'/0 AND 'PALL' BANKS. RETURN IF NOT THERE.
C
      CALL CLOC( INDV, 'VECT', 0 )
      NFOUND = -1
      IF( INDV .LE. 0 ) RETURN
C
      CALL CLOC( INDP, 'PALL', 0 )
      NFOUND = -2
      IF( INDP .LE. 0 ) RETURN
C
C                  CHECK RANGE OF INPUT 'VECT'/0 PARTICLE NUMBER
C
      NFOUND = -3
      MAXPRT = IDATA( INDV + 4 )
C
      IF( I4VECT .LE. MAXPRT .AND. I4VECT .GE. 1 ) GO TO 1
        IEFL3 = IEFL3 + 1
        IF(IEFL3 .GT. 50 ) RETURN
C
          WRITE(6,5) I4VECT, MAXPRT
  5       FORMAT(/' ***  ERROR  ***  I4VECT ( = ',I3,') IS EITHER ',
     +    'GREATER THAN MAX_PARTICLE_NO. ',
     +    '( = ',I3,') OR LESS THAN 1  IN MCHTRB'/)
          RETURN
C
C                  COMPUTE POINTERS TO 'VECT' AND 'PALL'
C
  1   NFOUND = 0
      LV0    = IDATA( INDV + 1 )
      LV1    = IDATA( INDV + 2 )
      INDEXV = INDV + LV0 + (I4VECT-1) * LV1
C
      LP0    = IDATA( INDP + 1 )
      LP1    = IDATA( INDP + 2 )
C
C                  EXTRACT INFORMATION FOR THE TRACK FROM 'VECT'/0
C
      DO  10  I = 1,5
        PV(I) = ADATA( INDEXV + I )
  10  CONTINUE
      IQV   = IDATA( INDEXV + 6 )
      ITV   = IABS( IDATA( INDEXV + 7 ) )
C
      NPALL = IDATA( INDP + 4 )
C
C                  SEARCH FOR AN EXACT MATCH BETWEEN THE GIVEN PARTICLE
C                  IN 'VECT'/0 AND 'PALL'.
C
      DO  22  J = 1,NPALL
        DO  20  I = 1,5
          INDEXP = INDP + LP0 + (NPALL-J) * LP1
          PP(I) = ADATA( INDEXP + I )
          IF( PP(I) .NE. PV(I) ) GO TO 22
  20    CONTINUE
        IQP = IDATA( INDEXP + 6 )
        ITP = IDATA( INDEXP + 7 )
        IF( IQP .NE. IQV .OR. IABS( ITP ) .NE. ITV ) GO TO 22
C
C                  WE HAVE A MATCH - EXIT THE OUTER LOOP
C
        GO TO 30
  22  CONTINUE
C
C                  NO MATCH FOUND - PROBABLY A 'PALL' BANK OF AN OLD
C                  4-VECTOR EVENT NOT CONFORMING TO THE CONVENTION.
C
      IEFL   = IEFL + 1
      IF( IEFL .GT. 100 ) RETURN
        WRITE(6,35) I4VECT
  35    FORMAT(/' *** WARNING ***  MCHTRB FOUND NO MATCH FOR ''VECT''/',
     +    '0 TRACK ',I4,' IN THE ''PALL'' BANK.'/)
        RETURN
C
C                  RECORD DETAILS OF ORIGINAL PARTICLE
C
  30  IPALL(1) = NPALL - J + 1
      DO  40  I = 1,5
        P9VECT(I,1) = PP(I)
  40  CONTINUE
      P9VECT(6,1) = FLOAT( IQP )
      P9VECT(7,1) = FLOAT( ITP )
      P9VECT(8,1) = FLOAT( IDATA( INDEXP + 8 ) )
      P9VECT(9,1) = FLOAT( IDATA( INDEXP + 9 ) )
C
C                  STORE PARTON ORDER NUMBER AND EVENT FLAVOUR
C                  THEN FIND THE PARENT POINTER
C
      IPN       = IDATA( INDEXP + 9 )
      IPARNT    = IDATA( INDEXP + 8 )
      NFOUND    = 1
      IFLAVR    = IDATA( INDP + 9 )
C
C                  LOCATE IN 'PALL' THE PARENT AND ANCESTORS
C
  60  IF( IPARNT .EQ. 0 ) GO TO 70
      INDEXP = INDP + LP0 + (IPARNT-1) * LP1
      NFOUND = NFOUND + 1
      IF( NFOUND .GT. 30 ) GO TO 80
      IPALL(NFOUND) = IPARNT
      DO  50  I = 1,5
        P9VECT(I,NFOUND) = ADATA( INDEXP + I )
 50   CONTINUE
C
      DO  55  I = 6,9
        P9VECT(I,NFOUND)   = FLOAT( IDATA( INDEXP + I ) )
 55   CONTINUE
      IPARNT = IDATA( INDEXP + 8 )
      GO TO 60
C
C                  ALL PARTICLES TRACED. FIND THE PARTON. IQG = 0  IF
C                  NOT FOUND, OTHERWISE IQG = POSITION NUMBER.
C
  70  DO  71  I = 1,NFOUND
        IF( P9VECT(9,I) .NE. -100 ) GO TO 71
          IQG = I
          RETURN
  71  CONTINUE
C
      RETURN
  80  IEFL2 = IEFL2 + 1
      IF( IEFL2 .GT. 100 ) RETURN
        WRITE(6,81)
  81    FORMAT(/' ***  ERROR  ***  MCHTRB: > 30 PARTICLES IN TRACEBACK',
     +          ' OR PARENT POINTER NOT SET TO ZERO AT END OF CHAIN'/)
        RETURN
      END
