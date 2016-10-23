C   19/02/84 402192304  MEMBER NAME  RDDOUB   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDDOUB( IDIFF, NHITS, HITS, IERASE )
C-----------------------------------------------------------------------
C
C   AUTHOR    E. ELSEN    25/04/79 :  DOUBLE HIT RESOLUTION HIT LOSSES
C
C        MOD  E. ELSEN    02/05/79 :
C        MOD  C. BOWDERY  28/09/83 :  POINTERS FROM OLD TO NEW HITS
C        MOD  C. BOWDERY  13/10/83 :  CORRECT PROBLEM OF POINTER TO DEAD
C                                  :  HITS.
C   LAST MOD  C. BOWDERY  17/10/83 :  NEGATIVE POINTER FOR ABSORBED HIT
C
C        SET SOME DRIFT WIRES IN ARRAY HITS WITH NHITS HITS TO
C        ZERO TO ACCOUNT FOR FINITE DOUBLE PULSE RESOLUTION IDIFF.
C        IDIFF IS MINIMUM RESOLVED DISTANCE IN TIMING BINS IN EVERY
C        RING. NUMBER OF ERASED HITS IS IERASE.
C
C-----------------------------------------------------------------------
C
C                  DOUBLE HIT RESOLUTION
C                  ---------------------
C
C        WHEN A HIT REACHES A WIRE IN THE JET CHAMBER, THE ELECTRONICS
C        FOR THAT WIRE ARE NOT SENSITIVE TO ANOTHER HIT FOR ABOUT 120NS.
C        IF ANOTHER HIT ARRIVES WITHIN THAT PERIOD, IT IS LOST BUT THE
C        ELECTRONICS THEN NEEDS A FURTHER 120NS TO RECOVER.
C
C        IN THIS ROUTINE THE HITS ARE PROCESSED IN ORDER OF DECREASING
C        DRIFT TIMES FOR A GIVEN WIRE.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / CWORK  / HWORK(20000)
      COMMON / CWORK1 / JHITS, H4VJH(4000), H4VMRG(5000)
C
      DIMENSION IDIFF(3), HITS(2)
C
C------------------  C O D E  ------------------------------------------
C
      IERASE = 0
      DO 10  I = 1,NHITS
        H4VMRG(I) = 0
 10   CONTINUE
C
      IF( NHITS .LT. 2 )  RETURN
C
C                  LOOP OVER ALL HITS AND SET AFFECTED WIRES TO ZERO
C
      N4  = NHITS * 4
      N44 = N4 - 4
      IL  = 1
      KL  = 1
C
  100 IF( HITS(IL) .NE. 0 ) GO TO 200
          IL = IL + 4
          KL = KL + 1
          IF( IL .LT. N44 ) GO TO 100
          GO TO 2000
C
  200 IH = IL + 4
      KH = KL + 1
  210 IF( IH .GT. N4 ) GO TO 2000
      IF( HITS(IH) .NE. 0 ) GO TO 300
          IH = IH + 4
          KH = KH + 1
          GO TO 210
C
C                            HITS BELONG TO THE SAME WIRE ?
C
  300 IF( HITS(IL)/8 .NE. HITS(IH)/8 ) GO TO 1000
          IRING = MIN0( HITS(IL)/3072 + 1, 3 )
          IDR   = HITS(IL+3)-HITS(IH+3)
C
C                            TEST IF DRIFT TIME DIFFERENCE IS SMALLER
C                            THAN 2 HIT RESOLUTION TIME FOR THIS RING
C
          IF( IABS( IDR ) .GT. IDIFF(IRING) ) GO TO 1000
             IF( HITS(IH+1) .LT. HITS(IL+1) )   HITS(IH+1) = HITS(IL+1)
             IF( HITS(IH+2) .LT. HITS(IL+2) )   HITS(IH+2) = HITS(IL+2)
             HITS(IL) = 0
             IERASE = IERASE + 1
C
C                            STORE THE HIT NUMBER THAT THIS HIT WAS
C                            TOO CLOSE TO. SET IT NEGATIVE.
C
             H4VMRG(KL) = -KH
C
C                            FIND ANY POINTERS TO THIS DEAD HIT AND SET
C                            THEM TO THE HIT THAT KILLED THIS HIT.
C                            ONLY SEARCH BACKWARDS UP TO 8 HITS.
C
             KKB = KL - 8
             IF(KKB .LT. 1) KKB = 1
             KKT = KL - 1
             IF(KKT .LT. 1) GO TO 1000
C
             DO  301  KK = KKB,KKT
               IF(H4VMRG(KK) .EQ. -KL ) H4VMRG(KK) = -KH
  301        CONTINUE
C
 1000 IL = IL + 4
      KL = KL + 1
      GO TO 100
C
C                  UPDATE THE ARRAY OF POINTER FROM THE ORIGINAL HITS
C                  TO THE CURRENT HITS FOR EACH HIT MERGED WITH ANOTHER
C                  SO THAT A COMPLETE HISTORY EXISTS.
C

 2000 DO  2010  I = 1,JHITS
        IF( H4VMRG( H4VJH(I) ) .NE. 0 ) H4VJH(I) = H4VMRG( H4VJH(I) )
 2010 CONTINUE
C
      RETURN
      END
