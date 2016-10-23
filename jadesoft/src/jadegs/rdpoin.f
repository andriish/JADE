C   19/02/84 402192324  MEMBER NAME  RDPOIN   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDPOIN( H1, N2, H2 )
C-----------------------------------------------------------------------
C
C   AUTHOR    E. ELSEN    25/04/79 :  PURGE DELETED HITS
C
C        MOD  E. ELSEN    02/05/79 :
C        MOD  C. BOWDERY  29/09/83 :  POINTERS FROM OLD TO NEW HITS
C   LAST MOD  C. BOWDERY  17/10/83 :  NEGATIVE VALUES FOR ABSORBED HITS
C
C        ERASE WIRES IN ARRAY H2 (TOTAL N2) WITH ZERO WIRE NUMBER
C        AND COPY TO H1 STARTING IN POSITION 99.
C        CELL POINTERS ARE STORED IN H1(1..98).
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / CWORK  / HWORK(20000)
      COMMON / CWORK1 / JHITS, H4VJH(4000), H4VMRG(5000)
C
      DIMENSION H1(2), H2(2)
C
      DATA ICLENG / 98 /
C
C---------------------------  C O D E  ---------------------------------
C
C                  CLEAR ARRAY OF POINTERS FROM INPUT SET OF HITS TO
C                  THE OUTPUT SET OF HITS
C
      DO  1  I = 1,N2
        H4VMRG(I) = 0
  1   CONTINUE
      JNEWP = 1
      JOLDP = 1
C
      IL  = ICLENG
      IL2 = 1
      IR2 = IL2
      IH2 = N2*4 + IL2
      H2(IH2) = 0
C
C                  COPY WIRES, OMIT ZEROS
C
  100 IF( IR2 .GT. IH2 ) GO TO 1000
C
C                  USE A BLOCK TRANSFER STRATEGY. IF WIRE NUMBER IS NOT
C                  ZERO THEN INCREASE POINTER. ONLY COPY A BLOCK WHEN
C                  A DELETED HIT IS FOUND.
C

      IF( H2(IR2) .NE. 0 ) GO TO 300
C
C                  COPY OLD INFORMATION, IF ANY, AND INCREMENT INDEX
C
         LD = IR2 - IL2
         IF( LD . LE. 0 ) GO TO 200
         CALL MVCL( H1, IL*2, H2, (IL2-1)*2, LD*2 )
         IL = IL + LD
  200    IL2 = IR2 + 4
         GO TO 400
C
C                  IF WIRE NOT ZERO (HIT NOT DELETED), STORE NEW
C                  POSITION FOR THIS HIT
C
  300    H4VMRG(JOLDP) = JNEWP
         JNEWP = JNEWP + 1
  400    JOLDP = JOLDP + 1
         IR2 = IR2 + 4
      GO TO 100
 1000 CONTINUE
C
C                  SET UP CELL POINTERS
C
      ICL = 0
      ILOW = 1
      IH = IL - 3
      IL = ICLENG + 1
 1100 IF( IL .GT. IH ) GO TO 1400
           ICELL = H1(IL) / 128 + 1
               IF( ICELL .EQ. ICL ) GO TO 1300
                      DO 1200 I = ILOW, ICELL
 1200                 H1(I) = IL - ICLENG
                      ILOW = ICELL + 1
                      ICL = ICELL
 1300          IL = IL + 4
      GO TO 1100
C
 1400 DO 1500 I=ILOW, ICLENG
 1500 H1(I) = IL - ICLENG
C
C                  USE THE H4VJH POINTER LIST WITH THE NEW H4VMRG LIST
C                  TO CREATE A FINAL ASSOCIATION ARRAY BETWEEN THE
C                  HIT INFORMATION IN THE ORIGINAL JETC BANK AND THE
C                  NEW SMEARED JETC BANK
C
      DO  2  I = 1,JHITS
        I4VJH    = H4VJH(I)
        H4VJH(I) = H4VMRG( IABS( I4VJH ) ) * ISIGN( 1,I4VJH)
 2    CONTINUE
C
      RETURN
      END
