C   19/02/84 402192303  MEMBER NAME  RDMERG   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDMERG( N1, H1, N2, H2 )
C-----------------------------------------------------------------------
C
C   AUTHOR    E. ELSEN    25/04/79 :  MERGE TRUE HITS WITH RANDOM HITS
C
C        MOD  E. ELSEN    02/05/79 :
C   LAST MOD  C. BOWDERY  30/08/83 :  POINTERS FROM OLD TO NEW HITS
C
C             MERGE N1 DRIFT DATA FROM H1 WITH N2 DRIFT DATA FROM H2
C             IN ARRAY H2 IN INCREASING WIRE NUMBER. PROGRAM ASSUMES
C             DATA OF H2 TO START IN H2(N1*4+1). IN ORDER TO KEEP
C             TRACK OF INFORMATION RELATED TO THE 4 VECTOR/JETC HIT
C             ASSOCIATION, ALSO STORE AN ARRAY OF POINTERS FROM THE
C             ORIGINAL HITS TO THEIR NEW POSITIONS.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / CWORK  / HWORK(20000)
      COMMON / CWORK1 / JHITS, H4VJH(4000), H4VMRG(5000)
C
      DIMENSION H1(1), H2(1)
C
C-----------------------  C O D E  -------------------------------------
C
      IL1 = 1
      IL2 = N1*4 + 1
      IH1 = IL2 - 4
      IH2 = IH1 + N2*4
      ILN = 0
C
      JHITS = 0
      JNEW  = 1
C
  100 IF( ILN .GT. IH2 ) RETURN
C
C                       CHECK IF DATA LEFT IN H2
C
      IF( IL2 .GT. IH2 ) GO TO 400
C
C                       CHECK IF DATA LEFT IN H1
C
      IF( IL1 .GT. IH1 ) GO TO 500
C
C                       COMPARE WIRE NUMBERS.
C                       IF SAME NUMBER COMPARE DRIFT TIMES
C
      IF( H1(IL1) .LT. H2(IL2) ) GO TO 200
      IF( H1(IL1).EQ.H2(IL2) .AND. H1(IL1+3).GE.H2(IL2+3) ) GO TO 200
C
C                       TAKE THE NEXT RANDOM HIT AND PLACE INTO OUTPUT
C                       AREA WHICH STARTS FROM BEGINNING OF H2. IE.
C                       MOVE DATA FROM HIGH END OF H2 TO ILN  POSITION
C
      CALL MVC( H2, ILN*2, H2, (IL2-1)*2, 8 )
      IL2 = IL2 + 4
      GO TO 300
C
C                       TAKE THE NEXT REAL HIT AND PLACE INTO OUTPUT
C                       AREA WHICH STARTS FROM BEGINNING OF H2. IE.
C                       MOVE DATA FROM H1 TO H2 TO ILN POSITION.
C
  200 CALL MVC( H2, ILN*2, H1, (IL1-1)*2, 8 )
      IL1 = IL1 + 4
C
      JHITS = JHITS + 1
      H4VJH(JHITS) = JNEW
C
  300 ILN  = ILN  + 4
      JNEW = JNEW + 1
      GO TO 100
C
C                       RANDOM HITS EXHAUSTED SO COPY REST OF TRUE HITS.
C                       MOVE REST OF H1 DATA TO H2 AFTER ILN POSITION.
C
  400 NL = IH1 - IL1 + 4
      IF( NL .LE. 0 ) RETURN
      CALL MVCL( H2, ILN*2, H1, (IL1-1)*2, NL*2 )
C
      JN =  NL / 4
      DO  450  J = 1,JN
        JHITS = JHITS + 1
        H4VJH(JHITS) = JNEW + J - 1
 450  CONTINUE
C
      RETURN
C
C                       TRUE HITS EXHAUSTED SO COPY REST OF RANDOM HITS.
C                       MOVE REST OF H1 DATA TO H2 AFTER ILN POSITION.
C
  500 NL = IH2 - IL2 + 4
      IF( NL .LE. 0 ) RETURN
      CALL MVCL( H2, ILN*2, H2, (IL2-1)*2, NL*2 )
C
      RETURN
      END
