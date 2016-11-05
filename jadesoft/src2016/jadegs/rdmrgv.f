C   01/11/84 411011500  MEMBER NAME  RDMRGV   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDMRGV( N1, H1, N2, H2 )
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN    25/04/79 :  MERGE DRIFT DATA
C
C   LAST MOD  J. HAGEMANN 17/05/83 :  MODIFIED FOR VERTEX CHAMBER DATA
C             R. RAMCKE
C
C      MERGE N1 DRIFT DATA FROM H1 WITH N2 DRIFT DATA FROM H2
C      IN ARRAY H2 IN INCREASING WIRE NUMBER. PROGRAM ASSUMES
C      DATA OF H2 TO START IN H2(N1*4+1).
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      DIMENSION H1(2), H2(2)
C
C------------------  C O D E  ------------------------------------------
C
      IL1 = 1
      IL2 = N1*4 + 1
      IH1 = IL2 - 4
      IH2 = IH1 + N2*4
      ILN = 0
C
C
  100 IF( ILN .GT. IH2 ) GO TO 1000
C
C                            CHECK IF DATA LEFT IN H2
      IF( IL2 .GT. IH2 ) GO TO 400
C
C                            CHECK IF DATA LEFT IN H1
      IF( IL1 .GT. IH1 ) GO TO 500
C
C                            COMPARE WIRE NUMBERS
C                            IF SAME NUMBER COMPARE DRIFT TIMES
      IF( H1(IL1) .LT. H2(IL2) ) GO TO 200
      IF( H1(IL1).EQ.H2(IL2) .AND. H1(IL1+3).LE.H2(IL2+3) ) GO TO 200
C
C                            MOVE DATA FROM END OF H2 TO ILN  POSITION
      CALL MVC( H2, ILN*2, H2, (IL2-1)*2, 8 )
      IL2 = IL2 + 4
      GO TO 300
C
C                            MOVE DATA FROM H1 TO H2 TO ILN POSITION
  200 CALL MVC( H2, ILN*2, H1, (IL1-1)*2, 8 )
      IL1 = IL1 + 4
C
  300 ILN = ILN + 4
      GO TO 100
C
C                          MOVE REST OF H1 DATA TO H2 AFTER ILN POSITION
  400 NL = IH1 - IL1 + 4
      IF( NL .GT. 0 ) CALL MVCL( H2, ILN*2, H1, (IL1-1)*2, NL*2 )
      GO TO 1000
C
C                          MOVE REST OF H1 DATA TO H2 AFTER ILN POSITION
  500 NL = IH2 - IL2 + 4
      IF( NL .GT. 0 ) CALL MVCL( H2, ILN*2, H2, (IL2-1)*2, NL*2 )
C
 1000 RETURN
      END
