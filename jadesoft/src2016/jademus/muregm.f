C   16/06/79 C9061801   MEMBER NAME  MUREGM   (S)           FORTRAN
C=======================================================================
C
C THIS ROUTINE PRODUCES A MASK (IE. A STRING OF 6 BITS) FROM A SET OF
C
C DIRECTION COSINES.THIS MASK 'TELLS' MUREGY WHICH FACES TO LOOK AT
C
C ACCORDING TO THE FOLLOWING SCHEME:
C
C          BIT        FACE
C           1........1  (-X)
C          10........2  (+X)
C         100........3  (-Y)
C        1000........4  (+Y)
C       10000........5  (-Z)
C      100000........6  (+Z)
C
C
C THE MASK WILL ENABLE A BIT BY BIT .AND. OF THE CORRESPONDING
C
C BITS OF HRMASK(I) & HMASK TO BE MADE IN MUREGY.THE RESULT IS THEN
C
C .OR.ED .IF THE 'TRUTH' VALUE IS 0 THEN THE GIVEN REGION 'I' IS IGNORED
C
C (AS IT DOES NOT BELONG TO A FACE SPECIFIED BY THE MASK).
C
C
C
C CUTS MAY BE APPLIED TO INDIVIDUAL DIRECTION COSINES SO THAT IF A
C
C DIRECTION COSINE 'L' SAY, IS LESS THAN OR EQUAL TO CUT(1) WITH RESPECT
C
C TO A GIVEN FACE ,THAT FACE IS IGNORED. NOTE: IF THE CUTS ARE 0 THEN
C
C THEY ARE IN EFFECT NOT APPLIED.
C
C=======================================================================
C
C
C  JUNE  1979   HARRISON B. PROSPER        J A D E
C
C
C=======================================================================
C
C
      SUBROUTINE MUREGM(C,CUT,HMASK)
      IMPLICIT INTEGER*2 (H)
      DIMENSION C(3),CUT(3)
C
C
C
C
C_____SET___A___BIT___FOR___EACH___DIRECTION___COSINE___________________
C
      HMASK = 0
C
      DO 53 J=1,3
C
                   IF(ABS(C(J)).LE.CUT(J))       GOTO 51
                   IF(C(J).LT.0.0)               GOTO 50
                   HRFA = 2**( 2*J - 1 )
                                                 GOTO 52
50                 HRFA = 2**( 2*J - 2 )
                                                 GOTO 52
51                 HRFA = 0
52                 HMASK = HMASK + HRFA
C
53    CONTINUE
C
C
C
C=======================================================================
C
C
C
C
      RETURN
      END
