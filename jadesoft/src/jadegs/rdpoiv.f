C   01/11/84            MEMBER NAME  RDPOIV   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDPOIV( H1, N2, H2 )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 08/12/82 :  PURGE DELETED HITS OF VERTEX
C             R. RAMCKE               CHAMBER
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      DIMENSION H1(2), H2(2)
C
C------------------  C O D E  ------------------------------------------
C
      IL      = 0
      IL2     = 1
      IR2     = IL2
      IH2     = N2*4 + IL2
      H2(IH2) = 0
C
C                       COPY WIRES, OMIT ZEROS
  100 IF( IR2 .GT. IH2 ) GOTO 1000
C
         IF( H2(IR2) .NE. 0 ) GO TO 300
C                      COPY OLD INFORMATION, IF ANY, AND INCREMENT INDEX
            LD = IR2 - IL2
            IF( LD . LE. 0 ) GO TO 200
               CALL MVCL( H1, IL*2, H2, (IL2-1)*2, LD*2 )
               IL = IL + LD
  200       IL2 = IR2 + 4
  300    IR2 = IR2 + 4
         GO TO 100
 1000 CONTINUE
      RETURN
      END
