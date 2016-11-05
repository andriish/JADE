C   19/04/85 504191421  MEMBER NAME  MUHITS   (JADEMUS)     FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MUHITS( NHITS )
C-----------------------------------------------------------------------
C
C
C     PROVIDES NUMBER OF MUON HITS.  MUANAC IS CALLED IF NO MUR1
C     BANK ALREADY EXISTS FOR THIS EVENT.
C
C
C LAST CHANGE 14.00 19/04/85 C. BOWDERY  --- CALL MUANAB BEFORE MUANAC
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C                            COMMONS
C
#include "cmubcs.for"
C
C------------------  C O D E  ------------------------------------------
C
C                            LOCATE MUR1/0 IF EXISTING
C
      CALL CLOC(IP0,'MUR1',0)
      IF( IP0 .GT. 0 ) GO TO 1
C
C                            MUR1/0 NOT FOUND - CALL MUANAC TO CREATE IT
C                            BUT FIRST CALL MUANAB TO EXTRACT HEAD INFO
C
        CALL MUANAB
        CALL MUANAC
C
        CALL CLOC(IP0,'MUR1',0)
        IF( IP0 .LE. 0 ) GO TO 98
C
C                            MUR1/0 FOUND. EXTRACT NUMBER OF MU HITS
C
  1   NHITS = IDATA(IP0+1)
C
      RETURN
C
C
C                            ERROR CONDITIONS
C
 98   CALL MUERRY('MUHITS',0,'''MUR1'' CANNOT BE CREATED.^')
      NHITS = 0
      RETURN
C
      END
