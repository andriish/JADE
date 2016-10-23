C   15/02/88 802151516  MEMBER NAME  PRLIST   (S)        M  F77
      SUBROUTINE PRLIST( NNAMES, LIST )
C-----------------------------------------------------------
C  Version of 15/02/88         Last Mod 15/02/88 E Elsen
C  Print all NNAMES BOS banks specified in the list LIST.
C  All banks of a given name will be printed.
C-----------------------------------------------------------
      INTEGER LIST(NNAMES)
C
      DO 10 J=1,NNAMES
        CALL PRALLB( LIST(J) )
   10 CONTINUE
      END
