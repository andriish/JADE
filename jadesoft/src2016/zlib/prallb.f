C   15/02/88 802151547  MEMBER NAME  PRALLB   (S)           F77
      SUBROUTINE PRALLB( NAME )
C-----------------------------------------------------------
C  Version of 15/02/88         Last Mod 15/02/88 E Elsen
C  Print all BOS banks of name NAME.
C-----------------------------------------------------------
      COMMON /BCS/ IW(1)
C
      NPNAME = IBLN(NAME) + 1
      DO 10 WHILE( IW(NPNAME-1) .GT.0 )
        NPNAME = IW(NPNAME-1)
        CALL PRSINB( NAME, IW(NPNAME-2) )
   10 CONTINUE
      END
