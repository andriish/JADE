C   29/10/82 501081234  MEMBER NAME  XXTEST   (JADESR)      FORTRAN
      SUBROUTINE XXTEST
C
    1 READ (10,END=100) IRD, IED
      READ (20,END=100) IRR, IER
      PRINT 50, IRD, IED, IRR, IER
   50 FORMAT('  IRD, IED, IRR, IER =',4I10)
      GOTO 1
C
  100 CONTINUE
      RETURN
C
      END
