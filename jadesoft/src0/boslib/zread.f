C   03/02/79 C9020501   MEMBER NAME  ZREAD    (S)           FORTRAN
      SUBROUTINE ZREAD(IUN,NTOT,BIN,NDIM,*,*)
      INTEGER BIN(1),IER/-1/
      COMMON/CFLAG/IFLAG
      IF(IER.LT.0) WRITE(6,201) IUN
      IER=0
      IFLAG=0
      CALL IBCOMR(IUN,&21,&11)
      CALL IBWORD(NTOT,&22,&12)
      IF(NTOT.LE.0) GOTO 31
      IF(NTOT.GT.NDIM) GOTO 32
      CALL IBARR(BIN,NTOT,&23,&13)
      CALL IBCOMF
      GOTO 100
C     READ ERRORS
   13 IER=IER+1
   12 IER=IER+1
   11 IER=IER+1
      IFLAG=IER
      WRITE(6,202) IER
      GOTO 101
C     END EXITS
   23 IER=IER+1
   22 IER=IER+1
   21 IER=IER+1
      IFLAG=IER+10
      WRITE(6,203) IER
      GOTO 102
C     ERROR IN FIRST WORD
   32 IER=IER+1
   31 IER=IER+1
      IFLAG=IER+100
      CALL IBCOMF
      WRITE(6,204) IER
      GOTO 101
C
  100 RETURN
  101 RETURN 1
  102 RETURN 2
C
  201 FORMAT('0ZREAD FIRST ENTRY',I3)
  202 FORMAT('0ZREAD IO-ERROR',I3)
  203 FORMAT('0ZREAD END',I3)
  204 FORMAT('0ZREAD L-ERROR',I3)
      END
      SUBROUTINE PRFLAG
      COMMON/CFLAG/IFLAG
      WRITE(6,101) IFLAG
      CALL ABEND
      RETURN
  101 FORMAT('0** PRFLAG **  IFLAG=',I10)
      END
