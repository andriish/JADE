C   07/06/96 606071828  MEMBER NAME  BREAD    (S4)          FORTG1
      SUBROUTINE BREAD(IUN,*,*)
C     BOS SUBPROGRAM =3.1=
      COMMON/BCS/IW(60000)
   10 CALL BRDS(IUN,NBUFL,INIR)
      IF(INIR.EQ.0) GOTO 100
C
C PMF 28/10/98: Read IW(INIR) explicitly BEFORE calling BFWR and
C perform then a backspace. Otherwise BFWR does not work correctly
C on the current platforms.
C PMF 07/07/99: correct bug
       write(*,*)'ONE', INIR,IW(INIR)
      READ(IUN,ERR=101,END=102) IW(INIR)
      BACKSPACE (IUN)
             write(*,*)'TWO', INIR,IW(INIR)

      CALL BFRD(IUN,IW(INIR),IW(INIR+1))
      IF(IW(INIR)) 102,101,10
  100 RETURN
  101 RETURN 1
  102 RETURN 2
      END
