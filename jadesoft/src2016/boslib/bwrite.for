C   07/06/96 606071836  MEMBER NAME  BWRITE   (S4)          FORTG1
      SUBROUTINE BWRITE(IUN)
C     BOS SUBPROGRAM =3.3=
      COMMON/BCS/IW(1)
      EXTERNAL BFRD
   10 CALL BWRS(IUN,NBUFL,INIW)
      IF(INIW.EQ.0) GOTO 100
      CALL BFWR(IUN,NBUFL,IW(INIW))
      GOTO 10
  100 RETURN
      END