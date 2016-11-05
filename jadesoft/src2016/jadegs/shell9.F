      SUBROUTINE SHELL9(IVAL,IIND,N)
      DIMENSION IVAL(N),IIND(N)
      INTEGER*2 I,J,K,M
      M=N/2
15000 CONTINUE
      IF(
     - M.GT.0
     -)THEN
         K=N-M
      DO 13000 J=1,K
            I=J
15002 CONTINUE
      IF(
     - I.GE.1
     -)THEN
               IVI=IVAL(IIND(I))
               IVIPM=IVAL(IIND(I+M))
      IF(
     - IVIPM.GE.IVI
     -)THEN
      GOTO 15003
      ENDIF
               I1=IIND(I+M)
               IIND(I+M)=IIND(I)
               IIND(I)=I1
               I=I-M
      GOTO 15002
      ENDIF
15003 CONTINUE
13000 CONTINUE
13001 CONTINUE
         M=M/2
      GOTO 15000
      ENDIF
15001 CONTINUE
      RETURN
      END
