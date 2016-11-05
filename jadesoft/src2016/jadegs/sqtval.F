      FUNCTION SQTVAL(F,G,AL,EPS)
      S=(AL/F)*G
      U=-S*(AL/F)
      S=-.5*S*AL
      IF(
     - ABS(U).GT..3
     -)THEN
      IF(
     - U.LT..98
     -)THEN
            SQTVAL=F*(SQRT(1.-U)-1.)
      ELSE
            SQTVAL=0.
            PRINT 100,F,G,AL
100         FORMAT(1X,' SQTVAL',3E16.7)
      ENDIF
         RETURN
      ENDIF
      VAL=-S*(1.+.25*U+.125*U**2)
      Q=S*U**3/12.8
      N=5
15000 CONTINUE
      IF(
     - ABS(Q).GT.EPS .AND.N.LT.15
     -)THEN
         VAL=VAL-Q
         Q=Q*U*(1.-1.5/N)
         N=N+1
      GOTO 15000
      ENDIF
15001 CONTINUE
      SQTVAL=VAL
      RETURN
      END
