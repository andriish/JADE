      FUNCTION SAGCIR(FPP,CP,G,SP,EPS)
      FP=FPP*CP
      U=-(G*FP)*FP
      S=-.5*G*FPP
      IF(
     - ABS(U).GT..3
     -)THEN
      IF(
     - U.LT..98
     -)THEN
            SAGCIR=(SQRT(1.-U)-1.)/FP
      ELSE
            SAGCIR=0.
            PRINT 100,FPP,CP,G
100         FORMAT(1X,' SAGCIR',3E16.7)
      ENDIF
         SP=SAGCIR/CP
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
      SP=VAL
      SAGCIR=SP*CP
      RETURN
      END
