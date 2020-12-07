      REAL FUNCTION PARKOR*8(IP) 
C COPIED AND RENAMED FROM F22KLE.JVTXC.S(PARCOR)   18.9.1986
C
C     IP : POINTER TO TRACK IN PATR-BANK
C
C     CURVATURE-CORRECTION FOR CIRCLES FITTED WITH A PARABOLA
C
C PMF 03.11.98 
      LOGICAL TBIT
      REAL*8 P,Q,R,CM,D,U,V,C
C
      INTEGER*2 HDATA
C----------------------------------------------------------------------
C             MACRO CDATA .... BOS COMMON.
C
C             THIS MACRO ONLY DEFINES THE IDATA/HDATA/ADATA NAMES.
C             THE ACTUAL SIZE OF /BCS/ IS FIXED ON MACRO CBCSMX
C             OR BY OTHER MEANS. A DEFAULT SIZE OF 40000 IS GIVEN HERE.
C
C----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(40000)
      DIMENSION HDATA(80000),ADATA(40000),IPNT(50)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1)),(IPNT(1),IDATA(55))
      EQUIVALENCE (NWORD,IPNT(50))
C
C------------------------ END OF MACRO CDATA --------------------------
C
C *** MEASURED CURVATURE
      CM = -ADATA(IP+22)
      C  =  ADATA(IP+25) * 0.5
      IF (IDATA(IP+18).NE.2) GOTO 100
C *** MEASURED PROJ. TRACKLENGTH
      R  = ABS( ( ADATA(IP+7) - ADATA(IP+14) ) / ADATA(IP+30) ) * 0.5
C
C     CARDANS RULE
C
      P =  7.D0/(18.D0*R*R)
      Q = -7.D0*CM/(12.D0*R*R)
C
      D = DSQRT(Q*Q+P*P*P)
      U = (-Q+D)**(1./3.)
      V = ( Q+D)**(1./3.)
      C = U-V
C
  100 CONTINUE
      PARKOR = C * 2.0D0
C
      RETURN
      END
