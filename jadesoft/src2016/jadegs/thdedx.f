C   05/11/82 401161419  MEMBER NAME  THDEDX   (S)           FORTRAN
      FUNCTION THDEDX (BETA,GAMMA,DEMIN)
C ---------------------------------------------------------------------
C
C        CALCULATE THEORETICAL VALUE FOR DEDX
C
C        STERNHEIMER PREDICTION FOR DEDX AS FUNCTION OF BETA, GAMMA
C        RERISE ADJUSTED TO OBSERVATION
C        HIGH DE/DX ADJUSTED TO OBSEVATION (CADEDX)
C        PARAMETERS IN DATA STATEMENT CALCULATED IN PROGRAM 'VAVRA'
C
C        INPUT : BETA, GAMMA
C                DEMIN = EXPERIMENTAL VALUE FOR DE/DX IN MINIMUM
C
C        OUTPUT: THDEDX = DE/DX  AT GIVEN VALUE OF BETA,GAMMA
C
C                 A. WAGNER  15.8.1983
C        CHANGED: K. AMBRUS  15.1.1984
C ---------------------------------------------------------------------
C
C     ALFAT  = ALFA*T
C     XMEANI = MEAN IONISATION POTENTIAL
C     RSTERN = STERNHEIMER FERMIPLATEAU
C     REXP   = EXPERIMENTAL FERMIPLATEAU
C     AI0    = VALUE OF DEDX(THEOR) IN MINIMUM
C
C                            CONSTANTS FOR ARGON - CH4 - I C4H10
C                                          .887   .085    .028
C                            JADE PARAMETERS
C
      DATA ALFAT /0.4798E-3/, XMEANI /0.1730E-3/
      DATA XA /2.2480/, X0 /1.6990/, X1 /3.6990/
      DATA RSTERN /1.540/, REXP / 1.425 /, AI0 /6.001E-3/
      DATA XMEL /0.5110034/
      DEMIN = 10. / REXP
      CREXP = REXP*0.995927
      RCORR =(CREXP-1.) / (RSTERN-1.)
C
      BETAG=BETA*GAMMA
      BB=BETA*BETA
      X=XMEL*ALFAT/(XMEANI*XMEANI)
      X=ALOG(X)+0.891+2.*ALOG(BETAG)-ALOG(BB)-BB
      X=X-DENS(XA,X0,X1,BETAG)
      RDEDX=(ALFAT*X/BB) / AI0
      IF (BETAG.GT.4.) RDEDX = (RDEDX-1.)*RCORR+1.
      THDEDX = DEMIN * RDEDX
      THDEDX = THDEDX*CADEDX(THDEDX,DEMIN)
      RETURN
      END
C ----------------------------------------------------------------------
      FUNCTION DENS (XA,X0,X1,BETAG)
      X=ALOG10(BETAG)
      DENS=0.
      IF (X.LT.X0) RETURN
      IF (X.GE.X0.AND.X.LE.X1) GO TO 1000
      DENS=4.606*(X-XA)
      RETURN
 1000 CONTINUE
      AA=4.606*(XA-X0)
      AA=AA/(X1-X0)**3.
      DENS=4.606*(X-XA)+AA*(X1-X)**3.
      RETURN
      END
C ----------------------------------------------------------------------
      FUNCTION CADEDX(DEDXTH,DEMIN)
      DATA P1/16.7581/, P2/1.20645/, P3/-0.301784/
      IF(DEDXTH.LE.DEMIN) GO TO 100
      ZZ = (DEDXTH-DEMIN)/P1
      IF(ZZ.GT.50.) ZZ = 50.
C
      CADEDX = EXP(-P2*ZZ) + ZZ
      CADEDX = CADEDX**P3
      RETURN
C
 100  CONTINUE
      CADEDX = 1.
      RETURN
      END
