C   22/03/97 703221846  MEMBER NAME  JEOSUM4  (JADEGS)      FORTRAN
C   09/06/83 703131338  MEMBER NAME  SAGCIR   (S)           SHELTRAN
      FUNCTION SAGCIR(FPP,CP,G,SP,EPS)
C                                   J. SPITZER
C     CALCULATE SAGCIR=(SQRT(1+G*(FPP*CP)**2)-1)/(FPP*CP)
C               SP    =SAGCIR/CP
C     TO ACHIEVE GOOD PRECISION, FOR SMALL FPP*CP THE TAYLOR EXPANSION
C     IS USED UPTO AT MOST 15 TERMS
C     EPS IS THE REQUIRED ABSOLUTE PRECISION
C
      FP=FPP*CP
      U=-(G*FP)*FP
      S=-.5*G*FPP
      IF ABS(U).GT..3
      THEN
         IF U.LT..98
         THEN
            SAGCIR=(SQRT(1.-U)-1.)/FP
         ELSE
            SAGCIR=0.
            PRINT 100,FPP,CP,G
100         FORMAT(1X,' SAGCIR',3E16.7)
         CIF
         SP=SAGCIR/CP
         RETURN
      CIF
      VAL=-S*(1.+.25*U+.125*U**2)
      Q=S*U**3/12.8
      N=5
      WHILE ABS(Q).GT.EPS .AND.N.LT.15
         VAL=VAL-Q
         Q=Q*U*(1.-1.5/N)
         N=N+1
      CWHILE
      SP=VAL
      SAGCIR=SP*CP
      RETURN
      END
C   09/06/83 703131345  MEMBER NAME  SHELL9   (S)           SHELTRAN
      SUBROUTINE SHELL9(IVAL,IIND,N)
C                                   J. SPITZER
C     SORT INDICES IN THE ARRAY IIND(.) ACCORDING TO
C     THE CORRESPONDING VALUES IN IVAL(IIND(.))
C
      DIMENSION IVAL(N),IIND(N)
      INTEGER*2 I,J,K,M
C
      M=N/2
      WHILE M.GT.0
         K=N-M
         FOR J=1,K
            I=J
            WHILE I.GE.1
               IVI=IVAL(IIND(I))
               IVIPM=IVAL(IIND(I+M))
               IF(IVIPM.GE.IVI) XWHILE
               I1=IIND(I+M)
               IIND(I+M)=IIND(I)
               IIND(I)=I1
               I=I-M
            CWHILE
         CFOR
         M=M/2
      CWHILE
      RETURN
      END
C   09/06/83 703131348  MEMBER NAME  SQTVAL   (S)           SHELTRAN
      FUNCTION SQTVAL(F,G,AL,EPS)
C                                  J. SPITZER
C     CALCULATE SQTVAL=F*(SQRT(1+G*AL**2/F**2)-1).
C     TO ACHIEVE GOOD PRECISION, FOR LARGE F THE TAYLOR EXPANSION
C     IS USED UPTO AT MOST 15 TERMS
C     EPS IS THE REQUIRED ABSOLUTE PRECISION
C
      S=(AL/F)*G
      U=-S*(AL/F)
      S=-.5*S*AL
      IF ABS(U).GT..3
      THEN
         IF U.LT..98
         THEN
            SQTVAL=F*(SQRT(1.-U)-1.)
         ELSE
            SQTVAL=0.
            PRINT 100,F,G,AL
100         FORMAT(1X,' SQTVAL',3E16.7)
         CIF
         RETURN
      CIF
      VAL=-S*(1.+.25*U+.125*U**2)
      Q=S*U**3/12.8
      N=5
      WHILE ABS(Q).GT.EPS .AND.N.LT.15
         VAL=VAL-Q
         Q=Q*U*(1.-1.5/N)
         N=N+1
      CWHILE
      SQTVAL=VAL
      RETURN
      END
C   25/10/79 807251857  MEMBER NAME  THRUNB   (JADEGS)      SHELTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE THRUNB( P, NP, NMAXP, PTHR, AXIS, ITMERR )
C-----------------------------------------------------------------------
C
C    VERSION OF 21/05/79   LAST MOD 25/10/79   E.ELSEN
C                          ITMERR IS ADDED TO THE ARG06-06-79 S.YAMADA
C    INPUT : P(4,NP)   NP MOMENTA PX,PY,PZ,ABS(P)
C            NP        NUMBER OF MOMENTA
C            NMAXP     CUTOFF FOR PERMUTED MOMENTA
C                      =0 MEANS NO CUTOFF
C    OUTPUT: PTHR      THRUST VALUE
C            AXIS      THRUST AXIS
C            ITMERR    =1, IF LEFT TIME IS NOT ENOUGH TO CALCULATE THRST
C                      (NOW MIN.2 SECONDS ARE REQUIRED.)
C    METHOD: NO MOMENTUM CONSERVATION
C            THRUST VALUE IS FOUND BY LOOKING THROUGH ALL
C            PERMUTATIONS OF THE MOMENTA.
C
C-----------------------------------------------------------------------
C
      DIMENSION P(1), AXIS(3)
      DIMENSION PIN(320), IPERM(80),IPS(320),PS(320)
      COMMON / CWORK / WORK(720)
      EQUIVALENCE (WORK(1),PIN(1)),(WORK(321),IPERM(1))
      EQUIVALENCE (WORK(401),PS(1)),(WORK(401),IPS(1))
C
      DATA IPINC / 4 /
      DATA NLIMIT / 15 /
      DATA NSEC / 2 /
C
C------------------  C O D E  ------------------------------------------
C
      ITMERR = 0
C
      NMAXP1 = NMAXP
      IF  NP.GE.NLIMIT .AND. ( NMAXP.EQ.0 .OR. NMAXP.GT.NLIMIT )
      THEN
N     WARNING CONCERNING NUMBER OF PARAMETERS
      PERFORM WARN
      NMAXP1 = NLIMIT
      CIF
      NTOT = NP
      NP1 = NTOT
      IF NTOT.GT.NMAXP1 .AND. NMAXP1.NE. 0
      THEN
N     SORT MOMENTA IN DECREASING ORDER
      PERFORM SORT
      NTOT = NMAXP1 - 1
N     PIN ARRAY = ORDERED P ARRAY
      PERFORM FILLIN
      ELSE
N     COPY FROM P TO PIN ARRAY
      CALL UCOPY( P, PIN, NTOT*4 )
      CIF
N     THRUST AXIS FOR PIN ARRAY
      PERFORM THRST
N     THRUST VALUE FOR P ARRAY
      PERFORM THREXP
      RETURN
C
C
C
C--------------
C
      PROC WARN
C--------------
      WRITE(6,9101) NP,NLIMIT,NMAXP
 9101 FORMAT(' +++++  WARNING FROM THRUST ROUTINE  +++++'/
     *       2X,I4,' ARE TOO MANY INPUT VECTORS. NLIMIT=',I4,
     *' INPUT CUTOFF NMAXP=',I4,'.PTHR VAL. FOR NMAXP=NLIMIT WAS TAKEN')
      CPROC
C
C
C
C--------------
C
      PROC SORT
C--------------
N     ORDER P ARRAY USING SHELLSORT ALGTHM
N     SEQUENCE OF MOMENTA IS STORED IN IPERM
      FOR J=1,NTOT
N     INIT IPERM IN NATURAL ORDER
      IPERM(J) = J*4
      CFOR
C
      M = NTOT / 2
      WHILE  M.GT.0
      K = NTOT - M
      FOR J=1,K
      I = J
      WHILE  I.GT.0
      ILOW = IPERM(I)
      IHIGH = IPERM(I+M)
      IF  P(IHIGH) .GT. P(ILOW)
      THEN
      IPERM(I) = IHIGH
      IPERM(I+M) = ILOW
      I = I - M
      ELSE
      XWHILE
      CIF
      CWHILE
      CFOR
      M = M/2
      CWHILE
      CPROC
C
C
C--------------
C
      PROC FILLIN
C--------------
N     PIN(1..4,J) = P(1..4,IPERM(J)/4)
      FOR J=1,NTOT
      IPJ = IPERM(J)
      J4 = J*4
      PIN(J4  ) = P(IPJ  )
      PIN(J4-1) = P(IPJ-1)
      PIN(J4-2) = P(IPJ-2)
      PIN(J4-3) = P(IPJ-3)
      CFOR
      CPROC
C
C
C----------------
      PROC THRST
C----------------
      NUP = NTOT*4 - 4
N     FIRST VALID POSITION
      K = 5
N     INIT ACCUMULATOR ARRAY
      IPS(1) = 1
      IPS(K) = 1
      PS(2) = 0.
      PS(3) = 0.
      PS(4) = 0.
      PM2 = 0.
C
N     START OF PERMUTATION LOOP
      REPEAT
N     COMPARE THIS PERMUTATION WITH PREVIOUS
      PERFORM COMPAR
      IF IPS(K) .LT. NUP
      THEN
N     EXPAND NUMBER OF ELEMENTS IN PERMUTATION
      PERFORM EXPAND
      ELSE
N     DECREASE NUMBER OF ELEMENTS IN PERMUTATION
      PERFORM COMPRS
      CIF
N     END IF FIRST VALID ELEMENT IS THE SECOND VECTOR
      UNTIL IPS(1) .NE. 1
C
C
N     SQRT( MAX (SUM OVER PERM. OF MOMENTA ))
      AXISL = SQRT( PM2)
      FOR  J=1,3
N     THRUST AXIS
      AXIS(J) = AXIS(J) / AXISL
      CFOR
      PSUM = 0.
      NIPINC = NTOT * 4
      FOR J = 4,NIPINC,4
N     SUM OF MOMENTA
      PSUM = PSUM + PIN(J)
      CFOR
N     THRUST VALUE
      PTHR = 2. * AXISL / PSUM
      CPROC
C
C
C--------------
C
      PROC THREXP
C--------------
N     COMPUTE THRUST EXPLICITLY
      NIPINC = NP1*IPINC
      SPCOS = 0.
      PSUM = 0.
      FOR J=1,NIPINC,IPINC
      SPCOS = SPCOS + ABS( AXIS(1)*P(J)+AXIS(2)*P(J+1)+AXIS(3)*P(J+2) )
      PSUM = PSUM + P(J+3)
      CFOR
      PTHR = SPCOS / PSUM
      CPROC
C
C
C--------------
C
      PROC COMPAR
C--------------
N     CHECK THIS PERMUTATION AND STORE IN AXIS IF .GT. THAN ALL PREVIOUS
      J = IPS(K)
      PS(K+1) = PS(K-3) + PIN(J  )
      PS(K+2) = PS(K-2) + PIN(J+1)
      PS(K+3) = PS(K-1) + PIN(J+2)
      PCL2 = PS(K+1)*PS(K+1) + PS(K+2)*PS(K+2) + PS(K+3)*PS(K+3)
      IF  PCL2 .GT. PM2
      THEN
      PM2 = PCL2
      AXIS(1) = PS(K+1)
      AXIS(2) = PS(K+2)
      AXIS(3) = PS(K+3)
      CIF
      CPROC
C
C
C-----------------
C
      PROC EXPAND
C-----------------
N     INCREASE NUMBER OF COMPARED ELEMENTS BY 1
      K = K + 4
      IPS(K) = IPS(K-4) + 4
      CPROC
C
C
C-----------------
C
      PROC COMPRS
C-----------------
N     DECREASE NUMBER OF COMPARED ELEMENTS BY 1
      K = K - 4
      IPS(K) = IPS(K) + 4
N     CHECK REMAINING TIME
      IF  JUHR(NSEC) .EQ. 2
      THEN
      PERFORM ERROR
      RETURN
      CIF
      CPROC
C
C
C-----------------
C
      PROC ERROR
C-----------------
N     SET DEFAULT FOR AXIS AND THRUST
      AXIS(1) = 0.
      AXIS(2) = 0.
      AXIS(3) = 1.
      PTHR = .5
      ITMERR = 1
      WRITE(6,9102) NSEC,NP,NMAXP,NLIMIT
 9102 FORMAT(' +++++++   TIME PROBLEMS IN THRUST - ROUTINE   ++++++++'/
     *       '        REMAINING TIME IS LESS THAN ',I4,' SECS.'/
     *       '        ',I4,' ARE TOO MANY INPUT VECTORS.  NMAXP =',I4,
     *       ' NLIMIT =',I4/
     *       '         AXIS WAS SET TO Z AXIS AND PTHR = .5')
      CPROC
C
C
      END
C   22/09/79 807251900  MEMBER NAME  THRUST   (JADEGS)      SHELTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE THRUST( P, NP, NMAXP, PTHR, AXIS, ITMERR )
C-----------------------------------------------------------------------
C
C    VERSION OF 21/05/79   LAST MOD 16/05/80   E.ELSEN
C
C    INPUT : P(4,NP)   NP MOMENTA PX,PY,PZ,ABS(P)
C            NP        NUMBER OF MOMENTA
C            NMAXP     CUTOFF FOR PERMUTED MOMENTA
C                      =0 MEANS NO CUTOFF
C    OUTPUT: PTHR      THRUST VALUE
C            AXIS      THRUST AXIS
C            ITMERR    =1, IF LEFT TIME IS NOT ENOUGH TO CALCULATE THRST
C                      (NOW MIN.2 SECONDS ARE REQUIRED.)
C    METHOD: MOMENTUM CONSERVATION IS TAKEN CARE OF BY INTRODUCING
C            MISSING MOMENTUM VECTOR AT THE END OF P ARRAY.
C            THRUST VALUE IS FOUND BY LOOKING THROUGH ALL
C            PERMUTATIONS OF THE MOMENTA.
C
C-----------------------------------------------------------------------
C
      DIMENSION P(1), AXIS(3)
      DIMENSION PIN(320), IPERM(200),IPS(320),PS(320)
      COMMON / CWORK / WORK(840)
      EQUIVALENCE (WORK(1),PIN(1)),(WORK(321),IPERM(1))
      EQUIVALENCE (WORK(521),PS(1),IPS(1))
C
      DATA IPINC / 4 /
      DATA NLIMIT / 15 /
      DATA NSEC / 2 /
      DATA IER0 / 0/, IER1 /0/, IER2 /0/
C
C------------------  C O D E  ------------------------------------------
C
      ITMERR = 0
C
      AXIS(1) = 0.
      AXIS(2) = 0.
      AXIS(3) = 1.
      PTHR = .5
C
      IF NP .LE. 0
      THEN
      IF IER0 .LT. 10
      THEN
      WRITE(6,9103)
 9103 FORMAT(' +++ ERROR IN THRUST.  CALLED WITH ZERO PARTICLES +++')
      CIF
      IER0 = IER0 + 1
      RETURN
      CIF
C
C
      NMAXP1 = NMAXP
      IF  NP.GE.NLIMIT .AND. ( NMAXP.EQ.0 .OR. NMAXP.GT.NLIMIT )
      THEN
N     WARNING CONCERNING NUMBER OF PARAMETERS
      IF IER1 .LT. 10
      THEN
      WRITE(6,9101) NP,NLIMIT,NMAXP
 9101 FORMAT(' +++++  WARNING FROM THRUST ROUTINE  +++++'/
     *       2X,I4,' ARE TOO MANY INPUT VECTORS. INTERNAL LIMIT=',I4,
     *' INPUT CUTOFF NMAXP=',I4,'. ONLY NLIMIT PARTICLES ARE PERMUTED')
C
      NMAXP1 = NLIMIT
      CIF
      IER1 = IER1 + 1
      CIF
C
      NTOT = NP
N     BALANCE MOMENTA IN P ARRAY
      CALL BALANC( P, NTOT, PSUM )
      IF PSUM .LT. 1.E-6
      THEN
      IF IER2 .LT. 10
      THEN
      WRITE(6,9104)
 9104 FORMAT(' +++ ERROR IN THRUST. MOMENTUM SUM AFTER BALANCE=0. +++')
      CIF
      IER2 = IER2 + 1
      RETURN
      CIF
C
      IF NTOT.GT.NMAXP1 .AND. NMAXP1.NE. 0
      THEN
N     SORT MOMENTA IN DECREASING ORDER
      PERFORM SORT
C
      NP1 = NTOT
      NTOT = NMAXP1 - 1
N     PIN ARRAY = ORDERED P ARRAY
      PERFORM FILLIN
C
N     BALANCE REDUCED SET OF MOMENTA
      CALL BALANC( PIN, NTOT, PSUM1 )
C
N     THRUST AXIS FOR PIN ARRAY
      PERFORM THRST
N     THRUST VALUE FOR P ARRAY
      PERFORM THREXP
      ELSE
N     COPY FROM P TO PIN ARRAY
      CALL UCOPY( P, PIN, NTOT*4 )
N     THRUST FOR PIN ARRAY
      PERFORM THRST
      CIF
      RETURN
C
C
C
C--------------
C
      PROC SORT
C--------------
N     ORDER P ARRAY USING SHELLSORT ALGTHM
N     SEQUENCE OF MOMENTA IS STORED IN IPERM
      FOR J=1,NTOT
N     INIT IPERM IN NATURAL ORDER
      IPERM(J) = J*4
      CFOR
C
      M = NTOT / 2
      WHILE  M.GT.0
      K = NTOT - M
      FOR J=1,K
      I = J
      WHILE  I.GT.0
      ILOW = IPERM(I)
      IHIGH = IPERM(I+M)
      IF  P(IHIGH) .GT. P(ILOW)
      THEN
      IPERM(I) = IHIGH
      IPERM(I+M) = ILOW
      I = I - M
      ELSE
      XWHILE
      CIF
      CWHILE
      CFOR
      M = M/2
      CWHILE
      CPROC
C
C
C--------------
C
      PROC FILLIN
C--------------
N     PIN(1..4,J) = P(1..4,IPERM(J)/4)
      FOR J=1,NTOT
      IPJ = IPERM(J)
      J4 = J*4
      PIN(J4  ) = P(IPJ  )
      PIN(J4-1) = P(IPJ-1)
      PIN(J4-2) = P(IPJ-2)
      PIN(J4-3) = P(IPJ-3)
      CFOR
      CPROC
C
C
C----------------
      PROC THRST
C----------------
      NPERM = 2**(NTOT-1)
      NOUT = ( NPERM - 1) / 8192 + 1
      NIN = MIN0( 8192, NPERM )
      NUP = NTOT*4 - 3
N     START VALUES
      PS(1) = 0.
      PS(2) = 0.
      PS(3) = 0.
      IPS(4) = 1
      PM2 = 0.
      J = 1
      K = 1
C
N     LOOPS OVER PERMUTATIONS
      FOR IOUT = 1, NOUT
      IF JUHR(NSEC) .EQ. 2
      THEN
N     SET DEFAULT FOR AXIS AND THRUST
      AXIS(1) = 0.
      AXIS(2) = 0.
      AXIS(3) = 1.
      PTHR = .5
      ITMERR = 1
      WRITE(6,9102) NSEC
 9102 FORMAT(' +++++++   TIME PROBLEMS IN THRUST - ROUTINE   ++++++++'/
     *       '        REMAINING TIME IS LESS THAN ',I4,' SECS.'/
     *       '         AXIS WAS SET TO Z AXIS AND THRUST = .5')
      RETURN
      CIF
C
      FOR IIN = 1, NIN
C
      PS1 = PS(K  ) + PIN(J  )
      PS2 = PS(K+1) + PIN(J+1)
      PS3 = PS(K+2) + PIN(J+2)
      PCL2 = PS1*PS1 + PS2*PS2 + PS3*PS3
      IF  PCL2 .GT. PM2
      THEN
      PM2 = PCL2
      AXIS(1) = PS1
      AXIS(2) = PS2
      AXIS(3) = PS3
      CIF
C
      IF J .LT. NUP
N     EXPAND
      THEN
      J = J + 4
      K = K + 4
      PS(K  ) = PS1
      PS(K+1) = PS2
      PS(K+2) = PS3
      IPS(K+3) = J
C
N     COMPRESS
      ELSE
      J = IPS(K+3)
      K = K - 4
      CIF
C
      CFOR
      CFOR
C
N     SQRT( MAX (SUM OVER PERM. OF MOMENTA ))
      AXISL = SQRT( PM2)
      FOR  J=1,3
N     THRUST AXIS
      AXIS(J) = AXIS(J) / AXISL
      CFOR
N     THRUST VALUE
      PTHR = 2. * AXISL / PSUM
      CPROC
C
C
C--------------
C
      PROC THREXP
C--------------
N     COMPUTE THRUST EXPLICITLY
      NIPINC = NP1*IPINC
      SPCOS = 0.
      FOR J=1,NIPINC,IPINC
      SPCOS = SPCOS + ABS( AXIS(1)*P(J)+AXIS(2)*P(J+1)+AXIS(3)*P(J+2) )
      CFOR
      PTHR = SPCOS / PSUM
      CPROC
C
C
      END
C   23/05/79            MEMBER NAME  BALANC   (PHYS1)       SHELTRAN
      SUBROUTINE BALANC( P, NTOT, PSUM )
C *---------------------------------------------------------
C *
C *  VERSION OF 23/05/79      LAST MOD 14/05/80     E.ELSEN
C *  PUT IN MOMENTUM BALANCING VECTOR BEHIND THE NTOT MOMENTA
C *  IN P. NTOT IS CHANGED ON RETURN. EXTRA LOCATIONS IN P MUST
C *  BE ACCOUNTED FOR IN CALLING ROUTINE.
C *  PSUM = SUM | P |    FOR BALANCED SET
C *  STORAGE :    P(1..3) = THREE VECTOR
C *               P(4)    = TOTAL MOMENTUM      BOTH REPEATED NTOT TIMES
C *---------------------------------------------------------
C
      DIMENSION P(1)
C
N     FIND SUM OF MOMENTA
      PSUM1 = 0.
      PSUM2 = 0.
      PSUM3 = 0.
      PSUM  = 0.
      N4 = NTOT*4
      FOR J=1,N4,4
      PSUM1 = P(J  ) + PSUM1
      PSUM2 = P(J+1) + PSUM2
      PSUM3 = P(J+2) + PSUM3
      PSUM  = P(J+3) + PSUM
      CFOR
      PTOT = SQRT( PSUM1*PSUM1 + PSUM2*PSUM2 + PSUM3*PSUM3 )
N     EXTRA LOC FOR PSUM IN P IF PSUM .GT. .01 GEV
      IF  PTOT .GT. .01
      THEN
      P(N4+1) = -PSUM1
      P(N4+2) = -PSUM2
      P(N4+3) = -PSUM3
      P(N4+4) = PTOT
      PSUM = PSUM + PTOT
N     INCREMENT NUMBER OF MOMENTA
      NTOT = NTOT + 1
      CIF
C
      RETURN
      END
C   09/06/83 807271340  MEMBER NAME  XYRFTV   (JADEGS)      SHELTRAN
      SUBROUTINE XYRFTV(MODE)
C-----------------------------------------------------------------------
C                                   J. SPITZER 13/3/87
C        FIT ALL TRACKS WITH OR WITHOUT CONSTRAINT TO RUN VERTEX
C        INPUT :
C        MODE   = 0 : OVERWRITE OLD PATR-BANK WITH NEW RESULTS
C        MODE   = 1 : CREATE NEW PATR-BANK WITH NEW RESULTS
C        MODE   + 2 : NOT USED
C        MODE   + 4 : VERTEX WEAKLY CONSTRAINED (ERRFAC = 100.0)
C        MODE   + 8 : NOT USED
C        MODE   +16 : NO VERTEX CONSTRAINT (ERRFAC = 1000.0 )
C        MODE   +32 : UPDATE OR CREATE JHTL IN PARALLEL WITH PATR.
C                     IF A NEW PATR IS TO BE CREATED OR THE OLD
C                       PATR IS TO BE OVERWRITTEN AND THERE IS NO
C                       JHTL WITH THE SAME NUMBER A NEW JHTL WILL
C                       BE CREATED.
C                     OTHERWISE THE OLD JHTL IS OVERWRITTEN.
C  MODIFICATIONS:
C     27/7/88    EXTRA BITS SET TO DISTINGUISH STRONG AND WEAK
C                CONSTRAINS       E ELSEN
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
      COMMON/XYFVT1/MODXYV
C
      DATA LBINIT /0/, IQPATR/0/, IQJHTL/0/
C
      INTEGER WEAK / Z80000 /, STRONG / Z40000 /, CONCLR,
     *        CONSTR / Z400 /
C
N     INITIALIZATION
      IF LBINIT .LE.0
      THEN
         LBINIT = 1
         IQPATR = IBLN('PATR')
         IQJHTL = IBLN('JHTL')
C
         CONCLR = LCOMPL(LOR(STRONG,WEAK))
C
         IF LAND(MODE,16).NE.0
         THEN
            WRITE(6,81)
 81         FORMAT(' *** XYRFTV WITHOUT VERTEX CONSTRAINT ***')
         ELSE
            IF LAND(MODE,4).EQ.0
            THEN
               WRITE(6,82)
 82            FORMAT(' *** XYRFTV WITH VERTEX CONSTRAINT ***')
               WRITE(6,84)
 84            FORMAT(' VC NOT APPLIED IF VERTEX INCOMPATIBLE',
     +         ' WITH TRACK FITTED W/O VC FIRST.')
            ELSE
               WRITE(6,83)
 83            FORMAT(' *** XYRFTV WITH WEAK VERTEX CONSTRAINT ***')
               WRITE(6,84)
            CIF
         CIF
      CIF
C
C
N     CHECK IF PATR- AND JHTL-BANK
      IPPAT0 = IDATA(IQPATR)
      IF(IPPAT0.LE.0 .OR. IDATA(IQJHTL).LE.0 ) RETURN
C
      NTR    = IDATA(IPPAT0+2)
C
N     CHECK IF 1 TRACK
      IF(NTR.LT.1) RETURN
C
N     CREATE NEW PATR BANK
      IF LAND(MODE,1) .NE. 0
      THEN
         NBNK1  = IDATA(IPPAT0-2) - 1
         NWRD   = IDATA(IPPAT0)
         NBYTE  = NWRD*4
         CALL CCRE(IPPATR,'PATR',NBNK1,NWRD,IERR)
         IF IERR.NE.0
         THEN
            PRINT 2900, IERR
 2900       FORMAT(' CREATION OF NEW PATR-BANK RESULTED IN ERROR',I3)
            RETURN
         CIF
N        COPY CONTENTS OF 'PATR'-BANK
         CALL MVCL(IDATA(IPPATR+1),0,IDATA(IPPAT0+1),0,NBYTE)
      CIF
C
N     UPDATE JHTL BANK
      IF LAND(MODE,32) .NE. 0
      THEN
         NBNK1 = IDATA(IDATA(IQPATR)-2)
         IPJHTL = IDATA(IQJHTL)
         NWRD = IDATA(IPJHTL)
         CALL CLOC( NPJHTL, 'JHTL', NBNK1, IER )
         IF NPJHTL.LE.0
         THEN
           CALL CCRE(NPJHTL,'JHTL',NBNK1,NWRD,IERR)
           IF IERR.NE.0
           THEN
              PRINT 2910, IERR
 2910         FORMAT(' CREATION OF NEW JHTL-BANK RESULTED IN ERROR',I3)
              RETURN
           CIF
N        COPY CONTENTS OF 'JHTL'-BANK
           CALL MVCL(IDATA(NPJHTL+1),0,IDATA(IPJHTL+1),0,NWRD*4)
         CIF
      CIF
C
      IPPATR = IDATA(IQPATR)
      IPTR   = IDATA(IPPATR+1) + IPPATR
      LDTR   = IDATA(IPPATR+3)
C
      IF LAND(MODE,16) .NE. 0
      THEN
         ERRFAC = 1000.0
         KIND = 0
      ELSE
         IF LAND(MODE,4)  .NE. 0
         THEN
            ERRFAC =  100.0
            KIND = WEAK
         ELSE
            ERRFAC = 1.0
            KIND = STRONG
         CIF
      CIF
C
      MODXYV=MODE
      FOR ITR=1,NTR
N        R-PHI FIT
         CALL XYRFT1(IPTR,IDATA(IQJHTL),ERRFAC,LDTR)
N        CLEAR CONSTRAIN BITS
         IDATA(IPTR+2) = LAND(IDATA(IPTR+2),CONCLR)
         IF  LAND(IDATA(IPTR+2),CONSTR).NE.0
         THEN
            IDATA(IPTR+2) = LOR(IDATA(IPTR+2),KIND)
         CIF
         IPTR=IPTR+LDTR
      CFOR
C
      RETURN
      END
      SUBROUTINE XYRFTV(MODE)
C-----------------------------------------------------------------------
C                                   J. SPITZER 13/3/87
C        FIT ALL TRACKS WITH OR WITHOUT CONSTRAINT TO RUN VERTEX
C        INPUT :
C        MODE   = 0 : OVERWRITE OLD PATR-BANK WITH NEW RESULTS
C        MODE   = 1 : CREATE NEW PATR-BANK WITH NEW RESULTS
C        MODE   + 2 : NOT USED
C        MODE   + 4 : VERTEX WEAKLY CONSTRAINED (ERRFAC = 100.0)
C        MODE   + 8 : NOT USED
C        MODE   +16 : NO VERTEX CONSTRAINT (ERRFAC = 1000.0 )
C        MODE   +32 : UPDATE OR CREATE JHTL IN PARALLEL WITH PATR.
C                     IF A NEW PATR IS TO BE CREATED OR THE OLD
C                       PATR IS TO BE OVERWRITTEN AND THERE IS NO
C                       JHTL WITH THE SAME NUMBER A NEW JHTL WILL
C                       BE CREATED.
C                     OTHERWISE THE OLD JHTL IS OVERWRITTEN.
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
      COMMON/XYFVT1/MODXYV
C
      DATA LBINIT /0/, IQPATR/0/, IQJHTL/0/
C
N     INITIALIZATION
      IF LBINIT .LE.0
      THEN
         LBINIT = 1
         IQPATR = IBLN('PATR')
         IQJHTL = IBLN('JHTL')
         IF LAND(MODE,16).NE.0
         THEN
            WRITE(6,81)
 81         FORMAT(' *** XYRFTV WITHOUT VERTEX CONSTRAINT ***')
         ELSE
            IF LAND(MODE,4).EQ.0
            THEN
               WRITE(6,82)
 82            FORMAT(' *** XYRFTV WITH VERTEX CONSTRAINT ***')
               WRITE(6,84)
 84            FORMAT(' VC NOT APPLIED IF VERTEX INCOMPATIBLE',
     +         ' WITH TRACK FITTED W/O VC FIRST.')
            ELSE
               WRITE(6,83)
 83            FORMAT(' *** XYRFTV WITH WEAK VERTEX CONSTRAINT ***')
               WRITE(6,84)
            CIF
         CIF
      CIF
C
C
N     CHECK IF PATR- AND JHTL-BANK
      IPPAT0 = IDATA(IQPATR)
      IF(IPPAT0.LE.0 .OR. IDATA(IQJHTL).LE.0 ) RETURN
C
      NTR    = IDATA(IPPAT0+2)
C
N     CHECK IF 1 TRACK
      IF(NTR.LT.1) RETURN
C
N     CREATE NEW PATR BANK
      IF LAND(MODE,1) .NE. 0
      THEN
         NBNK1  = IDATA(IPPAT0-2) - 1
         NWRD   = IDATA(IPPAT0)
         NBYTE  = NWRD*4
         CALL CCRE(IPPATR,'PATR',NBNK1,NWRD,IERR)
         IF IERR.NE.0
         THEN
            PRINT 2900, IERR
 2900       FORMAT(' CREATION OF NEW PATR-BANK RESULTED IN ERROR',I3)
            RETURN
         CIF
N        COPY CONTENTS OF 'PATR'-BANK
         CALL MVCL(IDATA(IPPATR+1),0,IDATA(IPPAT0+1),0,NBYTE)
      CIF
C
N     UPDATE JHTL BANK
      IF LAND(MODE,32) .NE. 0
      THEN
         NBNK1 = IDATA(IDATA(IQPATR)-2)
         IPJHTL = IDATA(IQJHTL)
         NWRD = IDATA(IPJHTL)
         CALL CLOC( NPJHTL, 'JHTL', NBNK1, IER )
         IF NPJHTL.LE.0
         THEN
           CALL CCRE(NPJHTL,'JHTL',NBNK1,NWRD,IERR)
           IF IERR.NE.0
           THEN
              PRINT 2910, IERR
 2910         FORMAT(' CREATION OF NEW JHTL-BANK RESULTED IN ERROR',I3)
              RETURN
           CIF
N        COPY CONTENTS OF 'JHTL'-BANK
           CALL MVCL(IDATA(NPJHTL+1),0,IDATA(IPJHTL+1),0,NWRD*4)
         CIF
      CIF
C
      IPPATR = IDATA(IQPATR)
      IPTR   = IDATA(IPPATR+1) + IPPATR
      LDTR   = IDATA(IPPATR+3)
C
      ERRFAC = 1.0
      IF(LAND(MODE,4)  .NE. 0) ERRFAC =  100.0
      IF(LAND(MODE,16) .NE. 0) ERRFAC = 1000.0
C
      MODXYV=MODE
      FOR ITR=1,NTR
N        R-PHI FIT
         CALL XYRFT1(IPTR,IDATA(IQJHTL),ERRFAC,LDTR)
         IPTR=IPTR+LDTR
      CFOR
C
      RETURN
      END
C   09/06/83 809021126  MEMBER NAME  XYRFT1   (JADEGS)      SHELTRAN
      SUBROUTINE XYRFT1(IPTR,IPJHTL,ERRFAC,LDTR)
C
C        REFIT TRACK ITRK IN 'PATR'-BANK (WITH A VERTEX
C        CONSTRAINT OF STRENGTH 1/ERRFAC IF ERRFAC<200.
C        VERTEX OMITTED IF INCOMPATIBLE.)
C        PARABOLA FIT IF |OLD CURVATURE * HALF TRACK LENGTH| < .04
C        CIRCLE FIT OTHERWISE
C
C    TEST VERSION 3.     (TESTED TO SOME EXTENT)
C    18.3.88   PROPER RUN NUMBER HANDLING USING LDATYP      E E
C    22.2.88   MVC CHANGED TO MVCL (256 BYTES NOT ENOUGH!)  J.H./J.O.
C     2.9.88   SI --> SID IN COV EXPRESSION                 C.K./E.E.
C
C                                J. SPITZER  25/3/87
C
C    EXTENDED TO GIVE COVARIANCE MATRIX FOR FIT PARAMETERS
C    CIRCLE PARAMETERS ARE SET EVEN IF PARABOLA FIT WAS PERFORMED
C                                      J.S.  2/4/87
C
C    DOUBLE PRECISION TO CALCULATE DETERMINANT FOR COVARIANCE MATRIX
C                                      J.S.  5/6/87
C
C    MODIFIED TO UPDATE JHTL BANK UPON REQUEST (MADE IN MODXYV)
C    + SMALL MODIFICATIONS IN THE WORK COMMON FOR THE
C      VERTEX CHAMBER GROUP
C                                      J.S.  5/1/88
C
      IMPLICIT INTEGER*2 (H)
      REAL*8   S0D,S1D,S2D,S3D,S4D,S8D,DETD
C
      COMMON/XYFVT1/MODXYV
C
C
#include "cdata.for"
C
#include "cgeo1.for"
C
#include "calibr.for"
C
#include "cworkpr.for"
#include "cworkeq.for"
C
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
C
#include "cpatlm.for"
C
#include "cjdrch.for"
#include "cdsmax.for"
      INTEGER DATE(5), IDAY /0/
C-----------------------------
      INTEGER NCHECK(5)/5*8/
      REAL RCHECK(12,2,5)/
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1.,
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1.,
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1.,
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1.,
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1./
C
C
      REAL RESCUT/8./,CKAPP/.966/
      DIMENSION ISORT1(71),ISORT2(3,71),IRESHT(71),ISORT3(91)
     +,ISORT4(2,91)
      DATA KPRT1/0/,NPRT1/50/,IQJETC/0/,IQHEAD/0/
      DATA MASK1/Z2FFFFFF/,MASK2/ZFFFF02FF/,MASK3/ZFFFFF1FF/
C
C
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
         LBINIT = 1
C
         IQJETC = IBLN('JETC')
         IQHEAD = IBLN('HEAD')
C
         CALL DAY2(DATE)
         IDAY = DATE(1)*1000 + DATE(2)
C
N       MULT. SCATTERING CONSTANTS
         RESMS = .020**2/2. * .16 * (1. + ALOG10(.16) /9.) * 155.45**2
C
         WRITE(6,137)
 137     FORMAT(/,' *** XYRFT1 ***  A NEW R-PHI FITTING ROUTINE',/,
     +            '                 TEST VERSION 3. (J. SPITZER)',
     +                            ' LAST MOD 02/09/88           ',/
     +     ' BIT 512 OR 1024 IS SET IN THE PROGRAM IDENTIFIER WORD',/,
     +' IN CASE VERTEX CONSTRAINT WAS NOT OR WAS USED RESP.',/,
     +' CORRECT COV MATRIX IS PROVIDED WITH BIT 2048 SWITCHED ON',/,
     +' IF THE PATR BANK ALREADY HAS THE LARGER LENGTH, THE NUMBER',/,
     +' OF HITS USED IN THE FIT IS AT LEAST 10 AND THE FIT CONVERGED.',
     +/)
      CIF
C
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      NEV  = HDATA(IPHEAD+11)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
C
N     RESERVE SPACE IN CWORK
      HPFREE = 1
C
C=======================================================================
N     GET X-Y-VERTEX AND STRENGTH OF VC
      IPV    = ICALIB(10)
      XO     = ACALIB(IPV+ 1)
      YO     = ACALIB(IPV+ 3)
      CURVXY=ADATA(IPTR+25)
      IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
      DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +ADATA(IPTR+9),1./CURVXY,XO,YO,XP,YP,FI)
      FV     = ERRFAC
      IF(FV .LT. .50) FV = .50
      IF LDATYP(DUMMY) .EQ. 1
      THEN
         SRESO=.160
      ELSE
         SRESO=.100
      CIF
      SIGMIN=(SRESO/1.6)**2
      PTRANS = ABS(0.0299792458*BKGAUS/CURVXY) * .001
      RESV=.0100+.25*SIN(FI)**2+RESMS/PTRANS**2
      WGHT0  = (SRESO/FV)**2 / RESV
      INDBIT=512
C=======================================================================
N     HALF DISTANCE BETWEEN FIRST AND LAST POINTS ON TRACK
      XX    =  ADATA(IPTR+12) - ADATA(IPTR+5)
      YY    =  ADATA(IPTR+13) - ADATA(IPTR+6)
      RR    = 0.5*SQRT(XX**2+YY**2)
C
      IF RR.LT.10.
      THEN
         IF KPRT1.LT.NPRT1
         THEN
            KPRT1=KPRT1+1
            WRITE(6,848) NRUN,NEV,ITRK,RR
848         FORMAT(' ******** RUN,EV,TRACK',I8,I6,I3,/,
     +      ' HALF DISTANCE OF FIRST AND LAST POINTS',E14.3,
     +      ', XYRFT1 DOES NOT ATTEMPT R-PHI FIT')
         CIF
         RETURN
      CIF
C
C
N     FETCH HITS, CALCULATE COORDINATES, AND
N     FILL ARRAY IN /CWORK/
      HPCO0  = HPFREE
      LHIT   = 14
C     ORIGIN HALFWAY BETWEEN FIRST AND LAST POINTS ON TRACK
      INDFET = 2
      CALL JFETCH(IPTR,IPJHTL,WRK(HPCO0),LHIT,IPRES,INDFET,XO,YO)
C
C
      HLDCO  = LHIT
      HPCO9  = IPRES - 1
      HPAR0  = IPRES
      HLDPA  = 20
      HPAR9  = HPAR0 + HLDPA - 1
      HPFREE = HPAR9 + 1
      XT     = WRK (IPRES   )
      YT     = WRK (IPRES+ 1)
      CSROT  = WRK (IPRES+ 2)
      SNROT  = WRK (IPRES+ 3)
N     JADE ORIGIN IN THE FIT SYSTEM
      XOR    =- XT*CSROT -  YT*SNROT
      YOR    =  XT*SNROT -  YT*CSROT
N     VERTEX IN THE FIT SYSTEM
      X0     = (XO-XT)*CSROT+(YO-YT)*SNROT
      Y0     =-(XO-XT)*SNROT+(YO-YT)*CSROT
C
N     ZVERT, THETA
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTH   = WRK (IPRES+11)
      SNTH   = WRK (IPRES+12)
C
      IWRK(IPRES+10)=0
C
C ORIGINAL CHI2 AND CURVATURE ERROR IN PATR BANK
      SIG=ADATA(IPTR+23)**2
      IF(SIG.LT.1.E-5) SIG=1.E-5
      SIG11=(.5*ADATA(IPTR+26))**2/SIG
C
C-----------------------------------------------------------------------
C
C     GYMNASTICS FOR PRIVATE HIT QUALIFICATION
C     AND FOR HANDLING MORE HITS ON SAME WIRE
C
C
C TRY TO RECOVER HITS POSSIBLY LOST BY EARLIER FIT
      XHCUT=RR+200.
C
      XREGA= 100000.
      XREGB=-100000.
      NHALL=0
      NHWIR=0
      NHPOT=0
      IPCO=HPCO0
      REPEAT
         NHWIR=NHWIR+1
         IF(NHWIR.GT.70) RETURN
         ISORT1(NHWIR)=NHWIR
         ISORT2(1,NHWIR)=IPCO
         ISORT2(3,NHWIR)=0
         IW0=IWRK(IPCO)
         ICL0=IWRK(IPCO+9)
         LFL=0
         WHILE IPCO.LE.HPCO9
            IW9=IWRK(IPCO)
            ICL9=IWRK(IPCO+9)
            IF IW9.EQ.IW0 .AND. ICL9.EQ.ICL0
            THEN
N        HIT ON THE SAME WIRE
               NHALL=NHALL+1
               IF(NHALL.GT.90) RETURN
               IF(ISORT2(3,NHWIR).EQ.0) ISORT2(2,NHWIR)=NHALL
               ISORT2(3,NHWIR)=ISORT2(3,NHWIR)+1
C
               XA=WRK(IPCO+3)
               LBGOOD=IWRK(IPCO+10)
               IF LBGOOD.LE.2
               THEN
                  IF ABS(XA).GT.XHCUT
                  THEN
                     ISORT3(NHALL)=-1
                  ELSE
                     ISORT3(NHALL)= 1
                     LFL=1
                     IF(XA.LT.XREGA) XREGA=XA
                     IF(XA.GT.XREGB) XREGB=XA
                  CIF
C
                  IWRK(IPCO+10)=1
               ELSE
                  ISORT3(NHALL)=-1
               CIF
C
               IPCO=IPCO+HLDCO
            ELSE
               XWHILE
            CIF
         CWHILE
         IF LFL.EQ.1
         THEN
            NHPOT=NHPOT+1
         ELSE
            ISORT3(ISORT2(2,NHWIR))=-2
         CIF
      UNTIL IPCO.GT.HPCO9
C-----------------------------------------------------------------------
C
C IF LESS THAN 4 HITS SURVIVE NOTHING WILL BE DONE
      IF(NHPOT.LT.4) RETURN
      XHF=.5*(XREGA+XREGB)
      RRPL=.5*(XREGB-XREGA)
      RRMI=RRPL
      IF ABS(XHF).GT.RR
      THEN
         RRPL=RRPL+XHF-SIGN(RR,XHF)
         RRMI=RRMI-XHF+SIGN(RR,XHF)
         XHF=SIGN(RR,XHF)
      CIF
C-----------------------------------------------------------------------
C  STARTING VALUES OF FIT PARAMETERS
C
      LCHC=0
C  CHANGE START VALUE IF CURVATURE INCONSISTENT WITH FIRST & LAST POINTS
      IF ABS(CURVXY*RR).GT.CKAPP
      THEN
         LCHC=1
         CURVXY=SIGN(CKAPP/RR,CURVXY)
      CIF
      IF ABS(CURVXY)*RR .GT. .04
      THEN
C      CIRCLE FIT
         LFTYP=1
         P1=CURVXY/SQRT(1.-(CURVXY*RR)**2)
         AAH=(RR-XHF)*(RR+XHF)
         PAR3=SAGCIR(1.,P1,AAH,SAGPR,1.E-4)
         PAR2=-P1/(1.+PAR3*P1)*XHF
         CSI2GM=1.+PAR2**2
         PAR1=CURVXY*SQRT(CSI2GM)
      ELSE
C      PARABOLA FIT
         LFTYP=2
         PAR3=.5*CURVXY*(RR-XHF)*(RR+XHF)
         PAR2=-CURVXY*XHF
         CSI2GM=1.+PAR2**2
         PAR1=-0.5*CURVXY*SQRT(CSI2GM)*CSI2GM
      CIF
      XMIN=PAR2/PAR1-CKAPP/ABS(CURVXY)
      XMAX=PAR2/PAR1+CKAPP/ABS(CURVXY)
C
C-----------------------------------------------------------------------
C  FIT SHIFT AND ROTATION ONLY (CURVATURE KEPT FIXED)
      DISCUT=400.
      KFLIP=1
      PERFORM SHFROT
      IF(NHFIT.LT.4) RETURN
      IF NHFIT.LT.6
      THEN
C FIT OF SHIFT AND ROTATION HAS ONLY BEEN PERFORMED
C CHI2 AND CURVATURE ERROR REMAIN THE OLD VALUES IN THE PATR BANK
         PERFORM FITBNK
         RETURN
      CIF
C
C-----------------------------------------------------------------------
      IF ABS(DA).GT.1.5 .OR. ABS(DB).GT..1 .OR. LCHC.EQ.1
      THEN
C SHIFT TO ORIGINAL FIT TOO BIG OR CHANGE OF CURVATURE FOR START
C TRY TO FIND CORRECT STARTING VALUES, TAKE CIRCLE IN ANY CASE
         IF LFTYP.EQ.2
         THEN
            LFTYP=1
            PAR1=CUROUT*SQRT(CSI2GM)
            XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
            XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
         CIF
         PERFORM STVCIR
      CIF
C
C
C IF TOO MANY HITS THROWN AWAY EVEN WITH THE VERY LOOSE RESIDUAL CUTS
C DO NOT DARE TO ACCEPT/START THE FIT
C
      IF(NHFIT.LT.NHPOT/2) RETURN
C
C
N      SAVE CURRENT (START) VALUES
      NHFTLS=NHFIT
      PAR1LS=PAR1
      PAR2LS=PAR2
      PAR3LS=PAR3
      CSI2LS=CSI2GM
      CURLST=CUROUT
      XMAXLS=XMAX
      XMINLS=XMIN
      SIG11L=SIG11
      SIGLST=SIG
C
C
N      VERTEX
      NHWIRV=NHWIR+1
      ISORT1(NHWIRV)=NHWIRV
      ISORT2(1,NHWIRV)=-200
C
C
C=======================================================================
C
N     FIRST ITERATE WITHOUT VERTEX CONSTRAINT
      INDMAX=NHFIT/8+1
      IF(INDMAX.GT.8) INDMAX=8
      INDFIT=0
      WHILE INDFIT.LT.INDMAX
         INDFIT=INDFIT+1
N    PARABOLA OR CIRCLE FIT
         PERFORM FTCURV
         IF LNOCON.EQ.1
         THEN
C NO CONVERGENCE AS INDICATED BY LOSS OF TOO MANY HITS
C STILL RETAIN THE EARLIER FIT
C IF IT IS THE ONE OBTAINED IN PROC STVCIR,
C CHI2 AND CURVATURE ERROR REMAIN THE OLD VALUES IN THE PATR BANK
            NHFIT=NHFTLS
            PAR1=PAR1LS
            PAR2=PAR2LS
            PAR3=PAR3LS
            CSI2GM=CSI2LS
            CUROUT=CURLST
            XMAX=XMAXLS
            XMIN=XMINLS
            SIG11=SIG11L
            SIG=SIGLST
C
C DO NOT ATTEMPT VERTEX CONSTRAINT
            PERFORM FITBNK
            RETURN
         CIF
         IF(SIG.LT.SIGMIN) XWHILE
         IF INDFIT.GE.2
         THEN
            PERFORM LLSTOP
            IF LSTOP.EQ.1
            THEN
C      PREVIOUS FIT ACCEPTED, RESTORE ITS RESULTS
               INDFIT=INDFIT-1
               NHFIT=NHFTLS
               PAR1=PAR1LS
               PAR2=PAR2LS
               PAR3=PAR3LS
               CSI2GM=CSI2LS
               CUROUT=CURLST
               XMAX=XMAXLS
               XMIN=XMINLS
               SIG11=SIG11L
               SIG=SIGLST
               KFLIP=3-KFLIP
               XWHILE
            CIF
         CIF
         IF(INDFIT.EQ.INDMAX) XWHILE
N      SAVE FIT RESULTS
         NHFTLS=NHFIT
         PAR1LS=PAR1
         PAR2LS=PAR2
         PAR3LS=PAR3
         CSI2LS=CSI2GM
         CURLST=CUROUT
         XMAXLS=XMAX
         XMINLS=XMIN
         SIG11L=SIG11
         SIGLST=SIG
N      HIT CLEANING
         PERFORM HITCLN
      CWHILE
C
C            ======  VERTEX CONSTRAINT  =======
      X0R=X0-XHF
      IF FV.LT.200. .AND. X0R.GT.XMIN .AND. X0R.LT.XMAX
      THEN
C        VERTEX CONSTRAINT (WEEK OR STRONG) HAS BEEN REQUESTED
C        ROUGH CHECK IF RUN VERTEX CONSISTENT WITH THE TRACK
         IF LFTYP.EQ.2
         THEN
            DVCHI2=((PAR1*X0R+PAR2)*X0R+PAR3-Y0)**2*WGHT0
         ELSE
            AAH=-X0R**2*CSI2GM
            FDBPR=1./(1.+PAR1*X0R*PAR2)
            SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
            DVCHI2=(SAG+PAR2*X0R+PAR3-Y0)**2*WGHT0
         CIF
         IF DVCHI2 .LT.  9.*SIG
         THEN
            ISORT2(1,NHWIRV)=-100
            PERFORM FTCURV
            IF(LNOCON.EQ.0) INDBIT=1024
         CIF
      CIF
C
N     SET UP PATR-BANK
      PERFORM FITBNK
      RETURN
C=======================================================================
C
N     *************************
N     *      F T C U R V      *
N     *************************
C
C
N      PARABOLA OR CIRCLE FIT
      PROC FTCURV
C
      LNOCON=0
N     GET EQUATIONS
N     WEIGHT VERTEX AS POINT OF PARABOLA
      KFLIP=3-KFLIP
      KITER=0
      WHILE KITER .LT. 3-LFTYP
         KITER=KITER+1
         X0R=X0-XHF
         IF ISORT2(1,NHWIRV).EQ.-100 .AND. X0R.GT.XMIN .AND. X0R.LT.XMAX
         THEN
N      VERTEX INCLUDED
            IF LFTYP.EQ.2
            THEN
               DYDP1=X0R**2
               DYDP2=X0R
               DYRES=Y0
            ELSE
               AAH=-X0R**2
               FDBPR=1./(1.+PAR1*X0R*PAR2)
               SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
               CC1=FDBPR/(1.+SAG*PAR1*FDBPR)
               DYDP1=CC1*SAGPR
               DYDP2=X0R+PAR1*CC1*(AAH*PAR2-SAG*X0R)
               DYRES=Y0-SAG-PAR2*X0R-PAR3
            CIF
            S0 = WGHT0
            S1=DYDP2*WGHT0
            S2=DYDP1*WGHT0
            S3=S2*DYDP2
            S4=S2*DYDP1
            S8=S1*DYDP2
            S7=DYRES*WGHT0
            S6=S7*DYDP2
            S5=S7*DYDP1
         ELSE
N      VERTEX OMITTED
            S0 = 0.
            S1 = 0.
            S2 = 0.
            S3 = 0.
            S4 = 0.
            S8 = 0.
            S7 = 0.
            S6 = 0.
            S5 = 0.
         CIF
         S00=S0
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
            CFOR
            IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).EQ.1
                  THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR=XA-XHF
                     IF XAR.GT.XMIN.AND.XAR.LT.XMAX
                     THEN
                        YA = WRK(IPCO+4)
                        IF LFTYP.EQ.2
                        THEN
                           DYDP1A=XAR**2
                           DYDP2A=XAR
                           DYRESA=YA
                           DF0=ABS(YA-((PAR1*XAR+PAR2)*XAR+PAR3))
                        ELSE
                           AAH=-XAR**2
                           FDBPR=1./(1.+PAR1*XAR*PAR2)
                           SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
                           CC1=FDBPR/(1.+SAG*PAR1*FDBPR)
                           DYDP1A=CC1*SAGPR
                           DYDP2A=XAR+PAR1*CC1*(AAH*PAR2-SAG*XAR)
                           DYRESA=YA-SAG-PAR2*XAR-PAR3
                           DF0=ABS(DYRESA)
                        CIF
                     ELSE
                        DF0=15000.
                     CIF
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        DYDP1=DYDP1A
                        DYDP2=DYDP2A
                        DYRES=DYRESA
                        JHUSE=JH
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.RESCUT
               THEN
                  S0=S0+1.
                  S1=S1+DYDP2
                  S2=S2+DYDP1
                  S3=S3+DYDP1*DYDP2
                  S4=S4+DYDP1**2
                  S8=S8+DYDP2**2
                  S7=S7+DYRES
                  S6=S6+DYRES*DYDP2
                  S5=S5+DYRES*DYDP1
                  ISORT4(KFLIP,JHUSE)=1
               ELSE
                  ISORT3(IH)=-2
               CIF
            CIF
         CFOR
         NHF1=S0-S00+.1
         IF NHF1.LT.6 .OR. NHF1.LT.NHPOT/2
         THEN
            LNOCON=1
            XWHILE
         CIF
         NHFIT=NHF1
         DEG   = S0 - S00 - 3.
N     CURVATURE ERROR
         DET = (S8*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S8)*S2
         SIG11 = (S8*S0 - S1*S1)/DET
C
N        SOLVE EQUATIONS
         F1 = 1. / S4
         XX12 = S3*F1
         XX13 = S2*F1
         YY1  = S5*F1
         XX22 = S8 - S3*XX12
         XX23 = S1 - S3*XX13
         YY2  = S6 - S3*YY1
         XX32 = S1 - S2*XX12
         XX33 = S0 - S2*XX13
         YY3  = S7 - S2*YY1
         IF XX22.GT.XX32
         THEN
            XX23 = XX23 / XX22
            YY2  = YY2  / XX22
            PARR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
            PARR2 = YY2 - XX23*PARR3
         ELSE
            XX33 = XX33 / XX32
            YY3  = YY3  / XX32
            PARR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
            PARR2 = YY3 - XX33*PARR3
         CIF
         PARR1 = YY1 - XX12*PARR2 - XX13*PARR3
C
         IF LFTYP.EQ.2
         THEN
            PAR1=PARR1
            PAR2=PARR2
            PAR3=PARR3
            IF(ABS(PAR1).LT.1.E-10) PAR1 = SIGN(1.E-10,PAR1)
            CSI2GM=PAR2**2+1.
            CUROUT =-PAR1 * 2./ (SQRT(CSI2GM)*CSI2GM)
         ELSE
            PAR1=PAR1+PARR1
            PAR2=PAR2+PARR2
            PAR3=PAR3+PARR3
            IF(ABS(PAR1).LT.1.E-10) PAR1 = SIGN(1.E-10,PAR1)
            CSI2GM=PAR2**2+1.
            CUROUT=PAR1/SQRT(CSI2GM)
            XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
            XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
         CIF
      CWHILE
C  END ITERATION DONE IN CASE OF CIRCLE FIT ONLY
C
C
      IF LNOCON.EQ.0
      THEN
N     CALC. CHISQ + SOLVE L/R AMBIGUITY
         CHISQ = 0.
         NHF1=0
         FOR IHWIR=1,NHWIR
            IRESHT(IHWIR)=-1
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            IF ISORT3(IH).GE.0 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).GE.0
                  THEN
                     IFLG=ISORT3(JH)
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XR= WRK(IPCO+3)-XHF
                     IF XR.GT.XMIN.AND.XR.LT.XMAX
                     THEN
                        Y = WRK(IPCO+4)
                        IF LFTYP.EQ.2
                        THEN
                           DF0=ABS(Y-(PAR1*XR+PAR2)*XR-PAR3)
                        ELSE
                           AAH=-XR**2*CSI2GM
                           FDBPR=1./(1.+PAR1*XR*PAR2)
                           SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
                           DF0=ABS(Y-SAG-PAR2*XR-PAR3)
                        CIF
                        IF(DF0.LT.RESMIN) RESMIN=DF0
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.5000.
               THEN
                  IRESHT(IHWIR)=RESMIN*1.E6
                  IF IFLG.EQ.1
                  THEN
                     CHISQ=CHISQ+RESMIN**2
                     NHF1=NHF1+1
                  CIF
               CIF
            CIF
         CFOR
         IF(NHF1.LT.NHFIT-3) LNOCON=1
         SIG    =      CHISQ  / DEG
      CIF
      CPROC
C=======================================================================
      PROC HITCLN
C      LABEL HITS NOT TO BE USED IN THE NEXT ITRATION
N       SQRT(CHI2) OF VERTEX
C  VERTEX NOT INCLUDED IN THE ITERATIVE PART
C  IN THE CURRENT VERSION
CV       X0R=X0-XHF
CV       IF LFTYP.EQ.2
CV       THEN
CV          DFVERT=((PAR1*X0R+PAR2)*X0R+PAR3-Y0)*SQRT(WGHT0)
CV       ELSE
CV          AAH=-X0R**2*CSI2GM
CV          FDBPR=1./(1.+PAR1*X0R*PAR2)
CV          SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
CV          DFVERT=(SAG+PAR2*X0R+PAR3-Y0)*SQRT(WGHT0)
CV       CIF
CV       IRESHT(NHWIRV)=ABS(DFVERT)*1.E6
C-------------------------------------------------------------
C
N       SORT HITS ACCORDING TO RESIDUALS
C  EXCLUDE THE INDFIT LARGEST RESIDUAL HITS,
C  RESTORE THE OTHERS (EXLUDED FOR EVER HITS NOT COUNTED)
C
CV       CALL SHELL9(IRESHT,ISORT1,NHWIRV)
         CALL SHELL9(IRESHT,ISORT1,NHWIR)
         KOMIT=0
CV       FOR J1=1,NHWIRV
         FOR J1=1,NHWIR
CV          IHWIR=ISORT1(NHWIRV+1-J1)
            IHWIR=ISORT1(NHWIR+1-J1)
            IPCO=ISORT2(1,IHWIR)
            IF IPCO.NE.-100 .AND. IPCO.NE.-200
            THEN
N     HIT, NOT VERTEX
               NNH=ISORT2(3,IHWIR)
               IH=ISORT2(2,IHWIR)
               LFLG=0
               FOR JNH=1,NNH
                  IHA=IH+JNH-1
                  IQA=ISORT3(IHA)
                  IF IQA.GT.-1
                  THEN
                     IF LFLG.EQ.0
                     THEN
                        LFLG=1
                        KOMIT=KOMIT+1
                     CIF
                     IF KOMIT.LE.INDFIT
                     THEN
                        ISORT3(IHA)=0
                     ELSE
                        ISORT3(IHA)=1
                     CIF
                  CIF
               CFOR
CV          ELSE
C   VERTEX;   DOES NOT OCCOUR IN THE CURRENT VERSION
CV             KOMIT=KOMIT+1
CV             IF(KOMIT.LE.INDFIT) WGHT0=WGHT0*.01
            CIF
         CFOR
      CPROC
C=======================================================================
      PROC LLSTOP
         IF INDFIT.LE.6
         THEN
            INDCK=INDFIT-1
         ELSE
            INDCK=5
         CIF
         ICHCK=NCHECK(INDCK)
         WHILE SIGLST.LT.RCHECK(ICHCK,1,INDCK)
            ICHCK=ICHCK-1
         CWHILE
         IF(ICHCK.LT.1) ICHCK=1
         IF SIG/SIGLST.GT.RCHECK(ICHCK,2,INDCK)
         THEN
            LSTOP=1
         ELSE
            LSTOP=0
         CIF
      CPROC
C=======================================================================
C
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
N     START + END POINTS
         XST=XHF+XMIN
         IF(XST.LT.XREGA) XST=XREGA
         XSTR=XST-XHF
         XEN=XHF+XMAX
         IF(XEN.GT.XREGB) XEN=XREGB
         XENR=XEN-XHF
         IF LFTYP.EQ.2
         THEN
            YST  = (PAR1 *XSTR+ PAR2 )*XSTR+ PAR3
            YEN  = (PAR1 *XENR+ PAR2 )*XENR+ PAR3
N     DIRECTION AT START + END POINT
            TGST = PAR1*XSTR*2.+ PAR2
            TGEN = PAR1*XENR*2.+ PAR2
         ELSE
            AAH=-XSTR**2
            FDBPR=1./(1.+PAR1*XSTR*PAR2)
            SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
            YST=SAG+PAR2*XSTR+PAR3
         TGST=PAR2-PAR1*FDBPR/(1.+SAG*PAR1*FDBPR)*(SAG*PAR2+XSTR*CSI2GM)
            AAH=-XENR**2
            FDBPR=1./(1.+PAR1*XENR*PAR2)
            SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
            YEN=SAG+PAR2*XENR+PAR3
         TGEN=PAR2-PAR1*FDBPR/(1.+SAG*PAR1*FDBPR)*(SAG*PAR2+XENR*CSI2GM)
         CIF
         DXST = 1./SQRT(TGST**2+1.)
         DYST = DXST * TGST
         DXEN = 1./SQRT(TGEN**2+1.)
         DYEN = DXEN * TGEN
C
C
C
N     COPY TRACK BANK
         HPTR0 = HPFREE
         CALL MVCL(IWRK(HPTR0),0,IDATA(IPTR+1),0,4*LDTR)
C
N     FILL FIT-BANK
         IP    = HPTR0 - 1
         IWRK(IP+2) = LAND(IWRK(IP+2),MASK3)
         IWRK(IP+2) = LOR(IWRK(IP+2),INDBIT)
         IWRK(IP+ 3) = IDAY
         WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
         WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
         WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2)*TGTH+ZVERT
         DXSTJ       =  DXST*CSROT - DYST*SNROT
         DYSTJ       =  DXST*SNROT + DYST*CSROT
         WRK (IP+ 8) =  DXSTJ*CSTH
         WRK (IP+ 9) =  DYSTJ*CSTH
         WRK (IP+12) = XEN *CSROT - YEN *SNROT + XT
         WRK (IP+13) = XEN *SNROT + YEN *CSROT + YT
         WRK (IP+14) = SQRT(WRK(IP+12)**2 + WRK(IP+13)**2)*TGTH+ZVERT
         WRK (IP+15) = (DXEN*CSROT - DYEN*SNROT)*CSTH
         WRK (IP+16) = (DXEN*SNROT + DYEN*CSROT)*CSTH
         IWRK(IP+24) = NHFIT
         WRK (IP+25) = CUROUT
         WRK (IP+27) = CUROUT
         WRK (IP+28) = CUROUT
C
         WRK (IP+23) = SQRT(SIG)
         WRK (IP+26) = SQRT(SIG*SIG11/CSI2GM)
         IF(LFTYP.EQ.2) WRK(IP+26)=WRK(IP+26)*2./CSI2GM
C
C        IWRK(IP+18) = LFTYP
C EVEN IF PARABOLA FIT WAS DONE, CIRCLE PARAMETERS ARE STORED
         IWRK(IP+18) = 1
C
         PAR1=CUROUT*SQRT(CSI2GM)
         SIGNC=SIGN(1.,CUROUT)
         ACURV=ABS(CUROUT)
         A=((XHF-XOR)*PAR2-PAR3+YOR)/SQRT(CSI2GM)
         B=(XHF-XOR+(PAR3-YOR)*PAR2)/SQRT(CSI2GM)
         FDBPR=1./ACURV+SIGNC*A
         IF FDBPR.LT.100.
         THEN
            DIMP=-1./ACURV+SQRT(FDBPR**2+B**2)
         ELSE
            DIMP=SIGNC*A+SAGCIR(1./(1.+CUROUT*A),ACURV,B**2,SP,1.E-4)
         CIF
         FDBPR=1.+DIMP*ACURV
         IF FDBPR.LT.ACURV*1.E-3
         THEN
            WRK(IP+19)=ACURV
            WRK(IP+20)=DIMP
            WRK(IP+21)=0.
         ELSE
            FDBPR=SIGNC/FDBPR
            SGPFI=(PAR2*CSROT+SNROT)/SQRT(CSI2GM)
            CGPFI=(CSROT-PAR2*SNROT)/SQRT(CSI2GM)
            COSALP=(CUROUT*(XHF*CSROT-PAR3*SNROT+XT)+SGPFI)*FDBPR
            SINALP=(CUROUT*(XHF*SNROT+PAR3*CSROT+YT)-CGPFI)*FDBPR
            WRK(IP+19)=ACURV
            WRK(IP+20)=DIMP
            WRK(IP+21)=ATAN2(SINALP,COSALP)
C
            IF LDTR.GE.55.AND.NHFIT.GE.10.AND.LNOCON.EQ.0
            THEN
               LCOVAR=1
            ELSE
               LCOVAR=0
            CIF
            IF LAND(MODXYV,32) .NE. 0
            THEN
               LJHTLU=1
            ELSE
               LJHTLU=0
            CIF
C           CALCULATE COVARIANCE MATRIX AND/OR UPDATE JHTL BANK
            PERFORM COVAR
C
            IWRK(IPRES+10)=1
C
         CIF
C
N     PUT RESULT INTO PATR-BANK
         CALL MVCL(IDATA(IPTR+1),0,IWRK(HPTR0),0,4*LDTR)
C
C
      CPROC
C-----------------------------------------------------------------------
      PROC SHFROT
C  FIT SHIFT AND ROTATION ONLY (CURVATURE KEPT FIXED)
         S0 = 0.
         S1 = 0.
         S2 = 0.
         S3 = 0.
         S4 = 0.
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
            CFOR
            IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).EQ.1
                  THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR=XA-XHF
                     IF XAR.GT.XMIN .AND. XAR.LT.XMAX
                     THEN
                        YA = WRK(IPCO+4)
                        IF LFTYP.EQ.2
                        THEN
                           DYRESA=YA-(PAR1*XAR+PAR2)*XAR-PAR3
                        ELSE
                           AAH=-XAR**2*CSI2GM
                           FDBPR=1./(1.+PAR1*XAR*PAR2)
                           SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
                           DYRESA=YA-SAG-PAR2*XAR-PAR3
                        CIF
                        DF0=ABS(DYRESA)
                     ELSE
                        DF0=15000.
                     CIF
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        XR=XAR
                        DYRES=DYRESA
                        JHUSE=JH
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.DISCUT
               THEN
                  S0=S0+1.
                  S1=S1+XR
                  S2=S2+XR**2
                  S3=S3+DYRES
                  S4=S4+DYRES*XR
                  ISORT4(KFLIP,JHUSE)=1
               CIF
            CIF
         CFOR
         NHFIT=S0+.1
         IF NHFIT.GE.4
         THEN
            S12=S1/S2
            S42=S4/S2
            DA=(S3-S1*S42)/(S0-S1*S12)
            DB=S42-S12*DA
            PAR3=PAR3+DA
            PAR2=PAR2+DB
            CSI2GM=1.+PAR2**2
            IF LFTYP.EQ.2
            THEN
               CUROUT =-PAR1 * 2./ (SQRT(CSI2GM)*CSI2GM)
            ELSE
               CUROUT=PAR1/SQRT(CSI2GM)
               XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
               XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
            CIF
         CIF
      CPROC
C=======================================================================
      PROC STVCIR
C  TRY TO FIND STARTING VALUES FOR CIRCLE FIT
C  THIS PART IS EXECUTED FOR ONLY A VERY SMALL FRACTION OF THE TRACKS
C  JUST LOOP UNTIL 10, NO STOP CONDITION CHECKED
      ISTV1=0
      DISCUT=400.
      WHILE ISTV1.LT.10
         ISTV1=ISTV1+1
C  FIT PARABOLA P1*X**2+P2*X+P3 TO RESIDUALS & MODIFY CIRCLE PARAMETERS
         S0 = 0.
         S1 = 0.
         S2 = 0.
         S3 = 0.
         S4 = 0.
         S8 = 0.
         S7 = 0.
         S6 = 0.
         S5 = 0.
         KFLIP=3-KFLIP
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
            CFOR
            IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).EQ.1
                  THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR= WRK(IPCO+3)-XHF
                     IF XAR.GT.XMIN.AND.XAR.LT.XMAX
                     THEN
                        YA = WRK(IPCO+4)
                        AAH=-XAR**2
                        FDBPR=1./(1.+PAR1*XAR*PAR2)
                        SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
                        DYRESA=YA-SAG-PAR2*XAR-PAR3
                        DF0=ABS(DYRESA)
                     ELSE
                        DF0=15000.
                     CIF
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        X=XAR
                        DYRES=DYRESA
                        JHUSE=JH
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.DISCUT
               THEN
                  S0=S0+1.
                  S1=S1+X
                  S2=S2+X**2
                  S3=S3+X**3
                  S4=S4+X**4
                  S8=S8+X**2
                  S7=S7+DYRES
                  S6=S6+DYRES*X
                  S5=S5+DYRES*X**2
                  ISORT4(KFLIP,JHUSE)=1
               CIF
            CIF
         CFOR
         NHFIT=S0+.1
C
C
         IF(NHFIT.LT.5) RETURN
C
C
N        SOLVE EQUATIONS
         F1 = 1. / S4
         XX12 = S3*F1
         XX13 = S2*F1
         YY1  = S5*F1
         XX22 = S8 - S3*XX12
         XX23 = S1 - S3*XX13
         YY2  = S6 - S3*YY1
         XX32 = S1 - S2*XX12
         XX33 = S0 - S2*XX13
         YY3  = S7 - S2*YY1
         IF XX22.GT.XX32
         THEN
            XX23 = XX23 / XX22
            YY2  = YY2  / XX22
            PARR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
            PARR2 = YY2 - XX23*PARR3
         ELSE
            XX33 = XX33 / XX32
            YY3  = YY3  / XX32
            PARR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
            PARR2 = YY3 - XX33*PARR3
         CIF
         PARR1 = YY1 - XX12*PARR2 - XX13*PARR3
C
         XAR=-.7*RRMI
         IF(XAR.LT..8*XMIN) XAR=.8*XMIN
         XBR= .7*RRPL
         IF(XBR.GT..8*XMAX) XBR=.8*XMAX
         IF -XAR.LT.XBR
         THEN
            XBR=-XAR
         ELSE
            XAR=-XBR
         CIF
C
         AAH=-XAR**2*CSI2GM
         FDBPR=1./(1.+PAR1*XAR*PAR2)
         SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
         YA=SAG+PAR2*XAR+PAR3 + (PARR1*XAR+PARR2)*XAR+PARR3
         AAH=-XBR**2*CSI2GM
         FDBPR=1./(1.+PAR1*XBR*PAR2)
         SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
         YB=SAG+PAR2*XBR+PAR3 + (PARR1*XBR+PARR2)*XBR+PARR3
         YC=PAR3+PARR3
         P2=(YB-YA)/(2.*XBR)
         C2=1.+P2**2
         SAG=YC-.5*(YA+YB)
         IF SAG**2.GT.(CKAPP*XBR)**2*C2
         THEN
            SAG=SIGN(CKAPP*XBR*SQRT(C2),SAG)
            YC=.5*(YA+YB)+SAG
         CIF
         PAR3=YC
         P1=2.*SAG/(C2*XBR**2-SAG**2)
         CUROUT=P1/SQRT(C2*(1.+(P1*XBR)**2))
         IF(ABS(CUROUT).LT.1.E-8) CUROUT= SIGN(1.E-8,CUROUT)
         PAR2=P2/(1.+SAG*P1)
         CSI2GM=1.+PAR2**2
         PAR1=CUROUT*SQRT(CSI2GM)
         XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
         XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
C
         DISCUT=.5*DISCUT
         IF(DISCUT.LT.10.) DISCUT=10.
      CWHILE
      CPROC
C-----------------------------------------------------------------------
      PROC COVAR
C
C      CALCULATE COVARIANCE MATRIX AND/OR UPDATE JHTL BANK
C
         SAMFI=SINALP*CSROT-COSALP*SNROT
         CAMFI=COSALP*CSROT+SINALP*SNROT
C
C        UPDATE OF JHTL FOR HITS   N O T   USED IN THE FIT
         IP00=2*IDATA(IQJETC)+100
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               JH=IH+JNH-1
               IF ISORT4(KFLIP,JH).NE.1
               THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                  X=WRK(IPCO+3)
                  Y=WRK(IPCO+4)
                  A=(X-XOR)*CAMFI+(Y-YOR)*SAMFI
                  B=(X-XOR)*SAMFI-(Y-YOR)*CAMFI
                  FDBPR=1./ACURV+DIMP-A
                  IF FDBPR.LT.100.
                  THEN
                     SAG=SQRT(FDBPR**2+B**2)
                     RESA=SAG-1./ACURV
                  ELSE
                     FDBPR=1./(1.+ACURV*(DIMP-A))
                     SAG=SAGCIR(FDBPR,ACURV,B**2,SP,1.E-4)
                     RESA=DIMP-A+SAG
                  CIF
C
C                 IWRK(IPCO+10)=1
                  WRK(IPCO+13)=RESA
                  IF LJHTLU.EQ.1
                  THEN
                     IP1   =IWRK(IPCO+1)
                     LBSIDE=IWRK(IPCO+2)
                     IPHL=IPJHTL+2+(IP1-IP00)/4
                     LB=IDATA(IPHL)
                     IDST=ABS(RESA)*5.
                     IF(IDST.GT.31) IDST=31
                     IDST=SHFTL(IDST,11)
                     IDST=LOR(IDST,1024)
                     IF(LBSIDE.EQ.1) IDST=LOR(IDST,256)
                     ITR1=LAND(SHFTR(LB,17),127)
                     IF ITR1.EQ.ITRK
                     THEN
                        IDATA(IPHL)=LOR(LAND(LB,MASK1),SHFTL(IDST,16))
                     ELSE
                        IDATA(IPHL)=LOR(LAND(LB,MASK2),      IDST    )
                     CIF
                  CIF
               CIF
            CFOR
         CFOR
C
         CHISQ=0.
C
N      VERTEX OMITTED
         NHF1=0
         S0D= 0.D0
         S1D= 0.D0
         S2D= 0.D0
         S3D= 0.D0
         S4D= 0.D0
         S8D= 0.D0
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               JH=IH+JNH-1
               IF ISORT4(KFLIP,JH).EQ.1
               THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                  X=WRK(IPCO+3)
                  Y=WRK(IPCO+4)
                  A=(X-XOR)*CAMFI+(Y-YOR)*SAMFI
                  B=(X-XOR)*SAMFI-(Y-YOR)*CAMFI
                  FDBPR=1./ACURV+DIMP-A
                  IF FDBPR.LT.100.
                  THEN
                     SAG=SQRT(FDBPR**2+B**2)
                     RESA=SAG-1./ACURV
                     DRDP2=FDBPR/SAG
                     DRDP1=B*(DRDP2+A/SAG)
                     DRDP3=1./ACURV**2*(1.-DRDP2)
                  ELSE
                     FDBPR=1./(1.+ACURV*(DIMP-A))
                     SAG=SAGCIR(FDBPR,ACURV,B**2,SP,1.E-4)
                     DRDP2=1./(1.+SAG*FDBPR*ACURV)
                     DRDP1=DRDP2*B*(1.+A*FDBPR*ACURV)
                     DRDP3=DRDP2*FDBPR*SP
                     RESA=DIMP-A+SAG
                  CIF
C
                  NHF1=NHF1+1
                  CHISQ=CHISQ+RESA**2
                  S0D=S0D+DRDP3**2
                  S1D=S1D+DRDP3*DRDP2
                  S2D=S2D+DRDP1*DRDP3
                  S3D=S3D+DRDP1*DRDP2
                  S4D=S4D+DRDP1**2
                  S8D=S8D+DRDP2**2
C
                  IWRK(IPCO+10)=0
                  WRK(IPCO+13)=RESA
                  IF LJHTLU.EQ.1
                  THEN
                     IP1   =IWRK(IPCO+1)
                     LBSIDE=IWRK(IPCO+2)
                     IPHL=IPJHTL+2+(IP1-IP00)/4
                     LB=IDATA(IPHL)
                     IDST=ABS(RESA)*5.
                     IF(IDST.GT.31) IDST=31
                     IDST=SHFTL(IDST,11)
                     IF(LBSIDE.EQ.1) IDST=LOR(IDST,256)
                     ITR1=LAND(SHFTR(LB,17),127)
                     IF ITR1.EQ.ITRK
                     THEN
                        IDATA(IPHL)=LOR(LAND(LB,MASK1),SHFTL(IDST,16))
                     ELSE
                        IDATA(IPHL)=LOR(LAND(LB,MASK2),      IDST    )
                     CIF
                  CIF
               CIF
            CFOR
         CFOR
         IF LCOVAR.EQ.1
         THEN
            IF NHF1.LT.10.OR.NHF1.NE.NHFIT
            THEN
               PRINT 6781,NRUN,NEV,ITRK,NHFIT,NHF1
6781           FORMAT(' RUN,EV,TRK,NHFIT,NHF1',I7,I6,I3,2I5)
            ELSE
               DETD=(S8D*S0D-S1D*S1D)*S4D+
     +        (S2D*S1D-S3D*S0D)*S3D+(S3D*S1D-S2D*S8D)*S2D
               FACT=CHISQ/(NHFIT-3)/DETD
C
               IWRK(IP+2) =LOR(IWRK(IP+2),2048)
               WRK(IP+49)=CHISQ/.115**2
               WRK(IP+50)=(S8D*S0D-S1D**2)*FACT
               WRK(IP+51)=(S1D*S2D-S0D*S3D)*FACT
               WRK(IP+52)=(S4D*S0D-S2D**2)*FACT
               WRK(IP+53)=(S3D*S1D-S8D*S2D)*FACT
               WRK(IP+54)=(S2D*S3D-S1D*S4D)*FACT
               WRK(IP+55)=(S8D*S4D-S3D**2)*FACT
            CIF
         CIF
      CPROC
      END
      SUBROUTINE XYRFT1(IPTR,IPJHTL,ERRFAC,LDTR)
C
C        REFIT TRACK ITRK IN 'PATR'-BANK (WITH A VERTEX
C        CONSTRAINT OF STRENGTH 1/ERRFAC IF ERRFAC<200.
C        VERTEX OMITTED IF INCOMPATIBLE.)
C        PARABOLA FIT IF |OLD CURVATURE * HALF TRACK LENGTH| < .04
C        CIRCLE FIT OTHERWISE
C
C    TEST VERSION 3.     (TESTED TO SOME EXTENT)
C
C                                J. SPITZER  25/3/87
C
C    EXTENDED TO GIVE COVARIANCE MATRIX FOR FIT PARAMETERS
C    CIRCLE PARAMETERS ARE SET EVEN IF PARABOLA FIT WAS PERFORMED
C                                      J.S.  2/4/87
C
C    DOUBLE PRECISION TO CALCULATE DETERMINANT FOR COVARIANCE MATRIX
C                                      J.S.  5/6/87
C
C    MODIFIED TO UPDATE JHTL BANK UPON REQUEST (MADE IN MODXYV)
C    + SMALL MODIFICATIONS IN THE WORK COMMON FOR THE
C      VERTEX CHAMBER GROUP
C                                      J.S.  5/1/88
C
      IMPLICIT INTEGER*2 (H)
      REAL*8   S0D,S1D,S2D,S3D,S4D,S8D,DETD
C
      COMMON/XYFVT1/MODXYV
C
C
#include "cdata.for"
C
#include "cgeo1.for"
C
#include "calibr.for"
C
#include "cworkpr.for"
#include "cworkeq.for"
C
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
C
#include "cpatlm.for"
C
#include "cjdrch.for"
#include "cdsmax.for"
      INTEGER DATE(5), IDAY /0/
C-----------------------------
      INTEGER NCHECK(5)/5*8/
      REAL RCHECK(12,2,5)/
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1.,
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1.,
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1.,
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1.,
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1./
C
C
      REAL RESCUT/8./,CKAPP/.966/
      DIMENSION ISORT1(71),ISORT2(3,71),IRESHT(71),ISORT3(91)
     +,ISORT4(2,91)
      DATA KPRT1/0/,NPRT1/50/,IQJETC/0/,IQHEAD/0/
      DATA MASK1/Z2FFFFFF/,MASK2/ZFFFF02FF/,MASK3/ZFFFFF1FF/
C
C
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
         LBINIT = 1
C
         IQJETC = IBLN('JETC')
         IQHEAD = IBLN('HEAD')
C
         CALL DAY2(DATE)
         IDAY = DATE(1)*1000 + DATE(2)
C
N       MULT. SCATTERING CONSTANTS
         RESMS = .020**2/2. * .16 * (1. + ALOG10(.16) /9.) * 155.45**2
C
         WRITE(6,137)
 137     FORMAT(/,' *** XYRFT1 ***  A NEW R-PHI FITTING ROUTINE',/,
     +            '                 TEST VERSION 3. (J. SPITZER)',/
     +     ' BIT 512 OR 1024 IS SET IN THE PROGRAM IDENTIFIER WORD',/,
     +' IN CASE VERTEX CONSTRAINT WAS NOT OR WAS USED RESP.',/,
     +' COVARIANCE MATRIX IS PROVIDED WITH BIT 2048 SWITCHED ON',/,
     +' IF THE PATR BANK ALREADY HAS THE LARGER LENGTH, THE NUMBER',/,
     +' OF HITS USED IN THE FIT IS AT LEAST 10 AND THE FIT CONVERGED.',
     +/)
      CIF
C
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      NEV  = HDATA(IPHEAD+11)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
C
N     RESERVE SPACE IN CWORK
      HPFREE = 1
C
C=======================================================================
N     GET X-Y-VERTEX AND STRENGTH OF VC
      IPV    = ICALIB(10)
      XO     = ACALIB(IPV+ 1)
      YO     = ACALIB(IPV+ 3)
      CURVXY=ADATA(IPTR+25)
      IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
      DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +ADATA(IPTR+9),1./CURVXY,XO,YO,XP,YP,FI)
      FV     = ERRFAC
      IF(FV .LT. .50) FV = .50
      IF NRUN.LT.24200
      THEN
         SRESO=.160
      ELSE
         SRESO=.100
      CIF
      SIGMIN=(SRESO/1.6)**2
      PTRANS = ABS(0.0299792458*BKGAUS/CURVXY) * .001
      RESV=.0100+.25*SIN(FI)**2+RESMS/PTRANS**2
      WGHT0  = (SRESO/FV)**2 / RESV
      INDBIT=512
C=======================================================================
N     HALF DISTANCE BETWEEN FIRST AND LAST POINTS ON TRACK
      XX    =  ADATA(IPTR+12) - ADATA(IPTR+5)
      YY    =  ADATA(IPTR+13) - ADATA(IPTR+6)
      RR    = 0.5*SQRT(XX**2+YY**2)
C
      IF RR.LT.10.
      THEN
         IF KPRT1.LT.NPRT1
         THEN
            KPRT1=KPRT1+1
            WRITE(6,848) NRUN,NEV,ITRK,RR
848         FORMAT(' ******** RUN,EV,TRACK',I8,I6,I3,/,
     +      ' HALF DISTANCE OF FIRST AND LAST POINTS',E14.3,
     +      ', XYRFT1 DOES NOT ATTEMPT R-PHI FIT')
         CIF
         RETURN
      CIF
C
C
N     FETCH HITS, CALCULATE COORDINATES, AND
N     FILL ARRAY IN /CWORK/
      HPCO0  = HPFREE
      LHIT   = 14
C     ORIGIN HALFWAY BETWEEN FIRST AND LAST POINTS ON TRACK
      INDFET = 2
      CALL JFETCH(IPTR,IPJHTL,WRK(HPCO0),LHIT,IPRES,INDFET,XO,YO)
C
C
      HLDCO  = LHIT
      HPCO9  = IPRES - 1
      HPAR0  = IPRES
      HLDPA  = 20
      HPAR9  = HPAR0 + HLDPA - 1
      HPFREE = HPAR9 + 1
      XT     = WRK (IPRES   )
      YT     = WRK (IPRES+ 1)
      CSROT  = WRK (IPRES+ 2)
      SNROT  = WRK (IPRES+ 3)
N     JADE ORIGIN IN THE FIT SYSTEM
      XOR    =- XT*CSROT -  YT*SNROT
      YOR    =  XT*SNROT -  YT*CSROT
N     VERTEX IN THE FIT SYSTEM
      X0     = (XO-XT)*CSROT+(YO-YT)*SNROT
      Y0     =-(XO-XT)*SNROT+(YO-YT)*CSROT
C
N     ZVERT, THETA
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTH   = WRK (IPRES+11)
      SNTH   = WRK (IPRES+12)
C
      IWRK(IPRES+10)=0
C
C ORIGINAL CHI2 AND CURVATURE ERROR IN PATR BANK
      SIG=ADATA(IPTR+23)**2
      IF(SIG.LT.1.E-5) SIG=1.E-5
      SIG11=(.5*ADATA(IPTR+26))**2/SIG
C
C-----------------------------------------------------------------------
C
C     GYMNASTICS FOR PRIVATE HIT QUALIFICATION
C     AND FOR HANDLING MORE HITS ON SAME WIRE
C
C
C TRY TO RECOVER HITS POSSIBLY LOST BY EARLIER FIT
      XHCUT=RR+200.
C
      XREGA= 100000.
      XREGB=-100000.
      NHALL=0
      NHWIR=0
      NHPOT=0
      IPCO=HPCO0
      REPEAT
         NHWIR=NHWIR+1
         IF(NHWIR.GT.70) RETURN
         ISORT1(NHWIR)=NHWIR
         ISORT2(1,NHWIR)=IPCO
         ISORT2(3,NHWIR)=0
         IW0=IWRK(IPCO)
         ICL0=IWRK(IPCO+9)
         LFL=0
         WHILE IPCO.LE.HPCO9
            IW9=IWRK(IPCO)
            ICL9=IWRK(IPCO+9)
            IF IW9.EQ.IW0 .AND. ICL9.EQ.ICL0
            THEN
N        HIT ON THE SAME WIRE
               NHALL=NHALL+1
               IF(NHALL.GT.90) RETURN
               IF(ISORT2(3,NHWIR).EQ.0) ISORT2(2,NHWIR)=NHALL
               ISORT2(3,NHWIR)=ISORT2(3,NHWIR)+1
C
               XA=WRK(IPCO+3)
               LBGOOD=IWRK(IPCO+10)
               IF LBGOOD.LE.2
               THEN
                  IF ABS(XA).GT.XHCUT
                  THEN
                     ISORT3(NHALL)=-1
                  ELSE
                     ISORT3(NHALL)= 1
                     LFL=1
                     IF(XA.LT.XREGA) XREGA=XA
                     IF(XA.GT.XREGB) XREGB=XA
                  CIF
C
                  IWRK(IPCO+10)=1
               ELSE
                  ISORT3(NHALL)=-1
               CIF
C
               IPCO=IPCO+HLDCO
            ELSE
               XWHILE
            CIF
         CWHILE
         IF LFL.EQ.1
         THEN
            NHPOT=NHPOT+1
         ELSE
            ISORT3(ISORT2(2,NHWIR))=-2
         CIF
      UNTIL IPCO.GT.HPCO9
C-----------------------------------------------------------------------
C
C IF LESS THAN 4 HITS SURVIVE NOTHING WILL BE DONE
      IF(NHPOT.LT.4) RETURN
      XHF=.5*(XREGA+XREGB)
      RRPL=.5*(XREGB-XREGA)
      RRMI=RRPL
      IF ABS(XHF).GT.RR
      THEN
         RRPL=RRPL+XHF-SIGN(RR,XHF)
         RRMI=RRMI-XHF+SIGN(RR,XHF)
         XHF=SIGN(RR,XHF)
      CIF
C-----------------------------------------------------------------------
C  STARTING VALUES OF FIT PARAMETERS
C
      LCHC=0
C  CHANGE START VALUE IF CURVATURE INCONSISTENT WITH FIRST & LAST POINTS
      IF ABS(CURVXY*RR).GT.CKAPP
      THEN
         LCHC=1
         CURVXY=SIGN(CKAPP/RR,CURVXY)
      CIF
      IF ABS(CURVXY)*RR .GT. .04
      THEN
C      CIRCLE FIT
         LFTYP=1
         P1=CURVXY/SQRT(1.-(CURVXY*RR)**2)
         AAH=(RR-XHF)*(RR+XHF)
         PAR3=SAGCIR(1.,P1,AAH,SAGPR,1.E-4)
         PAR2=-P1/(1.+PAR3*P1)*XHF
         CSI2GM=1.+PAR2**2
         PAR1=CURVXY*SQRT(CSI2GM)
      ELSE
C      PARABOLA FIT
         LFTYP=2
         PAR3=.5*CURVXY*(RR-XHF)*(RR+XHF)
         PAR2=-CURVXY*XHF
         CSI2GM=1.+PAR2**2
         PAR1=-0.5*CURVXY*SQRT(CSI2GM)*CSI2GM
      CIF
      XMIN=PAR2/PAR1-CKAPP/ABS(CURVXY)
      XMAX=PAR2/PAR1+CKAPP/ABS(CURVXY)
C
C-----------------------------------------------------------------------
C  FIT SHIFT AND ROTATION ONLY (CURVATURE KEPT FIXED)
      DISCUT=400.
      KFLIP=1
      PERFORM SHFROT
      IF(NHFIT.LT.4) RETURN
      IF NHFIT.LT.6
      THEN
C FIT OF SHIFT AND ROTATION HAS ONLY BEEN PERFORMED
C CHI2 AND CURVATURE ERROR REMAIN THE OLD VALUES IN THE PATR BANK
         PERFORM FITBNK
         RETURN
      CIF
C
C-----------------------------------------------------------------------
      IF ABS(DA).GT.1.5 .OR. ABS(DB).GT..1 .OR. LCHC.EQ.1
      THEN
C SHIFT TO ORIGINAL FIT TOO BIG OR CHANGE OF CURVATURE FOR START
C TRY TO FIND CORRECT STARTING VALUES, TAKE CIRCLE IN ANY CASE
         IF LFTYP.EQ.2
         THEN
            LFTYP=1
            PAR1=CUROUT*SQRT(CSI2GM)
            XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
            XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
         CIF
         PERFORM STVCIR
      CIF
C
C
C IF TOO MANY HITS THROWN AWAY EVEN WITH THE VERY LOOSE RESIDUAL CUTS
C DO NOT DARE TO ACCEPT/START THE FIT
C
      IF(NHFIT.LT.NHPOT/2) RETURN
C
C
N      SAVE CURRENT (START) VALUES
      NHFTLS=NHFIT
      PAR1LS=PAR1
      PAR2LS=PAR2
      PAR3LS=PAR3
      CSI2LS=CSI2GM
      CURLST=CUROUT
      XMAXLS=XMAX
      XMINLS=XMIN
      SIG11L=SIG11
      SIGLST=SIG
C
C
N      VERTEX
      NHWIRV=NHWIR+1
      ISORT1(NHWIRV)=NHWIRV
      ISORT2(1,NHWIRV)=-200
C
C
C=======================================================================
C
N     FIRST ITERATE WITHOUT VERTEX CONSTRAINT
      INDMAX=NHFIT/8+1
      IF(INDMAX.GT.8) INDMAX=8
      INDFIT=0
      WHILE INDFIT.LT.INDMAX
         INDFIT=INDFIT+1
N    PARABOLA OR CIRCLE FIT
         PERFORM FTCURV
         IF LNOCON.EQ.1
         THEN
C NO CONVERGENCE AS INDICATED BY LOSS OF TOO MANY HITS
C STILL RETAIN THE EARLIER FIT
C IF IT IS THE ONE OBTAINED IN PROC STVCIR,
C CHI2 AND CURVATURE ERROR REMAIN THE OLD VALUES IN THE PATR BANK
            NHFIT=NHFTLS
            PAR1=PAR1LS
            PAR2=PAR2LS
            PAR3=PAR3LS
            CSI2GM=CSI2LS
            CUROUT=CURLST
            XMAX=XMAXLS
            XMIN=XMINLS
            SIG11=SIG11L
            SIG=SIGLST
C
C DO NOT ATTEMPT VERTEX CONSTRAINT
            PERFORM FITBNK
            RETURN
         CIF
         IF(SIG.LT.SIGMIN) XWHILE
         IF INDFIT.GE.2
         THEN
            PERFORM LLSTOP
            IF LSTOP.EQ.1
            THEN
C      PREVIOUS FIT ACCEPTED, RESTORE ITS RESULTS
               INDFIT=INDFIT-1
               NHFIT=NHFTLS
               PAR1=PAR1LS
               PAR2=PAR2LS
               PAR3=PAR3LS
               CSI2GM=CSI2LS
               CUROUT=CURLST
               XMAX=XMAXLS
               XMIN=XMINLS
               SIG11=SIG11L
               SIG=SIGLST
               KFLIP=3-KFLIP
               XWHILE
            CIF
         CIF
         IF(INDFIT.EQ.INDMAX) XWHILE
N      SAVE FIT RESULTS
         NHFTLS=NHFIT
         PAR1LS=PAR1
         PAR2LS=PAR2
         PAR3LS=PAR3
         CSI2LS=CSI2GM
         CURLST=CUROUT
         XMAXLS=XMAX
         XMINLS=XMIN
         SIG11L=SIG11
         SIGLST=SIG
N      HIT CLEANING
         PERFORM HITCLN
      CWHILE
C
C            ======  VERTEX CONSTRAINT  =======
      X0R=X0-XHF
      IF FV.LT.200. .AND. X0R.GT.XMIN .AND. X0R.LT.XMAX
      THEN
C        VERTEX CONSTRAINT (WEEK OR STRONG) HAS BEEN REQUESTED
C        ROUGH CHECK IF RUN VERTEX CONSISTENT WITH THE TRACK
         IF LFTYP.EQ.2
         THEN
            DVCHI2=((PAR1*X0R+PAR2)*X0R+PAR3-Y0)**2*WGHT0
         ELSE
            AAH=-X0R**2*CSI2GM
            FDBPR=1./(1.+PAR1*X0R*PAR2)
            SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
            DVCHI2=(SAG+PAR2*X0R+PAR3-Y0)**2*WGHT0
         CIF
         IF DVCHI2 .LT.  9.*SIG
         THEN
            ISORT2(1,NHWIRV)=-100
            PERFORM FTCURV
            IF(LNOCON.EQ.0) INDBIT=1024
         CIF
      CIF
C
N     SET UP PATR-BANK
      PERFORM FITBNK
      RETURN
C=======================================================================
C
N     *************************
N     *      F T C U R V      *
N     *************************
C
C
N      PARABOLA OR CIRCLE FIT
      PROC FTCURV
C
      LNOCON=0
N     GET EQUATIONS
N     WEIGHT VERTEX AS POINT OF PARABOLA
      KFLIP=3-KFLIP
      KITER=0
      WHILE KITER .LT. 3-LFTYP
         KITER=KITER+1
         X0R=X0-XHF
         IF ISORT2(1,NHWIRV).EQ.-100 .AND. X0R.GT.XMIN .AND. X0R.LT.XMAX
         THEN
N      VERTEX INCLUDED
            IF LFTYP.EQ.2
            THEN
               DYDP1=X0R**2
               DYDP2=X0R
               DYRES=Y0
            ELSE
               AAH=-X0R**2
               FDBPR=1./(1.+PAR1*X0R*PAR2)
               SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
               CC1=FDBPR/(1.+SAG*PAR1*FDBPR)
               DYDP1=CC1*SAGPR
               DYDP2=X0R+PAR1*CC1*(AAH*PAR2-SAG*X0R)
               DYRES=Y0-SAG-PAR2*X0R-PAR3
            CIF
            S0 = WGHT0
            S1=DYDP2*WGHT0
            S2=DYDP1*WGHT0
            S3=S2*DYDP2
            S4=S2*DYDP1
            S8=S1*DYDP2
            S7=DYRES*WGHT0
            S6=S7*DYDP2
            S5=S7*DYDP1
         ELSE
N      VERTEX OMITTED
            S0 = 0.
            S1 = 0.
            S2 = 0.
            S3 = 0.
            S4 = 0.
            S8 = 0.
            S7 = 0.
            S6 = 0.
            S5 = 0.
         CIF
         S00=S0
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
            CFOR
            IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).EQ.1
                  THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR=XA-XHF
                     IF XAR.GT.XMIN.AND.XAR.LT.XMAX
                     THEN
                        YA = WRK(IPCO+4)
                        IF LFTYP.EQ.2
                        THEN
                           DYDP1A=XAR**2
                           DYDP2A=XAR
                           DYRESA=YA
                           DF0=ABS(YA-((PAR1*XAR+PAR2)*XAR+PAR3))
                        ELSE
                           AAH=-XAR**2
                           FDBPR=1./(1.+PAR1*XAR*PAR2)
                           SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
                           CC1=FDBPR/(1.+SAG*PAR1*FDBPR)
                           DYDP1A=CC1*SAGPR
                           DYDP2A=XAR+PAR1*CC1*(AAH*PAR2-SAG*XAR)
                           DYRESA=YA-SAG-PAR2*XAR-PAR3
                           DF0=ABS(DYRESA)
                        CIF
                     ELSE
                        DF0=15000.
                     CIF
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        DYDP1=DYDP1A
                        DYDP2=DYDP2A
                        DYRES=DYRESA
                        JHUSE=JH
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.RESCUT
               THEN
                  S0=S0+1.
                  S1=S1+DYDP2
                  S2=S2+DYDP1
                  S3=S3+DYDP1*DYDP2
                  S4=S4+DYDP1**2
                  S8=S8+DYDP2**2
                  S7=S7+DYRES
                  S6=S6+DYRES*DYDP2
                  S5=S5+DYRES*DYDP1
                  ISORT4(KFLIP,JHUSE)=1
               ELSE
                  ISORT3(IH)=-2
               CIF
            CIF
         CFOR
         NHF1=S0-S00+.1
         IF NHF1.LT.6 .OR. NHF1.LT.NHPOT/2
         THEN
            LNOCON=1
            XWHILE
         CIF
         NHFIT=NHF1
         DEG   = S0 - S00 - 3.
N     CURVATURE ERROR
         DET = (S8*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S8)*S2
         SIG11 = (S8*S0 - S1*S1)/DET
C
N        SOLVE EQUATIONS
         F1 = 1. / S4
         XX12 = S3*F1
         XX13 = S2*F1
         YY1  = S5*F1
         XX22 = S8 - S3*XX12
         XX23 = S1 - S3*XX13
         YY2  = S6 - S3*YY1
         XX32 = S1 - S2*XX12
         XX33 = S0 - S2*XX13
         YY3  = S7 - S2*YY1
         IF XX22.GT.XX32
         THEN
            XX23 = XX23 / XX22
            YY2  = YY2  / XX22
            PARR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
            PARR2 = YY2 - XX23*PARR3
         ELSE
            XX33 = XX33 / XX32
            YY3  = YY3  / XX32
            PARR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
            PARR2 = YY3 - XX33*PARR3
         CIF
         PARR1 = YY1 - XX12*PARR2 - XX13*PARR3
C
         IF LFTYP.EQ.2
         THEN
            PAR1=PARR1
            PAR2=PARR2
            PAR3=PARR3
            IF(ABS(PAR1).LT.1.E-10) PAR1 = SIGN(1.E-10,PAR1)
            CSI2GM=PAR2**2+1.
            CUROUT =-PAR1 * 2./ (SQRT(CSI2GM)*CSI2GM)
         ELSE
            PAR1=PAR1+PARR1
            PAR2=PAR2+PARR2
            PAR3=PAR3+PARR3
            IF(ABS(PAR1).LT.1.E-10) PAR1 = SIGN(1.E-10,PAR1)
            CSI2GM=PAR2**2+1.
            CUROUT=PAR1/SQRT(CSI2GM)
            XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
            XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
         CIF
      CWHILE
C  END ITERATION DONE IN CASE OF CIRCLE FIT ONLY
C
C
      IF LNOCON.EQ.0
      THEN
N     CALC. CHISQ + SOLVE L/R AMBIGUITY
         CHISQ = 0.
         NHF1=0
         FOR IHWIR=1,NHWIR
            IRESHT(IHWIR)=-1
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            IF ISORT3(IH).GE.0 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).GE.0
                  THEN
                     IFLG=ISORT3(JH)
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XR= WRK(IPCO+3)-XHF
                     IF XR.GT.XMIN.AND.XR.LT.XMAX
                     THEN
                        Y = WRK(IPCO+4)
                        IF LFTYP.EQ.2
                        THEN
                           DF0=ABS(Y-(PAR1*XR+PAR2)*XR-PAR3)
                        ELSE
                           AAH=-XR**2*CSI2GM
                           FDBPR=1./(1.+PAR1*XR*PAR2)
                           SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
                           DF0=ABS(Y-SAG-PAR2*XR-PAR3)
                        CIF
                        IF(DF0.LT.RESMIN) RESMIN=DF0
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.5000.
               THEN
                  IRESHT(IHWIR)=RESMIN*1.E6
                  IF IFLG.EQ.1
                  THEN
                     CHISQ=CHISQ+RESMIN**2
                     NHF1=NHF1+1
                  CIF
               CIF
            CIF
         CFOR
         IF(NHF1.LT.NHFIT-3) LNOCON=1
         SIG    =      CHISQ  / DEG
      CIF
      CPROC
C=======================================================================
      PROC HITCLN
C      LABEL HITS NOT TO BE USED IN THE NEXT ITRATION
N       SQRT(CHI2) OF VERTEX
C  VERTEX NOT INCLUDED IN THE ITERATIVE PART
C  IN THE CURRENT VERSION
CV       X0R=X0-XHF
CV       IF LFTYP.EQ.2
CV       THEN
CV          DFVERT=((PAR1*X0R+PAR2)*X0R+PAR3-Y0)*SQRT(WGHT0)
CV       ELSE
CV          AAH=-X0R**2*CSI2GM
CV          FDBPR=1./(1.+PAR1*X0R*PAR2)
CV          SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
CV          DFVERT=(SAG+PAR2*X0R+PAR3-Y0)*SQRT(WGHT0)
CV       CIF
CV       IRESHT(NHWIRV)=ABS(DFVERT)*1.E6
C-------------------------------------------------------------
C
N       SORT HITS ACCORDING TO RESIDUALS
C  EXCLUDE THE INDFIT LARGEST RESIDUAL HITS,
C  RESTORE THE OTHERS (EXLUDED FOR EVER HITS NOT COUNTED)
C
CV       CALL SHELL9(IRESHT,ISORT1,NHWIRV)
         CALL SHELL9(IRESHT,ISORT1,NHWIR)
         KOMIT=0
CV       FOR J1=1,NHWIRV
         FOR J1=1,NHWIR
CV          IHWIR=ISORT1(NHWIRV+1-J1)
            IHWIR=ISORT1(NHWIR+1-J1)
            IPCO=ISORT2(1,IHWIR)
            IF IPCO.NE.-100 .AND. IPCO.NE.-200
            THEN
N     HIT, NOT VERTEX
               NNH=ISORT2(3,IHWIR)
               IH=ISORT2(2,IHWIR)
               LFLG=0
               FOR JNH=1,NNH
                  IHA=IH+JNH-1
                  IQA=ISORT3(IHA)
                  IF IQA.GT.-1
                  THEN
                     IF LFLG.EQ.0
                     THEN
                        LFLG=1
                        KOMIT=KOMIT+1
                     CIF
                     IF KOMIT.LE.INDFIT
                     THEN
                        ISORT3(IHA)=0
                     ELSE
                        ISORT3(IHA)=1
                     CIF
                  CIF
               CFOR
CV          ELSE
C   VERTEX;   DOES NOT OCCOUR IN THE CURRENT VERSION
CV             KOMIT=KOMIT+1
CV             IF(KOMIT.LE.INDFIT) WGHT0=WGHT0*.01
            CIF
         CFOR
      CPROC
C=======================================================================
      PROC LLSTOP
         IF INDFIT.LE.6
         THEN
            INDCK=INDFIT-1
         ELSE
            INDCK=5
         CIF
         ICHCK=NCHECK(INDCK)
         WHILE SIGLST.LT.RCHECK(ICHCK,1,INDCK)
            ICHCK=ICHCK-1
         CWHILE
         IF(ICHCK.LT.1) ICHCK=1
         IF SIG/SIGLST.GT.RCHECK(ICHCK,2,INDCK)
         THEN
            LSTOP=1
         ELSE
            LSTOP=0
         CIF
      CPROC
C=======================================================================
C
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
N     START + END POINTS
         XST=XHF+XMIN
         IF(XST.LT.XREGA) XST=XREGA
         XSTR=XST-XHF
         XEN=XHF+XMAX
         IF(XEN.GT.XREGB) XEN=XREGB
         XENR=XEN-XHF
         IF LFTYP.EQ.2
         THEN
            YST  = (PAR1 *XSTR+ PAR2 )*XSTR+ PAR3
            YEN  = (PAR1 *XENR+ PAR2 )*XENR+ PAR3
N     DIRECTION AT START + END POINT
            TGST = PAR1*XSTR*2.+ PAR2
            TGEN = PAR1*XENR*2.+ PAR2
         ELSE
            AAH=-XSTR**2
            FDBPR=1./(1.+PAR1*XSTR*PAR2)
            SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
            YST=SAG+PAR2*XSTR+PAR3
         TGST=PAR2-PAR1*FDBPR/(1.+SAG*PAR1*FDBPR)*(SAG*PAR2+XSTR*CSI2GM)
            AAH=-XENR**2
            FDBPR=1./(1.+PAR1*XENR*PAR2)
            SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
            YEN=SAG+PAR2*XENR+PAR3
         TGEN=PAR2-PAR1*FDBPR/(1.+SAG*PAR1*FDBPR)*(SAG*PAR2+XENR*CSI2GM)
         CIF
         DXST = 1./SQRT(TGST**2+1.)
         DYST = DXST * TGST
         DXEN = 1./SQRT(TGEN**2+1.)
         DYEN = DXEN * TGEN
C
C
C
N     COPY TRACK BANK
         HPTR0 = HPFREE
         CALL MVC(IWRK(HPTR0),0,IDATA(IPTR+1),0,4*LDTR)
C
N     FILL FIT-BANK
         IP    = HPTR0 - 1
         IWRK(IP+2) = LAND(IWRK(IP+2),MASK3)
         IWRK(IP+2) = LOR(IWRK(IP+2),INDBIT)
         IWRK(IP+ 3) = IDAY
         WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
         WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
         WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2)*TGTH+ZVERT
         DXSTJ       =  DXST*CSROT - DYST*SNROT
         DYSTJ       =  DXST*SNROT + DYST*CSROT
         WRK (IP+ 8) =  DXSTJ*CSTH
         WRK (IP+ 9) =  DYSTJ*CSTH
         WRK (IP+12) = XEN *CSROT - YEN *SNROT + XT
         WRK (IP+13) = XEN *SNROT + YEN *CSROT + YT
         WRK (IP+14) = SQRT(WRK(IP+12)**2 + WRK(IP+13)**2)*TGTH+ZVERT
         WRK (IP+15) = (DXEN*CSROT - DYEN*SNROT)*CSTH
         WRK (IP+16) = (DXEN*SNROT + DYEN*CSROT)*CSTH
         IWRK(IP+24) = NHFIT
         WRK (IP+25) = CUROUT
         WRK (IP+27) = CUROUT
         WRK (IP+28) = CUROUT
C
         WRK (IP+23) = SQRT(SIG)
         WRK (IP+26) = SQRT(SIG*SIG11/CSI2GM)
         IF(LFTYP.EQ.2) WRK(IP+26)=WRK(IP+26)*2./CSI2GM
C
C        IWRK(IP+18) = LFTYP
C EVEN IF PARABOLA FIT WAS DONE, CIRCLE PARAMETERS ARE STORED
         IWRK(IP+18) = 1
C
         PAR1=CUROUT*SQRT(CSI2GM)
         SIGNC=SIGN(1.,CUROUT)
         ACURV=ABS(CUROUT)
         A=((XHF-XOR)*PAR2-PAR3+YOR)/SQRT(CSI2GM)
         B=(XHF-XOR+(PAR3-YOR)*PAR2)/SQRT(CSI2GM)
         FDBPR=1./ACURV+SIGNC*A
         IF FDBPR.LT.100.
         THEN
            DIMP=-1./ACURV+SQRT(FDBPR**2+B**2)
         ELSE
            DIMP=SIGNC*A+SAGCIR(1./(1.+CUROUT*A),ACURV,B**2,SP,1.E-4)
         CIF
         FDBPR=1.+DIMP*ACURV
         IF FDBPR.LT.ACURV*1.E-3
         THEN
            WRK(IP+19)=ACURV
            WRK(IP+20)=DIMP
            WRK(IP+21)=0.
         ELSE
            FDBPR=SIGNC/FDBPR
            SGPFI=(PAR2*CSROT+SNROT)/SQRT(CSI2GM)
            CGPFI=(CSROT-PAR2*SNROT)/SQRT(CSI2GM)
            COSALP=(CUROUT*(XHF*CSROT-PAR3*SNROT+XT)+SGPFI)*FDBPR
            SINALP=(CUROUT*(XHF*SNROT+PAR3*CSROT+YT)-CGPFI)*FDBPR
            WRK(IP+19)=ACURV
            WRK(IP+20)=DIMP
            WRK(IP+21)=ATAN2(SINALP,COSALP)
C
            IF LDTR.GE.55.AND.NHFIT.GE.10.AND.LNOCON.EQ.0
            THEN
               LCOVAR=1
            ELSE
               LCOVAR=0
            CIF
            IF LAND(MODXYV,32) .NE. 0
            THEN
               LJHTLU=1
            ELSE
               LJHTLU=0
            CIF
C           CALCULATE COVARIANCE MATRIX AND/OR UPDATE JHTL BANK
            PERFORM COVAR
C
            IWRK(IPRES+10)=1
C
         CIF
C
N     PUT RESULT INTO PATR-BANK
         CALL MVC(IDATA(IPTR+1),0,IWRK(HPTR0),0,4*LDTR)
C
C
      CPROC
C-----------------------------------------------------------------------
      PROC SHFROT
C  FIT SHIFT AND ROTATION ONLY (CURVATURE KEPT FIXED)
         S0 = 0.
         S1 = 0.
         S2 = 0.
         S3 = 0.
         S4 = 0.
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
            CFOR
            IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).EQ.1
                  THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR=XA-XHF
                     IF XAR.GT.XMIN .AND. XAR.LT.XMAX
                     THEN
                        YA = WRK(IPCO+4)
                        IF LFTYP.EQ.2
                        THEN
                           DYRESA=YA-(PAR1*XAR+PAR2)*XAR-PAR3
                        ELSE
                           AAH=-XAR**2*CSI2GM
                           FDBPR=1./(1.+PAR1*XAR*PAR2)
                           SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
                           DYRESA=YA-SAG-PAR2*XAR-PAR3
                        CIF
                        DF0=ABS(DYRESA)
                     ELSE
                        DF0=15000.
                     CIF
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        XR=XAR
                        DYRES=DYRESA
                        JHUSE=JH
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.DISCUT
               THEN
                  S0=S0+1.
                  S1=S1+XR
                  S2=S2+XR**2
                  S3=S3+DYRES
                  S4=S4+DYRES*XR
                  ISORT4(KFLIP,JHUSE)=1
               CIF
            CIF
         CFOR
         NHFIT=S0+.1
         IF NHFIT.GE.4
         THEN
            S12=S1/S2
            S42=S4/S2
            DA=(S3-S1*S42)/(S0-S1*S12)
            DB=S42-S12*DA
            PAR3=PAR3+DA
            PAR2=PAR2+DB
            CSI2GM=1.+PAR2**2
            IF LFTYP.EQ.2
            THEN
               CUROUT =-PAR1 * 2./ (SQRT(CSI2GM)*CSI2GM)
            ELSE
               CUROUT=PAR1/SQRT(CSI2GM)
               XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
               XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
            CIF
         CIF
      CPROC
C=======================================================================
      PROC STVCIR
C  TRY TO FIND STARTING VALUES FOR CIRCLE FIT
C  THIS PART IS EXECUTED FOR ONLY A VERY SMALL FRACTION OF THE TRACKS
C  JUST LOOP UNTIL 10, NO STOP CONDITION CHECKED
      ISTV1=0
      DISCUT=400.
      WHILE ISTV1.LT.10
         ISTV1=ISTV1+1
C  FIT PARABOLA P1*X**2+P2*X+P3 TO RESIDUALS & MODIFY CIRCLE PARAMETERS
         S0 = 0.
         S1 = 0.
         S2 = 0.
         S3 = 0.
         S4 = 0.
         S8 = 0.
         S7 = 0.
         S6 = 0.
         S5 = 0.
         KFLIP=3-KFLIP
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
            CFOR
            IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).EQ.1
                  THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR= WRK(IPCO+3)-XHF
                     IF XAR.GT.XMIN.AND.XAR.LT.XMAX
                     THEN
                        YA = WRK(IPCO+4)
                        AAH=-XAR**2
                        FDBPR=1./(1.+PAR1*XAR*PAR2)
                        SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
                        DYRESA=YA-SAG-PAR2*XAR-PAR3
                        DF0=ABS(DYRESA)
                     ELSE
                        DF0=15000.
                     CIF
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        X=XAR
                        DYRES=DYRESA
                        JHUSE=JH
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.DISCUT
               THEN
                  S0=S0+1.
                  S1=S1+X
                  S2=S2+X**2
                  S3=S3+X**3
                  S4=S4+X**4
                  S8=S8+X**2
                  S7=S7+DYRES
                  S6=S6+DYRES*X
                  S5=S5+DYRES*X**2
                  ISORT4(KFLIP,JHUSE)=1
               CIF
            CIF
         CFOR
         NHFIT=S0+.1
C
C
         IF(NHFIT.LT.5) RETURN
C
C
N        SOLVE EQUATIONS
         F1 = 1. / S4
         XX12 = S3*F1
         XX13 = S2*F1
         YY1  = S5*F1
         XX22 = S8 - S3*XX12
         XX23 = S1 - S3*XX13
         YY2  = S6 - S3*YY1
         XX32 = S1 - S2*XX12
         XX33 = S0 - S2*XX13
         YY3  = S7 - S2*YY1
         IF XX22.GT.XX32
         THEN
            XX23 = XX23 / XX22
            YY2  = YY2  / XX22
            PARR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
            PARR2 = YY2 - XX23*PARR3
         ELSE
            XX33 = XX33 / XX32
            YY3  = YY3  / XX32
            PARR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
            PARR2 = YY3 - XX33*PARR3
         CIF
         PARR1 = YY1 - XX12*PARR2 - XX13*PARR3
C
         XAR=-.7*RRMI
         IF(XAR.LT..8*XMIN) XAR=.8*XMIN
         XBR= .7*RRPL
         IF(XBR.GT..8*XMAX) XBR=.8*XMAX
         IF -XAR.LT.XBR
         THEN
            XBR=-XAR
         ELSE
            XAR=-XBR
         CIF
C
         AAH=-XAR**2*CSI2GM
         FDBPR=1./(1.+PAR1*XAR*PAR2)
         SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
         YA=SAG+PAR2*XAR+PAR3 + (PARR1*XAR+PARR2)*XAR+PARR3
         AAH=-XBR**2*CSI2GM
         FDBPR=1./(1.+PAR1*XBR*PAR2)
         SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
         YB=SAG+PAR2*XBR+PAR3 + (PARR1*XBR+PARR2)*XBR+PARR3
         YC=PAR3+PARR3
         P2=(YB-YA)/(2.*XBR)
         C2=1.+P2**2
         SAG=YC-.5*(YA+YB)
         IF SAG**2.GT.(CKAPP*XBR)**2*C2
         THEN
            SAG=SIGN(CKAPP*XBR*SQRT(C2),SAG)
            YC=.5*(YA+YB)+SAG
         CIF
         PAR3=YC
         P1=2.*SAG/(C2*XBR**2-SAG**2)
         CUROUT=P1/SQRT(C2*(1.+(P1*XBR)**2))
         IF(ABS(CUROUT).LT.1.E-8) CUROUT= SIGN(1.E-8,CUROUT)
         PAR2=P2/(1.+SAG*P1)
         CSI2GM=1.+PAR2**2
         PAR1=CUROUT*SQRT(CSI2GM)
         XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
         XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
C
         DISCUT=.5*DISCUT
         IF(DISCUT.LT.10.) DISCUT=10.
      CWHILE
      CPROC
C-----------------------------------------------------------------------
      PROC COVAR
C
C      CALCULATE COVARIANCE MATRIX AND/OR UPDATE JHTL BANK
C
         SAMFI=SINALP*CSROT-COSALP*SNROT
         CAMFI=COSALP*CSROT+SINALP*SNROT
C
C        UPDATE OF JHTL FOR HITS   N O T   USED IN THE FIT
         IP00=2*IDATA(IQJETC)+100
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               JH=IH+JNH-1
               IF ISORT4(KFLIP,JH).NE.1
               THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                  X=WRK(IPCO+3)
                  Y=WRK(IPCO+4)
                  A=(X-XOR)*CAMFI+(Y-YOR)*SAMFI
                  B=(X-XOR)*SAMFI-(Y-YOR)*CAMFI
                  FDBPR=1./ACURV+DIMP-A
                  IF FDBPR.LT.100.
                  THEN
                     SAG=SQRT(FDBPR**2+B**2)
                     RESA=SAG-1./ACURV
                  ELSE
                     FDBPR=1./(1.+ACURV*(DIMP-A))
                     SAG=SAGCIR(FDBPR,ACURV,B**2,SP,1.E-4)
                     RESA=DIMP-A+SAG
                  CIF
C
C                 IWRK(IPCO+10)=1
                  WRK(IPCO+13)=RESA
                  IF LJHTLU.EQ.1
                  THEN
                     IP    =IWRK(IPCO+1)
                     LBSIDE=IWRK(IPCO+2)
                     IPHL=IPJHTL+2+(IP-IP00)/4
                     LB=IDATA(IPHL)
                     IDST=ABS(RESA)*5.
                     IF(IDST.GT.31) IDST=31
                     IDST=SHFTL(IDST,11)
                     IDST=LOR(IDST,1024)
                     IF(LBSIDE.EQ.1) IDST=LOR(IDST,256)
                     ITR1=LAND(SHFTR(LB,17),127)
                     IF ITR1.EQ.ITRK
                     THEN
                        IDATA(IPHL)=LOR(LAND(LB,MASK1),SHFTL(IDST,16))
                     ELSE
                        IDATA(IPHL)=LOR(LAND(LB,MASK2),      IDST    )
                     CIF
                  CIF
               CIF
            CFOR
         CFOR
C
         CHISQ=0.
C
N      VERTEX OMITTED
         NHF1=0
         S0D= 0.D0
         S1D= 0.D0
         S2D= 0.D0
         S3D= 0.D0
         S4D= 0.D0
         S8D= 0.D0
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               JH=IH+JNH-1
               IF ISORT4(KFLIP,JH).EQ.1
               THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                  X=WRK(IPCO+3)
                  Y=WRK(IPCO+4)
                  A=(X-XOR)*CAMFI+(Y-YOR)*SAMFI
                  B=(X-XOR)*SAMFI-(Y-YOR)*CAMFI
                  FDBPR=1./ACURV+DIMP-A
                  IF FDBPR.LT.100.
                  THEN
                     SAG=SQRT(FDBPR**2+B**2)
                     RESA=SAG-1./ACURV
                     DRDP2=FDBPR/SAG
                     DRDP1=B*(DRDP2+A/SAG)
                     DRDP3=1./ACURV**2*(1.-DRDP2)
                  ELSE
                     FDBPR=1./(1.+ACURV*(DIMP-A))
                     SAG=SAGCIR(FDBPR,ACURV,B**2,SP,1.E-4)
                     DRDP2=1./(1.+SAG*FDBPR*ACURV)
                     DRDP1=DRDP2*B*(1.+A*FDBPR*ACURV)
                     DRDP3=DRDP2*FDBPR*SP
                     RESA=DIMP-A+SAG
                  CIF
C
                  NHF1=NHF1+1
                  CHISQ=CHISQ+RESA**2
                  S0D=S0D+DRDP3**2
                  S1D=S1D+DRDP3*DRDP2
                  S2D=S2D+DRDP1*DRDP3
                  S3D=S3D+DRDP1*DRDP2
                  S4D=S4D+DRDP1**2
                  S8D=S8D+DRDP2**2
C
                  IWRK(IPCO+10)=0
                  WRK(IPCO+13)=RESA
                  IF LJHTLU.EQ.1
                  THEN
                     IP    =IWRK(IPCO+1)
                     LBSIDE=IWRK(IPCO+2)
                     IPHL=IPJHTL+2+(IP-IP00)/4
                     LB=IDATA(IPHL)
                     IDST=ABS(RESA)*5.
                     IF(IDST.GT.31) IDST=31
                     IDST=SHFTL(IDST,11)
                     IF(LBSIDE.EQ.1) IDST=LOR(IDST,256)
                     ITR1=LAND(SHFTR(LB,17),127)
                     IF ITR1.EQ.ITRK
                     THEN
                        IDATA(IPHL)=LOR(LAND(LB,MASK1),SHFTL(IDST,16))
                     ELSE
                        IDATA(IPHL)=LOR(LAND(LB,MASK2),      IDST    )
                     CIF
                  CIF
               CIF
            CFOR
         CFOR
         IF LCOVAR.EQ.1
         THEN
            IF NHF1.LT.10.OR.NHF1.NE.NHFIT
            THEN
               PRINT 6781,NRUN,NEV,ITRK,NHFIT,NHF1
6781           FORMAT(' RUN,EV,TRK,NHFIT,NHF1',I7,I6,I3,2I5)
            ELSE
               DETD=(S8D*S0D-S1D*S1D)*S4D+
     +        (S2D*S1D-S3D*S0D)*S3D+(S3D*S1D-S2D*S8D)*S2D
               FACT=CHISQ/(NHFIT-3)/DETD
C
               IWRK(IP+2) =LOR(IWRK(IP+2),2048)
               WRK(IP+49)=CHISQ/.115**2
               WRK(IP+50)=(S8*S0-S1**2)*FACT
               WRK(IP+51)=(S1*S2-S0*S3)*FACT
               WRK(IP+52)=(S4*S0-S2**2)*FACT
               WRK(IP+53)=(S3*S1-S8*S2)*FACT
               WRK(IP+54)=(S2*S3-S1*S4)*FACT
               WRK(IP+55)=(S8*S4-S3**2)*FACT
            CIF
         CIF
      CPROC
      END
      SUBROUTINE XYRFT1(IPTR,IPJHTL,ERRFAC,LDTR)
C
C        REFIT TRACK ITRK IN 'PATR'-BANK (WITH A VERTEX
C        CONSTRAINT OF STRENGTH 1/ERRFAC IF ERRFAC<200.
C        VERTEX OMITTED IF INCOMPATIBLE.)
C        PARABOLA FIT IF |OLD CURVATURE * HALF TRACK LENGTH| < .04
C        CIRCLE FIT OTHERWISE
C
C    TEST VERSION 3.     (TESTED TO SOME EXTENT)
C    22.2.88   MVC CHANGED TO MVCL (256 BYTES NOT ENOUGH!)  J.H./J.O.
C
C                                J. SPITZER  25/3/87
C
C    EXTENDED TO GIVE COVARIANCE MATRIX FOR FIT PARAMETERS
C    CIRCLE PARAMETERS ARE SET EVEN IF PARABOLA FIT WAS PERFORMED
C                                      J.S.  2/4/87
C
C    DOUBLE PRECISION TO CALCULATE DETERMINANT FOR COVARIANCE MATRIX
C                                      J.S.  5/6/87
C
C    MODIFIED TO UPDATE JHTL BANK UPON REQUEST (MADE IN MODXYV)
C    + SMALL MODIFICATIONS IN THE WORK COMMON FOR THE
C      VERTEX CHAMBER GROUP
C                                      J.S.  5/1/88
C
      IMPLICIT INTEGER*2 (H)
      REAL*8   S0D,S1D,S2D,S3D,S4D,S8D,DETD
C
      COMMON/XYFVT1/MODXYV
C
C
#include "cdata.for"
C
#include "cgeo1.for"
C
#include "calibr.for"
C
#include "cworkpr.for"
#include "cworkeq.for"
C
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
C
#include "cpatlm.for"
C
#include "cjdrch.for"
#include "cdsmax.for"
      INTEGER DATE(5), IDAY /0/
C-----------------------------
      INTEGER NCHECK(5)/5*8/
      REAL RCHECK(12,2,5)/
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1.,
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1.,
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1.,
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1.,
     +   0.,  .004,  .01,  .045,  .06,  .1,  1.,  6., 4*0.,
     +   0.,  .65,   .79,  .86,   .89, .91, .94,  1., 4*1./
C
C
      REAL RESCUT/8./,CKAPP/.966/
      DIMENSION ISORT1(71),ISORT2(3,71),IRESHT(71),ISORT3(91)
     +,ISORT4(2,91)
      DATA KPRT1/0/,NPRT1/50/,IQJETC/0/,IQHEAD/0/
      DATA MASK1/Z2FFFFFF/,MASK2/ZFFFF02FF/,MASK3/ZFFFFF1FF/
C
C
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
         LBINIT = 1
C
         IQJETC = IBLN('JETC')
         IQHEAD = IBLN('HEAD')
C
         CALL DAY2(DATE)
         IDAY = DATE(1)*1000 + DATE(2)
C
N       MULT. SCATTERING CONSTANTS
         RESMS = .020**2/2. * .16 * (1. + ALOG10(.16) /9.) * 155.45**2
C
         WRITE(6,137)
 137     FORMAT(/,' *** XYRFT1 ***  A NEW R-PHI FITTING ROUTINE',/,
     +            '                 TEST VERSION 3. (J. SPITZER)',/
     +     ' BIT 512 OR 1024 IS SET IN THE PROGRAM IDENTIFIER WORD',/,
     +' IN CASE VERTEX CONSTRAINT WAS NOT OR WAS USED RESP.',/,
     +' COVARIANCE MATRIX IS PROVIDED WITH BIT 2048 SWITCHED ON',/,
     +' IF THE PATR BANK ALREADY HAS THE LARGER LENGTH, THE NUMBER',/,
     +' OF HITS USED IN THE FIT IS AT LEAST 10 AND THE FIT CONVERGED.',
     +/)
      CIF
C
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      NEV  = HDATA(IPHEAD+11)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
C
N     RESERVE SPACE IN CWORK
      HPFREE = 1
C
C=======================================================================
N     GET X-Y-VERTEX AND STRENGTH OF VC
      IPV    = ICALIB(10)
      XO     = ACALIB(IPV+ 1)
      YO     = ACALIB(IPV+ 3)
      CURVXY=ADATA(IPTR+25)
      IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
      DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +ADATA(IPTR+9),1./CURVXY,XO,YO,XP,YP,FI)
      FV     = ERRFAC
      IF(FV .LT. .50) FV = .50
      IF NRUN.LT.24200
      THEN
         SRESO=.160
      ELSE
         SRESO=.100
      CIF
      SIGMIN=(SRESO/1.6)**2
      PTRANS = ABS(0.0299792458*BKGAUS/CURVXY) * .001
      RESV=.0100+.25*SIN(FI)**2+RESMS/PTRANS**2
      WGHT0  = (SRESO/FV)**2 / RESV
      INDBIT=512
C=======================================================================
N     HALF DISTANCE BETWEEN FIRST AND LAST POINTS ON TRACK
      XX    =  ADATA(IPTR+12) - ADATA(IPTR+5)
      YY    =  ADATA(IPTR+13) - ADATA(IPTR+6)
      RR    = 0.5*SQRT(XX**2+YY**2)
C
      IF RR.LT.10.
      THEN
         IF KPRT1.LT.NPRT1
         THEN
            KPRT1=KPRT1+1
            WRITE(6,848) NRUN,NEV,ITRK,RR
848         FORMAT(' ******** RUN,EV,TRACK',I8,I6,I3,/,
     +      ' HALF DISTANCE OF FIRST AND LAST POINTS',E14.3,
     +      ', XYRFT1 DOES NOT ATTEMPT R-PHI FIT')
         CIF
         RETURN
      CIF
C
C
N     FETCH HITS, CALCULATE COORDINATES, AND
N     FILL ARRAY IN /CWORK/
      HPCO0  = HPFREE
      LHIT   = 14
C     ORIGIN HALFWAY BETWEEN FIRST AND LAST POINTS ON TRACK
      INDFET = 2
      CALL JFETCH(IPTR,IPJHTL,WRK(HPCO0),LHIT,IPRES,INDFET,XO,YO)
C
C
      HLDCO  = LHIT
      HPCO9  = IPRES - 1
      HPAR0  = IPRES
      HLDPA  = 20
      HPAR9  = HPAR0 + HLDPA - 1
      HPFREE = HPAR9 + 1
      XT     = WRK (IPRES   )
      YT     = WRK (IPRES+ 1)
      CSROT  = WRK (IPRES+ 2)
      SNROT  = WRK (IPRES+ 3)
N     JADE ORIGIN IN THE FIT SYSTEM
      XOR    =- XT*CSROT -  YT*SNROT
      YOR    =  XT*SNROT -  YT*CSROT
N     VERTEX IN THE FIT SYSTEM
      X0     = (XO-XT)*CSROT+(YO-YT)*SNROT
      Y0     =-(XO-XT)*SNROT+(YO-YT)*CSROT
C
N     ZVERT, THETA
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTH   = WRK (IPRES+11)
      SNTH   = WRK (IPRES+12)
C
      IWRK(IPRES+10)=0
C
C ORIGINAL CHI2 AND CURVATURE ERROR IN PATR BANK
      SIG=ADATA(IPTR+23)**2
      IF(SIG.LT.1.E-5) SIG=1.E-5
      SIG11=(.5*ADATA(IPTR+26))**2/SIG
C
C-----------------------------------------------------------------------
C
C     GYMNASTICS FOR PRIVATE HIT QUALIFICATION
C     AND FOR HANDLING MORE HITS ON SAME WIRE
C
C
C TRY TO RECOVER HITS POSSIBLY LOST BY EARLIER FIT
      XHCUT=RR+200.
C
      XREGA= 100000.
      XREGB=-100000.
      NHALL=0
      NHWIR=0
      NHPOT=0
      IPCO=HPCO0
      REPEAT
         NHWIR=NHWIR+1
         IF(NHWIR.GT.70) RETURN
         ISORT1(NHWIR)=NHWIR
         ISORT2(1,NHWIR)=IPCO
         ISORT2(3,NHWIR)=0
         IW0=IWRK(IPCO)
         ICL0=IWRK(IPCO+9)
         LFL=0
         WHILE IPCO.LE.HPCO9
            IW9=IWRK(IPCO)
            ICL9=IWRK(IPCO+9)
            IF IW9.EQ.IW0 .AND. ICL9.EQ.ICL0
            THEN
N        HIT ON THE SAME WIRE
               NHALL=NHALL+1
               IF(NHALL.GT.90) RETURN
               IF(ISORT2(3,NHWIR).EQ.0) ISORT2(2,NHWIR)=NHALL
               ISORT2(3,NHWIR)=ISORT2(3,NHWIR)+1
C
               XA=WRK(IPCO+3)
               LBGOOD=IWRK(IPCO+10)
               IF LBGOOD.LE.2
               THEN
                  IF ABS(XA).GT.XHCUT
                  THEN
                     ISORT3(NHALL)=-1
                  ELSE
                     ISORT3(NHALL)= 1
                     LFL=1
                     IF(XA.LT.XREGA) XREGA=XA
                     IF(XA.GT.XREGB) XREGB=XA
                  CIF
C
                  IWRK(IPCO+10)=1
               ELSE
                  ISORT3(NHALL)=-1
               CIF
C
               IPCO=IPCO+HLDCO
            ELSE
               XWHILE
            CIF
         CWHILE
         IF LFL.EQ.1
         THEN
            NHPOT=NHPOT+1
         ELSE
            ISORT3(ISORT2(2,NHWIR))=-2
         CIF
      UNTIL IPCO.GT.HPCO9
C-----------------------------------------------------------------------
C
C IF LESS THAN 4 HITS SURVIVE NOTHING WILL BE DONE
      IF(NHPOT.LT.4) RETURN
      XHF=.5*(XREGA+XREGB)
      RRPL=.5*(XREGB-XREGA)
      RRMI=RRPL
      IF ABS(XHF).GT.RR
      THEN
         RRPL=RRPL+XHF-SIGN(RR,XHF)
         RRMI=RRMI-XHF+SIGN(RR,XHF)
         XHF=SIGN(RR,XHF)
      CIF
C-----------------------------------------------------------------------
C  STARTING VALUES OF FIT PARAMETERS
C
      LCHC=0
C  CHANGE START VALUE IF CURVATURE INCONSISTENT WITH FIRST & LAST POINTS
      IF ABS(CURVXY*RR).GT.CKAPP
      THEN
         LCHC=1
         CURVXY=SIGN(CKAPP/RR,CURVXY)
      CIF
      IF ABS(CURVXY)*RR .GT. .04
      THEN
C      CIRCLE FIT
         LFTYP=1
         P1=CURVXY/SQRT(1.-(CURVXY*RR)**2)
         AAH=(RR-XHF)*(RR+XHF)
         PAR3=SAGCIR(1.,P1,AAH,SAGPR,1.E-4)
         PAR2=-P1/(1.+PAR3*P1)*XHF
         CSI2GM=1.+PAR2**2
         PAR1=CURVXY*SQRT(CSI2GM)
      ELSE
C      PARABOLA FIT
         LFTYP=2
         PAR3=.5*CURVXY*(RR-XHF)*(RR+XHF)
         PAR2=-CURVXY*XHF
         CSI2GM=1.+PAR2**2
         PAR1=-0.5*CURVXY*SQRT(CSI2GM)*CSI2GM
      CIF
      XMIN=PAR2/PAR1-CKAPP/ABS(CURVXY)
      XMAX=PAR2/PAR1+CKAPP/ABS(CURVXY)
C
C-----------------------------------------------------------------------
C  FIT SHIFT AND ROTATION ONLY (CURVATURE KEPT FIXED)
      DISCUT=400.
      KFLIP=1
      PERFORM SHFROT
      IF(NHFIT.LT.4) RETURN
      IF NHFIT.LT.6
      THEN
C FIT OF SHIFT AND ROTATION HAS ONLY BEEN PERFORMED
C CHI2 AND CURVATURE ERROR REMAIN THE OLD VALUES IN THE PATR BANK
         PERFORM FITBNK
         RETURN
      CIF
C
C-----------------------------------------------------------------------
      IF ABS(DA).GT.1.5 .OR. ABS(DB).GT..1 .OR. LCHC.EQ.1
      THEN
C SHIFT TO ORIGINAL FIT TOO BIG OR CHANGE OF CURVATURE FOR START
C TRY TO FIND CORRECT STARTING VALUES, TAKE CIRCLE IN ANY CASE
         IF LFTYP.EQ.2
         THEN
            LFTYP=1
            PAR1=CUROUT*SQRT(CSI2GM)
            XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
            XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
         CIF
         PERFORM STVCIR
      CIF
C
C
C IF TOO MANY HITS THROWN AWAY EVEN WITH THE VERY LOOSE RESIDUAL CUTS
C DO NOT DARE TO ACCEPT/START THE FIT
C
      IF(NHFIT.LT.NHPOT/2) RETURN
C
C
N      SAVE CURRENT (START) VALUES
      NHFTLS=NHFIT
      PAR1LS=PAR1
      PAR2LS=PAR2
      PAR3LS=PAR3
      CSI2LS=CSI2GM
      CURLST=CUROUT
      XMAXLS=XMAX
      XMINLS=XMIN
      SIG11L=SIG11
      SIGLST=SIG
C
C
N      VERTEX
      NHWIRV=NHWIR+1
      ISORT1(NHWIRV)=NHWIRV
      ISORT2(1,NHWIRV)=-200
C
C
C=======================================================================
C
N     FIRST ITERATE WITHOUT VERTEX CONSTRAINT
      INDMAX=NHFIT/8+1
      IF(INDMAX.GT.8) INDMAX=8
      INDFIT=0
      WHILE INDFIT.LT.INDMAX
         INDFIT=INDFIT+1
N    PARABOLA OR CIRCLE FIT
         PERFORM FTCURV
         IF LNOCON.EQ.1
         THEN
C NO CONVERGENCE AS INDICATED BY LOSS OF TOO MANY HITS
C STILL RETAIN THE EARLIER FIT
C IF IT IS THE ONE OBTAINED IN PROC STVCIR,
C CHI2 AND CURVATURE ERROR REMAIN THE OLD VALUES IN THE PATR BANK
            NHFIT=NHFTLS
            PAR1=PAR1LS
            PAR2=PAR2LS
            PAR3=PAR3LS
            CSI2GM=CSI2LS
            CUROUT=CURLST
            XMAX=XMAXLS
            XMIN=XMINLS
            SIG11=SIG11L
            SIG=SIGLST
C
C DO NOT ATTEMPT VERTEX CONSTRAINT
            PERFORM FITBNK
            RETURN
         CIF
         IF(SIG.LT.SIGMIN) XWHILE
         IF INDFIT.GE.2
         THEN
            PERFORM LLSTOP
            IF LSTOP.EQ.1
            THEN
C      PREVIOUS FIT ACCEPTED, RESTORE ITS RESULTS
               INDFIT=INDFIT-1
               NHFIT=NHFTLS
               PAR1=PAR1LS
               PAR2=PAR2LS
               PAR3=PAR3LS
               CSI2GM=CSI2LS
               CUROUT=CURLST
               XMAX=XMAXLS
               XMIN=XMINLS
               SIG11=SIG11L
               SIG=SIGLST
               KFLIP=3-KFLIP
               XWHILE
            CIF
         CIF
         IF(INDFIT.EQ.INDMAX) XWHILE
N      SAVE FIT RESULTS
         NHFTLS=NHFIT
         PAR1LS=PAR1
         PAR2LS=PAR2
         PAR3LS=PAR3
         CSI2LS=CSI2GM
         CURLST=CUROUT
         XMAXLS=XMAX
         XMINLS=XMIN
         SIG11L=SIG11
         SIGLST=SIG
N      HIT CLEANING
         PERFORM HITCLN
      CWHILE
C
C            ======  VERTEX CONSTRAINT  =======
      X0R=X0-XHF
      IF FV.LT.200. .AND. X0R.GT.XMIN .AND. X0R.LT.XMAX
      THEN
C        VERTEX CONSTRAINT (WEEK OR STRONG) HAS BEEN REQUESTED
C        ROUGH CHECK IF RUN VERTEX CONSISTENT WITH THE TRACK
         IF LFTYP.EQ.2
         THEN
            DVCHI2=((PAR1*X0R+PAR2)*X0R+PAR3-Y0)**2*WGHT0
         ELSE
            AAH=-X0R**2*CSI2GM
            FDBPR=1./(1.+PAR1*X0R*PAR2)
            SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
            DVCHI2=(SAG+PAR2*X0R+PAR3-Y0)**2*WGHT0
         CIF
         IF DVCHI2 .LT.  9.*SIG
         THEN
            ISORT2(1,NHWIRV)=-100
            PERFORM FTCURV
            IF(LNOCON.EQ.0) INDBIT=1024
         CIF
      CIF
C
N     SET UP PATR-BANK
      PERFORM FITBNK
      RETURN
C=======================================================================
C
N     *************************
N     *      F T C U R V      *
N     *************************
C
C
N      PARABOLA OR CIRCLE FIT
      PROC FTCURV
C
      LNOCON=0
N     GET EQUATIONS
N     WEIGHT VERTEX AS POINT OF PARABOLA
      KFLIP=3-KFLIP
      KITER=0
      WHILE KITER .LT. 3-LFTYP
         KITER=KITER+1
         X0R=X0-XHF
         IF ISORT2(1,NHWIRV).EQ.-100 .AND. X0R.GT.XMIN .AND. X0R.LT.XMAX
         THEN
N      VERTEX INCLUDED
            IF LFTYP.EQ.2
            THEN
               DYDP1=X0R**2
               DYDP2=X0R
               DYRES=Y0
            ELSE
               AAH=-X0R**2
               FDBPR=1./(1.+PAR1*X0R*PAR2)
               SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
               CC1=FDBPR/(1.+SAG*PAR1*FDBPR)
               DYDP1=CC1*SAGPR
               DYDP2=X0R+PAR1*CC1*(AAH*PAR2-SAG*X0R)
               DYRES=Y0-SAG-PAR2*X0R-PAR3
            CIF
            S0 = WGHT0
            S1=DYDP2*WGHT0
            S2=DYDP1*WGHT0
            S3=S2*DYDP2
            S4=S2*DYDP1
            S8=S1*DYDP2
            S7=DYRES*WGHT0
            S6=S7*DYDP2
            S5=S7*DYDP1
         ELSE
N      VERTEX OMITTED
            S0 = 0.
            S1 = 0.
            S2 = 0.
            S3 = 0.
            S4 = 0.
            S8 = 0.
            S7 = 0.
            S6 = 0.
            S5 = 0.
         CIF
         S00=S0
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
            CFOR
            IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).EQ.1
                  THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR=XA-XHF
                     IF XAR.GT.XMIN.AND.XAR.LT.XMAX
                     THEN
                        YA = WRK(IPCO+4)
                        IF LFTYP.EQ.2
                        THEN
                           DYDP1A=XAR**2
                           DYDP2A=XAR
                           DYRESA=YA
                           DF0=ABS(YA-((PAR1*XAR+PAR2)*XAR+PAR3))
                        ELSE
                           AAH=-XAR**2
                           FDBPR=1./(1.+PAR1*XAR*PAR2)
                           SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
                           CC1=FDBPR/(1.+SAG*PAR1*FDBPR)
                           DYDP1A=CC1*SAGPR
                           DYDP2A=XAR+PAR1*CC1*(AAH*PAR2-SAG*XAR)
                           DYRESA=YA-SAG-PAR2*XAR-PAR3
                           DF0=ABS(DYRESA)
                        CIF
                     ELSE
                        DF0=15000.
                     CIF
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        DYDP1=DYDP1A
                        DYDP2=DYDP2A
                        DYRES=DYRESA
                        JHUSE=JH
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.RESCUT
               THEN
                  S0=S0+1.
                  S1=S1+DYDP2
                  S2=S2+DYDP1
                  S3=S3+DYDP1*DYDP2
                  S4=S4+DYDP1**2
                  S8=S8+DYDP2**2
                  S7=S7+DYRES
                  S6=S6+DYRES*DYDP2
                  S5=S5+DYRES*DYDP1
                  ISORT4(KFLIP,JHUSE)=1
               ELSE
                  ISORT3(IH)=-2
               CIF
            CIF
         CFOR
         NHF1=S0-S00+.1
         IF NHF1.LT.6 .OR. NHF1.LT.NHPOT/2
         THEN
            LNOCON=1
            XWHILE
         CIF
         NHFIT=NHF1
         DEG   = S0 - S00 - 3.
N     CURVATURE ERROR
         DET = (S8*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S8)*S2
         SIG11 = (S8*S0 - S1*S1)/DET
C
N        SOLVE EQUATIONS
         F1 = 1. / S4
         XX12 = S3*F1
         XX13 = S2*F1
         YY1  = S5*F1
         XX22 = S8 - S3*XX12
         XX23 = S1 - S3*XX13
         YY2  = S6 - S3*YY1
         XX32 = S1 - S2*XX12
         XX33 = S0 - S2*XX13
         YY3  = S7 - S2*YY1
         IF XX22.GT.XX32
         THEN
            XX23 = XX23 / XX22
            YY2  = YY2  / XX22
            PARR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
            PARR2 = YY2 - XX23*PARR3
         ELSE
            XX33 = XX33 / XX32
            YY3  = YY3  / XX32
            PARR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
            PARR2 = YY3 - XX33*PARR3
         CIF
         PARR1 = YY1 - XX12*PARR2 - XX13*PARR3
C
         IF LFTYP.EQ.2
         THEN
            PAR1=PARR1
            PAR2=PARR2
            PAR3=PARR3
            IF(ABS(PAR1).LT.1.E-10) PAR1 = SIGN(1.E-10,PAR1)
            CSI2GM=PAR2**2+1.
            CUROUT =-PAR1 * 2./ (SQRT(CSI2GM)*CSI2GM)
         ELSE
            PAR1=PAR1+PARR1
            PAR2=PAR2+PARR2
            PAR3=PAR3+PARR3
            IF(ABS(PAR1).LT.1.E-10) PAR1 = SIGN(1.E-10,PAR1)
            CSI2GM=PAR2**2+1.
            CUROUT=PAR1/SQRT(CSI2GM)
            XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
            XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
         CIF
      CWHILE
C  END ITERATION DONE IN CASE OF CIRCLE FIT ONLY
C
C
      IF LNOCON.EQ.0
      THEN
N     CALC. CHISQ + SOLVE L/R AMBIGUITY
         CHISQ = 0.
         NHF1=0
         FOR IHWIR=1,NHWIR
            IRESHT(IHWIR)=-1
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            IF ISORT3(IH).GE.0 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).GE.0
                  THEN
                     IFLG=ISORT3(JH)
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XR= WRK(IPCO+3)-XHF
                     IF XR.GT.XMIN.AND.XR.LT.XMAX
                     THEN
                        Y = WRK(IPCO+4)
                        IF LFTYP.EQ.2
                        THEN
                           DF0=ABS(Y-(PAR1*XR+PAR2)*XR-PAR3)
                        ELSE
                           AAH=-XR**2*CSI2GM
                           FDBPR=1./(1.+PAR1*XR*PAR2)
                           SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
                           DF0=ABS(Y-SAG-PAR2*XR-PAR3)
                        CIF
                        IF(DF0.LT.RESMIN) RESMIN=DF0
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.5000.
               THEN
                  IRESHT(IHWIR)=RESMIN*1.E6
                  IF IFLG.EQ.1
                  THEN
                     CHISQ=CHISQ+RESMIN**2
                     NHF1=NHF1+1
                  CIF
               CIF
            CIF
         CFOR
         IF(NHF1.LT.NHFIT-3) LNOCON=1
         SIG    =      CHISQ  / DEG
      CIF
      CPROC
C=======================================================================
      PROC HITCLN
C      LABEL HITS NOT TO BE USED IN THE NEXT ITRATION
N       SQRT(CHI2) OF VERTEX
C  VERTEX NOT INCLUDED IN THE ITERATIVE PART
C  IN THE CURRENT VERSION
CV       X0R=X0-XHF
CV       IF LFTYP.EQ.2
CV       THEN
CV          DFVERT=((PAR1*X0R+PAR2)*X0R+PAR3-Y0)*SQRT(WGHT0)
CV       ELSE
CV          AAH=-X0R**2*CSI2GM
CV          FDBPR=1./(1.+PAR1*X0R*PAR2)
CV          SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
CV          DFVERT=(SAG+PAR2*X0R+PAR3-Y0)*SQRT(WGHT0)
CV       CIF
CV       IRESHT(NHWIRV)=ABS(DFVERT)*1.E6
C-------------------------------------------------------------
C
N       SORT HITS ACCORDING TO RESIDUALS
C  EXCLUDE THE INDFIT LARGEST RESIDUAL HITS,
C  RESTORE THE OTHERS (EXLUDED FOR EVER HITS NOT COUNTED)
C
CV       CALL SHELL9(IRESHT,ISORT1,NHWIRV)
         CALL SHELL9(IRESHT,ISORT1,NHWIR)
         KOMIT=0
CV       FOR J1=1,NHWIRV
         FOR J1=1,NHWIR
CV          IHWIR=ISORT1(NHWIRV+1-J1)
            IHWIR=ISORT1(NHWIR+1-J1)
            IPCO=ISORT2(1,IHWIR)
            IF IPCO.NE.-100 .AND. IPCO.NE.-200
            THEN
N     HIT, NOT VERTEX
               NNH=ISORT2(3,IHWIR)
               IH=ISORT2(2,IHWIR)
               LFLG=0
               FOR JNH=1,NNH
                  IHA=IH+JNH-1
                  IQA=ISORT3(IHA)
                  IF IQA.GT.-1
                  THEN
                     IF LFLG.EQ.0
                     THEN
                        LFLG=1
                        KOMIT=KOMIT+1
                     CIF
                     IF KOMIT.LE.INDFIT
                     THEN
                        ISORT3(IHA)=0
                     ELSE
                        ISORT3(IHA)=1
                     CIF
                  CIF
               CFOR
CV          ELSE
C   VERTEX;   DOES NOT OCCOUR IN THE CURRENT VERSION
CV             KOMIT=KOMIT+1
CV             IF(KOMIT.LE.INDFIT) WGHT0=WGHT0*.01
            CIF
         CFOR
      CPROC
C=======================================================================
      PROC LLSTOP
         IF INDFIT.LE.6
         THEN
            INDCK=INDFIT-1
         ELSE
            INDCK=5
         CIF
         ICHCK=NCHECK(INDCK)
         WHILE SIGLST.LT.RCHECK(ICHCK,1,INDCK)
            ICHCK=ICHCK-1
         CWHILE
         IF(ICHCK.LT.1) ICHCK=1
         IF SIG/SIGLST.GT.RCHECK(ICHCK,2,INDCK)
         THEN
            LSTOP=1
         ELSE
            LSTOP=0
         CIF
      CPROC
C=======================================================================
C
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
N     START + END POINTS
         XST=XHF+XMIN
         IF(XST.LT.XREGA) XST=XREGA
         XSTR=XST-XHF
         XEN=XHF+XMAX
         IF(XEN.GT.XREGB) XEN=XREGB
         XENR=XEN-XHF
         IF LFTYP.EQ.2
         THEN
            YST  = (PAR1 *XSTR+ PAR2 )*XSTR+ PAR3
            YEN  = (PAR1 *XENR+ PAR2 )*XENR+ PAR3
N     DIRECTION AT START + END POINT
            TGST = PAR1*XSTR*2.+ PAR2
            TGEN = PAR1*XENR*2.+ PAR2
         ELSE
            AAH=-XSTR**2
            FDBPR=1./(1.+PAR1*XSTR*PAR2)
            SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
            YST=SAG+PAR2*XSTR+PAR3
         TGST=PAR2-PAR1*FDBPR/(1.+SAG*PAR1*FDBPR)*(SAG*PAR2+XSTR*CSI2GM)
            AAH=-XENR**2
            FDBPR=1./(1.+PAR1*XENR*PAR2)
            SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
            YEN=SAG+PAR2*XENR+PAR3
         TGEN=PAR2-PAR1*FDBPR/(1.+SAG*PAR1*FDBPR)*(SAG*PAR2+XENR*CSI2GM)
         CIF
         DXST = 1./SQRT(TGST**2+1.)
         DYST = DXST * TGST
         DXEN = 1./SQRT(TGEN**2+1.)
         DYEN = DXEN * TGEN
C
C
C
N     COPY TRACK BANK
         HPTR0 = HPFREE
         CALL MVCL(IWRK(HPTR0),0,IDATA(IPTR+1),0,4*LDTR)
C
N     FILL FIT-BANK
         IP    = HPTR0 - 1
         IWRK(IP+2) = LAND(IWRK(IP+2),MASK3)
         IWRK(IP+2) = LOR(IWRK(IP+2),INDBIT)
         IWRK(IP+ 3) = IDAY
         WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
         WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
         WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2)*TGTH+ZVERT
         DXSTJ       =  DXST*CSROT - DYST*SNROT
         DYSTJ       =  DXST*SNROT + DYST*CSROT
         WRK (IP+ 8) =  DXSTJ*CSTH
         WRK (IP+ 9) =  DYSTJ*CSTH
         WRK (IP+12) = XEN *CSROT - YEN *SNROT + XT
         WRK (IP+13) = XEN *SNROT + YEN *CSROT + YT
         WRK (IP+14) = SQRT(WRK(IP+12)**2 + WRK(IP+13)**2)*TGTH+ZVERT
         WRK (IP+15) = (DXEN*CSROT - DYEN*SNROT)*CSTH
         WRK (IP+16) = (DXEN*SNROT + DYEN*CSROT)*CSTH
         IWRK(IP+24) = NHFIT
         WRK (IP+25) = CUROUT
         WRK (IP+27) = CUROUT
         WRK (IP+28) = CUROUT
C
         WRK (IP+23) = SQRT(SIG)
         WRK (IP+26) = SQRT(SIG*SIG11/CSI2GM)
         IF(LFTYP.EQ.2) WRK(IP+26)=WRK(IP+26)*2./CSI2GM
C
C        IWRK(IP+18) = LFTYP
C EVEN IF PARABOLA FIT WAS DONE, CIRCLE PARAMETERS ARE STORED
         IWRK(IP+18) = 1
C
         PAR1=CUROUT*SQRT(CSI2GM)
         SIGNC=SIGN(1.,CUROUT)
         ACURV=ABS(CUROUT)
         A=((XHF-XOR)*PAR2-PAR3+YOR)/SQRT(CSI2GM)
         B=(XHF-XOR+(PAR3-YOR)*PAR2)/SQRT(CSI2GM)
         FDBPR=1./ACURV+SIGNC*A
         IF FDBPR.LT.100.
         THEN
            DIMP=-1./ACURV+SQRT(FDBPR**2+B**2)
         ELSE
            DIMP=SIGNC*A+SAGCIR(1./(1.+CUROUT*A),ACURV,B**2,SP,1.E-4)
         CIF
         FDBPR=1.+DIMP*ACURV
         IF FDBPR.LT.ACURV*1.E-3
         THEN
            WRK(IP+19)=ACURV
            WRK(IP+20)=DIMP
            WRK(IP+21)=0.
         ELSE
            FDBPR=SIGNC/FDBPR
            SGPFI=(PAR2*CSROT+SNROT)/SQRT(CSI2GM)
            CGPFI=(CSROT-PAR2*SNROT)/SQRT(CSI2GM)
            COSALP=(CUROUT*(XHF*CSROT-PAR3*SNROT+XT)+SGPFI)*FDBPR
            SINALP=(CUROUT*(XHF*SNROT+PAR3*CSROT+YT)-CGPFI)*FDBPR
            WRK(IP+19)=ACURV
            WRK(IP+20)=DIMP
            WRK(IP+21)=ATAN2(SINALP,COSALP)
C
            IF LDTR.GE.55.AND.NHFIT.GE.10.AND.LNOCON.EQ.0
            THEN
               LCOVAR=1
            ELSE
               LCOVAR=0
            CIF
            IF LAND(MODXYV,32) .NE. 0
            THEN
               LJHTLU=1
            ELSE
               LJHTLU=0
            CIF
C           CALCULATE COVARIANCE MATRIX AND/OR UPDATE JHTL BANK
            PERFORM COVAR
C
            IWRK(IPRES+10)=1
C
         CIF
C
N     PUT RESULT INTO PATR-BANK
         CALL MVCL(IDATA(IPTR+1),0,IWRK(HPTR0),0,4*LDTR)
C
C
      CPROC
C-----------------------------------------------------------------------
      PROC SHFROT
C  FIT SHIFT AND ROTATION ONLY (CURVATURE KEPT FIXED)
         S0 = 0.
         S1 = 0.
         S2 = 0.
         S3 = 0.
         S4 = 0.
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
            CFOR
            IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).EQ.1
                  THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR=XA-XHF
                     IF XAR.GT.XMIN .AND. XAR.LT.XMAX
                     THEN
                        YA = WRK(IPCO+4)
                        IF LFTYP.EQ.2
                        THEN
                           DYRESA=YA-(PAR1*XAR+PAR2)*XAR-PAR3
                        ELSE
                           AAH=-XAR**2*CSI2GM
                           FDBPR=1./(1.+PAR1*XAR*PAR2)
                           SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
                           DYRESA=YA-SAG-PAR2*XAR-PAR3
                        CIF
                        DF0=ABS(DYRESA)
                     ELSE
                        DF0=15000.
                     CIF
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        XR=XAR
                        DYRES=DYRESA
                        JHUSE=JH
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.DISCUT
               THEN
                  S0=S0+1.
                  S1=S1+XR
                  S2=S2+XR**2
                  S3=S3+DYRES
                  S4=S4+DYRES*XR
                  ISORT4(KFLIP,JHUSE)=1
               CIF
            CIF
         CFOR
         NHFIT=S0+.1
         IF NHFIT.GE.4
         THEN
            S12=S1/S2
            S42=S4/S2
            DA=(S3-S1*S42)/(S0-S1*S12)
            DB=S42-S12*DA
            PAR3=PAR3+DA
            PAR2=PAR2+DB
            CSI2GM=1.+PAR2**2
            IF LFTYP.EQ.2
            THEN
               CUROUT =-PAR1 * 2./ (SQRT(CSI2GM)*CSI2GM)
            ELSE
               CUROUT=PAR1/SQRT(CSI2GM)
               XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
               XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
            CIF
         CIF
      CPROC
C=======================================================================
      PROC STVCIR
C  TRY TO FIND STARTING VALUES FOR CIRCLE FIT
C  THIS PART IS EXECUTED FOR ONLY A VERY SMALL FRACTION OF THE TRACKS
C  JUST LOOP UNTIL 10, NO STOP CONDITION CHECKED
      ISTV1=0
      DISCUT=400.
      WHILE ISTV1.LT.10
         ISTV1=ISTV1+1
C  FIT PARABOLA P1*X**2+P2*X+P3 TO RESIDUALS & MODIFY CIRCLE PARAMETERS
         S0 = 0.
         S1 = 0.
         S2 = 0.
         S3 = 0.
         S4 = 0.
         S8 = 0.
         S7 = 0.
         S6 = 0.
         S5 = 0.
         KFLIP=3-KFLIP
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               ISORT4(KFLIP,IH+JNH-1)=0
            CFOR
            IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).EQ.1
                  THEN
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                     XA = WRK(IPCO+3)
                     XAR= WRK(IPCO+3)-XHF
                     IF XAR.GT.XMIN.AND.XAR.LT.XMAX
                     THEN
                        YA = WRK(IPCO+4)
                        AAH=-XAR**2
                        FDBPR=1./(1.+PAR1*XAR*PAR2)
                        SAG=SAGCIR(FDBPR,PAR1,AAH*CSI2GM,SAGPR,1.E-4)
                        DYRESA=YA-SAG-PAR2*XAR-PAR3
                        DF0=ABS(DYRESA)
                     ELSE
                        DF0=15000.
                     CIF
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        X=XAR
                        DYRES=DYRESA
                        JHUSE=JH
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.DISCUT
               THEN
                  S0=S0+1.
                  S1=S1+X
                  S2=S2+X**2
                  S3=S3+X**3
                  S4=S4+X**4
                  S8=S8+X**2
                  S7=S7+DYRES
                  S6=S6+DYRES*X
                  S5=S5+DYRES*X**2
                  ISORT4(KFLIP,JHUSE)=1
               CIF
            CIF
         CFOR
         NHFIT=S0+.1
C
C
         IF(NHFIT.LT.5) RETURN
C
C
N        SOLVE EQUATIONS
         F1 = 1. / S4
         XX12 = S3*F1
         XX13 = S2*F1
         YY1  = S5*F1
         XX22 = S8 - S3*XX12
         XX23 = S1 - S3*XX13
         YY2  = S6 - S3*YY1
         XX32 = S1 - S2*XX12
         XX33 = S0 - S2*XX13
         YY3  = S7 - S2*YY1
         IF XX22.GT.XX32
         THEN
            XX23 = XX23 / XX22
            YY2  = YY2  / XX22
            PARR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
            PARR2 = YY2 - XX23*PARR3
         ELSE
            XX33 = XX33 / XX32
            YY3  = YY3  / XX32
            PARR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
            PARR2 = YY3 - XX33*PARR3
         CIF
         PARR1 = YY1 - XX12*PARR2 - XX13*PARR3
C
         XAR=-.7*RRMI
         IF(XAR.LT..8*XMIN) XAR=.8*XMIN
         XBR= .7*RRPL
         IF(XBR.GT..8*XMAX) XBR=.8*XMAX
         IF -XAR.LT.XBR
         THEN
            XBR=-XAR
         ELSE
            XAR=-XBR
         CIF
C
         AAH=-XAR**2*CSI2GM
         FDBPR=1./(1.+PAR1*XAR*PAR2)
         SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
         YA=SAG+PAR2*XAR+PAR3 + (PARR1*XAR+PARR2)*XAR+PARR3
         AAH=-XBR**2*CSI2GM
         FDBPR=1./(1.+PAR1*XBR*PAR2)
         SAG=SAGCIR(FDBPR,PAR1,AAH,SAGPR,1.E-4)
         YB=SAG+PAR2*XBR+PAR3 + (PARR1*XBR+PARR2)*XBR+PARR3
         YC=PAR3+PARR3
         P2=(YB-YA)/(2.*XBR)
         C2=1.+P2**2
         SAG=YC-.5*(YA+YB)
         IF SAG**2.GT.(CKAPP*XBR)**2*C2
         THEN
            SAG=SIGN(CKAPP*XBR*SQRT(C2),SAG)
            YC=.5*(YA+YB)+SAG
         CIF
         PAR3=YC
         P1=2.*SAG/(C2*XBR**2-SAG**2)
         CUROUT=P1/SQRT(C2*(1.+(P1*XBR)**2))
         IF(ABS(CUROUT).LT.1.E-8) CUROUT= SIGN(1.E-8,CUROUT)
         PAR2=P2/(1.+SAG*P1)
         CSI2GM=1.+PAR2**2
         PAR1=CUROUT*SQRT(CSI2GM)
         XMIN=PAR2/PAR1-CKAPP/ABS(CUROUT)
         XMAX=PAR2/PAR1+CKAPP/ABS(CUROUT)
C
         DISCUT=.5*DISCUT
         IF(DISCUT.LT.10.) DISCUT=10.
      CWHILE
      CPROC
C-----------------------------------------------------------------------
      PROC COVAR
C
C      CALCULATE COVARIANCE MATRIX AND/OR UPDATE JHTL BANK
C
         SAMFI=SINALP*CSROT-COSALP*SNROT
         CAMFI=COSALP*CSROT+SINALP*SNROT
C
C        UPDATE OF JHTL FOR HITS   N O T   USED IN THE FIT
         IP00=2*IDATA(IQJETC)+100
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               JH=IH+JNH-1
               IF ISORT4(KFLIP,JH).NE.1
               THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                  X=WRK(IPCO+3)
                  Y=WRK(IPCO+4)
                  A=(X-XOR)*CAMFI+(Y-YOR)*SAMFI
                  B=(X-XOR)*SAMFI-(Y-YOR)*CAMFI
                  FDBPR=1./ACURV+DIMP-A
                  IF FDBPR.LT.100.
                  THEN
                     SAG=SQRT(FDBPR**2+B**2)
                     RESA=SAG-1./ACURV
                  ELSE
                     FDBPR=1./(1.+ACURV*(DIMP-A))
                     SAG=SAGCIR(FDBPR,ACURV,B**2,SP,1.E-4)
                     RESA=DIMP-A+SAG
                  CIF
C
C                 IWRK(IPCO+10)=1
                  WRK(IPCO+13)=RESA
                  IF LJHTLU.EQ.1
                  THEN
                     IP1   =IWRK(IPCO+1)
                     LBSIDE=IWRK(IPCO+2)
                     IPHL=IPJHTL+2+(IP1-IP00)/4
                     LB=IDATA(IPHL)
                     IDST=ABS(RESA)*5.
                     IF(IDST.GT.31) IDST=31
                     IDST=SHFTL(IDST,11)
                     IDST=LOR(IDST,1024)
                     IF(LBSIDE.EQ.1) IDST=LOR(IDST,256)
                     ITR1=LAND(SHFTR(LB,17),127)
                     IF ITR1.EQ.ITRK
                     THEN
                        IDATA(IPHL)=LOR(LAND(LB,MASK1),SHFTL(IDST,16))
                     ELSE
                        IDATA(IPHL)=LOR(LAND(LB,MASK2),      IDST    )
                     CIF
                  CIF
               CIF
            CFOR
         CFOR
C
         CHISQ=0.
C
N      VERTEX OMITTED
         NHF1=0
         S0D= 0.D0
         S1D= 0.D0
         S2D= 0.D0
         S3D= 0.D0
         S4D= 0.D0
         S8D= 0.D0
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               JH=IH+JNH-1
               IF ISORT4(KFLIP,JH).EQ.1
               THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*HLDCO
                  X=WRK(IPCO+3)
                  Y=WRK(IPCO+4)
                  A=(X-XOR)*CAMFI+(Y-YOR)*SAMFI
                  B=(X-XOR)*SAMFI-(Y-YOR)*CAMFI
                  FDBPR=1./ACURV+DIMP-A
                  IF FDBPR.LT.100.
                  THEN
                     SAG=SQRT(FDBPR**2+B**2)
                     RESA=SAG-1./ACURV
                     DRDP2=FDBPR/SAG
                     DRDP1=B*(DRDP2+A/SAG)
                     DRDP3=1./ACURV**2*(1.-DRDP2)
                  ELSE
                     FDBPR=1./(1.+ACURV*(DIMP-A))
                     SAG=SAGCIR(FDBPR,ACURV,B**2,SP,1.E-4)
                     DRDP2=1./(1.+SAG*FDBPR*ACURV)
                     DRDP1=DRDP2*B*(1.+A*FDBPR*ACURV)
                     DRDP3=DRDP2*FDBPR*SP
                     RESA=DIMP-A+SAG
                  CIF
C
                  NHF1=NHF1+1
                  CHISQ=CHISQ+RESA**2
                  S0D=S0D+DRDP3**2
                  S1D=S1D+DRDP3*DRDP2
                  S2D=S2D+DRDP1*DRDP3
                  S3D=S3D+DRDP1*DRDP2
                  S4D=S4D+DRDP1**2
                  S8D=S8D+DRDP2**2
C
                  IWRK(IPCO+10)=0
                  WRK(IPCO+13)=RESA
                  IF LJHTLU.EQ.1
                  THEN
                     IP1   =IWRK(IPCO+1)
                     LBSIDE=IWRK(IPCO+2)
                     IPHL=IPJHTL+2+(IP1-IP00)/4
                     LB=IDATA(IPHL)
                     IDST=ABS(RESA)*5.
                     IF(IDST.GT.31) IDST=31
                     IDST=SHFTL(IDST,11)
                     IF(LBSIDE.EQ.1) IDST=LOR(IDST,256)
                     ITR1=LAND(SHFTR(LB,17),127)
                     IF ITR1.EQ.ITRK
                     THEN
                        IDATA(IPHL)=LOR(LAND(LB,MASK1),SHFTL(IDST,16))
                     ELSE
                        IDATA(IPHL)=LOR(LAND(LB,MASK2),      IDST    )
                     CIF
                  CIF
               CIF
            CFOR
         CFOR
         IF LCOVAR.EQ.1
         THEN
            IF NHF1.LT.10.OR.NHF1.NE.NHFIT
            THEN
               PRINT 6781,NRUN,NEV,ITRK,NHFIT,NHF1
6781           FORMAT(' RUN,EV,TRK,NHFIT,NHF1',I7,I6,I3,2I5)
            ELSE
               DETD=(S8D*S0D-S1D*S1D)*S4D+
     +        (S2D*S1D-S3D*S0D)*S3D+(S3D*S1D-S2D*S8D)*S2D
               FACT=CHISQ/(NHFIT-3)/DETD
C
               IWRK(IP+2) =LOR(IWRK(IP+2),2048)
               WRK(IP+49)=CHISQ/.115**2
               WRK(IP+50)=(S8*S0-S1**2)*FACT
               WRK(IP+51)=(S1*S2-S0*S3)*FACT
               WRK(IP+52)=(S4*S0-S2**2)*FACT
               WRK(IP+53)=(S3*S1-S8*S2)*FACT
               WRK(IP+54)=(S2*S3-S1*S4)*FACT
               WRK(IP+55)=(S8*S4-S3**2)*FACT
            CIF
         CIF
      CPROC
      END
C   09/06/83 806271304  MEMBER NAME  ZSRFTV   (JADEGS)      SHELTRAN
C   09/06/83 803181238  MEMBER NAME  ZSRFTV   (S)           SHELTRAN
      SUBROUTINE ZSRFTV(MODE,IOPT)
C-----------------------------------------------------------------------
C                                   J. SPITZER 22/4/87
C                    UPDATED TO GIVE COMMON Z    1/6/87  J.S.
C    16.5.88 ARSIN ARG PROTECTION ADDED E.E.
C    13.5.88 (FRIDAY!) SEVERE BUG CORRECTED IN SEVERAL PLACES J.H./J.O.
C            ARSIN INSTEAD OF SIN
C    18.3.88   PROPER RUN NUMBER HANDLING USING LDATYP      E E
C    22.2.88   MVC CHANGED TO MVCL (256 BYTES NOT ENOUGH!)  J.H./J.O.
C
C       A GENERAL S-Z FIT ROUTINE
C       S = TRACK LENGTH ALONG THE CIRCLE COUNTED FROM THE
C           FIRST POINT IN THE DIRECTION OF THE LAST ONE
C
C      MODE   = 0 : OVERWRITE OLD PATR-BANK WITH NEW RESULTS
C      MODE   = 1 : CREATE NEW PATR-BANK WITH NEW RESULTS
C
C      IOPT =   1 : S-Z FIT SEPARATELY FOR ALL TRACKS
C      IOPT =   2 : S-Z FIT SEPARATELY FOR ALL TRACKS AND SUBSEQUENTLY
C                   A COMMON S-Z FIT FOR THOSE ONES WHICH
C                   EXTRAPOLATE WITHIN 15 MM TO THE RUN VERTEX IN R-PHI
C                   AND HAVE  | Z(R=0) | < 800 MM
C      *****************************************************************
C      *  THE FOLLOWIG OPTION NEEDS FILLING OF A COMMON IN ADDITION !!!*
C      *****************************************************************
C      IOPT =   4 : COMMON S-Z FITS FOR USER SPECIFIED (UPTO 5) SETS
C                   OF TRACKS WITH USER SPECIFIED COMMON (X,Y) POINTS
C                   IN R-PHI (OR SINGLE TRACK S-Z FIT, SEE LATER).
C                   THE SINGLE TRACK S-Z FITS ARE ONLY DONE FOR THOSE
C                   TRACKS WHICH APPEAR IN THE SETS.
C                   IF A TRACK IS NOT WITHIN 15 MM TO THE SPECIFIED
C                   (X,Y) POINT OR | Z(X,Y) | > 1600 MM, IT WILL BE
C                   DISCARDED.
C
C    TRACK SELECTION PARAMETERS FOR COMMON Z FIT ARE IN /CCMZCT/
C
C    ( IOPT <= IOPT+8 CREATES A SPECIAL BANK IN ADDITION )
C
C    For details of the parameter setting see:
C    JCN 95, p.1 and addendum p.7
C    JCN 95, supplement 1, p.3
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C-----------------------------------------------------------------------
C *****************************************************************
C *  THE COMMON TO BE FILLED BY THE USER IF IOPT=4 IS REQUESTED   *
C *****************************************************************
C
      COMMON/CZSSTE/ NSETZS,NTSTZS(5),KTSTZS(100,2,5),XYSTZS(2,5),
     +SQCHZS(3,5)
C
C NSETZS : NUMBER OF TRACK SETS FOR WHICH COMMON FIT IS TO BE DONE
C NTSTZS(1..NSETZS) : NUMBER OF TRACKS IN EACH SET
C KTSTZS(1..,1,I) TRACK SEQUENCE NUMBERS FOR SET "I"
C XYSTZS(1,I), XYSTZS(2,I) : COMMON X,Y FOR SET "I"
C
C FOR SINGLE TRACK FIT PLEASE SET     NSETZS=1, NTSTZS(1)=1,
C                                     KTSTZS(1,1,1)=ITRK (TR. SEQ. NR.)
C                              NOTHING ELSE NEEDS TO BE SET.
C
C  ***********
C  * ON EXIT *
C  ***********
C
C  KTSTZS(.,2,.)  IS A FLAG THAT TELLS YOU:
C       2 : TRACK WAS USED IN A COMMON Z-FIT
C       1 : SINGLE TRACK FIT WAS SUCCESSFUL BUT
C           THE TRACK WAS NOT USED IN A COMMON FIT
C       0 : TRACK WAS NOT CONSIDERED FOR REFIT
C      <0 : SINGLE TRACK FIT FAILED (TOO FEW USABLE HITS IN GENERAL)
C
C  THIS INFORMATION IS PROVIDED FOR IOPT=1 AND 2 AS WELL BUT
C  WITHOUT FILLING  KTSTZS(J,1,.)=J
C
C  SQCHZS(1,.) : SQUARE ROOT OF {SUM OF WEIGHTED RESIDUAL SQUARES
C                DIVIDED BY THE NUMBER OF DEGREES OF FREEDOM (LATTER=
C                NHITS-NTRACKS-1)} FOR COMMON FIT (.)
C                OR 0.0 IF NO COMMON FIT WAS PERFORMED.
C                THE (DIFFERENT FOR DL8 AND FADC) NORMALIZATION
C                TO GET THE CHI**2/DOF. IS LEFT FOR THE USER
C  SQCHZS(2,.) : NUMBER OF HITS USED IN THE COMMON FIT OR 0.0; REAL !!!
C  SQCHZS(3,.) : COMMON Z AT THE COMMON (X,Y)    (OR 0.0)
C
      COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5)
C
C THIS COMMON IS BLOCK DATA SET IN   JADEBD
C
C     BLOCK DATA
C     COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5)
C     DATA  DIMPCT /15./, ZCUTV /800./, ZCUTVV /1600./, IZVCST/5*0/
C     END
C=======================================================================
C
#include "cdata.for"
#include "calibr.for"
C
      COMMON/CWORK/NDIWRK,WRK(20000)
      DIMENSION IWRK(20000),HWRK(40000)
      EQUIVALENCE (WRK(1),IWRK(1),HWRK(1))
C
      INTEGER ITRREQ(100)
      REAL CORTRC(2,100)
      DATA LBINIT /0/, IQPATR/0/, IQJHTL/0/, IQHEAD/0/, IQJETC/0/
      DATA NPRLIM/50/,KPRLIM/0/
C=======================================================================
C
      KOPT=IOPT
      IF(KOPT.GT.8) KOPT=KOPT-8
      IF KOPT.NE.1 .AND. KOPT.NE.2 .AND. KOPT.NE.4
      THEN
         IF KPRLIM.LT.NPRLIM
         THEN
            KPRLIM=KPRLIM+1
            WRITE(6,100) IOPT
100         FORMAT(' **** ZSRFTV CALLED WITH INVALID OPTION :',I8)
         CIF
         RETURN
      CIF
C
C
      IF KOPT.EQ.4
      THEN
C PRODUCE ARRAY OF ALL TRACKS REQUESTED; CHECK CONSISTENCY OF REQUEST
         IF NTSTZS(1).EQ.1
         THEN
            KTC=KTSTZS(1,1,1)
            IF KTC.LT.1 .OR. KTC.GT.100
            THEN
               LINCON=1
            ELSE
               LINCON=0
               NTRREQ=1
               ITRREQ(1)=KTC
               KTSTZS(1,2,1)=0
            CIF
         ELSE
            IF NSETZS.GE.1.AND.NSETZS.LE.5
            THEN
               NTRREQ=0
               LINCON=0
               FOR ISET=1,NSETZS
                  SQCHZS(1,ISET)=0.
                  SQCHZS(2,ISET)=0.
                  SQCHZS(3,ISET)=0.
                  NTSETI=NTSTZS(ISET)
                  IF NTSETI.LT.2 .OR. NTSETI.GT.100
                  THEN
                     LINCON=1
                     XFOR
                  CIF
                  FOR JT=1,NTSETI
                     KTC=KTSTZS(JT,1,ISET)
                     IF KTC.LT.1 .OR. KTC.GT.100
                     THEN
                        LINCON=1
                        XFOR
                     CIF
                     KTSTZS(JT,2,ISET)=0
                     LPRES=0
                     IF NTRREQ.GT.0
                     THEN
                        FOR J=1,NTRREQ
                           IF ITRREQ(J).EQ.KTC
                           THEN
                              LPRES=1
                              XFOR
                           CIF
                        CFOR
                     CIF
                     IF LPRES.EQ.0
                     THEN
                        IF NTRREQ.EQ.100
                        THEN
                           LINCON=1
                           XFOR
                        CIF
                        NTRREQ=NTRREQ+1
                        ITRREQ(NTRREQ)=KTC
                     CIF
                  CFOR
                  IF(LINCON.NE.0) XFOR
               CFOR
            ELSE
               LINCON=1
            CIF
         CIF
         IF LINCON.NE.0
         THEN
            IF KPRLIM.LT.NPRLIM
            THEN
               KPRLIM=KPRLIM+1
               WRITE(6,200)
200            FORMAT(' **** ZSRFTV: INVALID REQUEST IN /CZSSTE/')
            CIF
            RETURN
         CIF
      ELSE
         SQCHZS(1,1)=0.
         SQCHZS(2,1)=0.
         SQCHZS(3,1)=0.
         FOR J=1,100
            KTSTZS(J,2,1)=0
         CFOR
      CIF
C-----------------------------------------------------------------------
N     INITIALIZATION
      IF LBINIT .LE.0
      THEN
         LBINIT = 1
         IQPATR = IBLN('PATR')
         IQJHTL = IBLN('JHTL')
         IQHEAD = IBLN('HEAD')
         IQJETC = IBLN('JETC')
      CIF
C
C-----------------------------------------------------------------------
C
N     CHECK IF PATR-BANK
      IF(IDATA(IQPATR).LE.0) RETURN
C-----------------------------------------------------------------------
C
C     CREATE NEW PATR BANK IF REQUESTED
      IF MODE.EQ.1
      THEN
         IPPAT0 = IDATA(IQPATR)
         NBNK1  = IDATA(IPPAT0-2) - 1
         NWRD   = IDATA(IPPAT0)
         NBYTE  = NWRD*4
         CALL CCRE(IPPATR,'PATR',NBNK1,NWRD,IERR)
         IF IERR.NE.0
         THEN
            PRINT 2900, IERR
 2900       FORMAT(' CREATION OF NEW PATR-BANK RESULTED IN ERROR',I3)
            RETURN
         CIF
N        COPY CONTENTS OF 'PATR'-BANK
         CALL MVCL(IDATA(IPPATR+1),0,IDATA(IPPAT0+1),0,NBYTE)
      CIF
C-----------------------------------------------------------------------
C
      IPPATR = IDATA(IQPATR)
      IPTR   = IDATA(IPPATR+1) + IPPATR
      LDTR   = IDATA(IPPATR+3)
      NTR    = IDATA(IPPATR+2)
C
N     CHECK IF 1 TRACK
      IF(NTR.LT.1) RETURN
C
      IF NTR.GT.100
      THEN
         IF KPRLIM.LT.NPRLIM
         THEN
            KPRLIM=KPRLIM+1
            WRITE(6,300) NTR
300         FORMAT(' **** ZSRFTV : NUMBER OF TRACKS IN PATR BANK :',
     +      I4,'. FIRST 100 WILL BE CONSIDERED.')
         CIF
         NTR=100
      CIF
C
C-----------------------------------------------------------------------
C
C GET LATEST AMPLITUDE CALIBRATION
      CALL JRECAL(IERR)
      IF IERR.NE.0
      THEN
         PRINT 6784,IERR
 6784    FORMAT(' *** ERROR IN JRECAL',I3)
         RETURN
      CIF
C-----------------------------------------------------------------------
C
N     RECALIBRATE Z-COORDINATES
      IPJETC = IDATA(IQJETC)
      IPJHTL = IDATA(IQJHTL)
C
C MODEZ=1 MEANS    CALIBRATION ONLY
C
      MODEZ  = 1
      CALL ZSFIT(IPJETC,IDATA(IPJETC-1),IPJHTL,IPPATR,MODEZ)
C
C
C=======================================================================
C
C     COLLECTION OF HIT DATA IN /CWORK/ AND SINGLE TRACK FITS
C
C DIMENSION OF WRK(.)
      NDIWRK=20000
C NUMBER OF TRACKS STORED IN /CWORK/
      NTRKS=0
C POINTER TO TRACK DATA THAT STORES THE STRUCTURE OF WRK(.) ETC.
      IDTR2=1
C LENGTH OF ABOVE DATA PRO TRACK
      LDTR2=11
C LENGTH OF HIT AND SUBSEQUENT TRACK DATA IN /CWORK/
      LHIT=8
      LTRREC=0
C POINTER TO FIRST HIT OF TRACK IN /CWORK/
      IHIT1=NTR*LDTR2+1
C
C
      FOR ITR=1,NTR
C NO SPACE TO STORE MORE TRACKS
         IF(IHIT1 .GT. NDIWRK-200-(70*LHIT+LTRREC)) XFOR
C CHECK IF TRACK CONSIDERED FOR REFIT
         IF KOPT.NE.4
         THEN
            LFIT=1
         ELSE
            LFIT=0
            FOR J=1,NTRREQ
               IF ITRREQ(J).EQ.ITR
               THEN
                  LFIT=1
                  XFOR
               CIF
            CFOR
         CIF
         IF LFIT.EQ.1
         THEN
            INDFET = 4
            CALL JFETCH(IPTR,IPJHTL,WRK(IHIT1),LHIT,IPRES,INDFET,XD,YD)
            NHIT=(IPRES-1)/LHIT
            IF NHIT.GT.1
            THEN
C OTHERWISE TRACK IS NOT CONSIDERED FOR REFIT
               IPRES=IHIT1+IPRES-1
C ----------------------------------------------------------------------
C S-Z FIT FOR SINGLE TRACK; MARK USED HITS
C IOPT IS PASSED ONLY TO INDICATE WHETHER SPECIAL BANK IS TO BE
C CREATED (IF IOPT>8)
               CALL ZSRFT1(IPTR,LDTR,IHIT1,IPRES,LHIT,IQUAL,IOPT)
C-----------------------------------------------------------------------
C
C SET SINGLE TRACK FIT FLAG IN   KTSTZS(.,2,.)
               IF KOPT.EQ.4
               THEN
                  IF NTSTZS(1).EQ.1
                  THEN
C  S-Z FIT OF A SINGLE TRACK WAS REQUESTED
                     KTSTZS(1,2,1)=IQUAL
                     RETURN
                  ELSE
                     FOR ISET=1,NSETZS
                        NTSETI=NTSTZS(ISET)
                        FOR JT=1,NTSETI
                     IF(KTSTZS(JT,1,ISET).EQ.ITR)KTSTZS(JT,2,ISET)=IQUAL
                        CFOR
                     CFOR
                  CIF
               ELSE
                  KTSTZS(ITR,2,1)=IQUAL
               CIF
C-----------------------------------------------------------------------
               IF IQUAL.GT.0 .AND. KOPT.NE.1
               THEN
C STORE TRACK FOR SUBSEQUENT COMMON S-Z FIT
                  NTRKS=NTRKS+1
                  IWRK(IDTR2  )=ITR
                  IWRK(IDTR2+1)=IPTR
C
                  IF(KOPT.EQ.2) IQUAL=2
C
                  IWRK(IDTR2+2)=IQUAL
                  IWRK(IDTR2+3)=IHIT1
                  IWRK(IDTR2+4)=IPRES
C
                  IHIT1=IPRES+LTRREC
                  IDTR2=IDTR2+LDTR2
               CIF
            CIF
         CIF
         IPTR=IPTR+LDTR
      CFOR
C
C NO COMMON FIT IS REQUESTED OR POSSIBLE
C
      IF(KOPT.EQ.1.OR.NTRKS.LT.2) RETURN
C
C=======================================================================
C
C     COMMON S-Z FIT TO RUN VERTEX
C
C
      IF KOPT.EQ.2
      THEN
         IRUN=HDATA( 2*IDATA(IQHEAD) + 10)
         IF  IRUN.GE.100
         THEN
            IPV    = ICALIB(10)
            XCOMM  = ACALIB(IPV+ 1)
            YCOMM  = ACALIB(IPV+ 3)
         ELSE
            XCOMM  = 0.
            YCOMM  = 0.
         CIF
         IVNEED=IZVCST(1)
C
         PERFORM COMMZS
C
      CIF
C
C=======================================================================
C
C     COMMON S-Z FIT FOR USER SPECIFIED TRACK SETS
C
C
      IF KOPT.EQ.4
      THEN
         FOR ISET=1,NSETZS
            NTSETI=NTSTZS(ISET)
            NTRFIT=0
            IDTR2=1
            FOR JTR=1,NTRKS
               ITR=IWRK(IDTR2)
               IWRK(IDTR2+2)=1
               FOR JT=1,NTSETI
                  IF KTSTZS(JT,1,ISET).EQ.ITR
                  THEN
                     NTRFIT=NTRFIT+1
                     IWRK(IDTR2+2)=2
                     XFOR
                  CIF
               CFOR
               IDTR2=IDTR2+LDTR2
            CFOR
            IF NTRFIT.GE.2
            THEN
               XCOMM=XYSTZS(1,ISET)
               YCOMM=XYSTZS(2,ISET)
               IVNEED=IZVCST(ISET)
               PERFORM COMMZS
            CIF
         CFOR
      CIF
C
      RETURN
C
C
C=======================================================================
C
C
C  CODE FOR THE COMMON S-Z FIT
C  HITS ARE USED   IFF   MARKED AS USED IN ZSRFT1
C
      PROC COMMZS
C
C STARTING VALUE FOR COMMON Z AND
C CHECK IF TRACK CONSISTENT WITH THE COMMON POINT IN R-PHI
C AND IF Z AT COMMON POINT IS WITHIN LIMITS
C
         ZCUT=ZCUTV
         IF(KOPT.EQ.4) ZCUT=ZCUTVV
         IDTR2=1
         NTRFIT=0
         ZCOMM=0.
         FOR JTR=1,NTRKS
            IF IWRK(IDTR2+2).EQ.2
            THEN
C TRACK WAS REQUESTED
               IPTR=IWRK(IDTR2+1)
C CALCULATE DISTANCE OF COMMON POINT TO CIRCLE IN R-PHI
               CURVXY=ADATA(IPTR+25)
               IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
               DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +         ADATA(IPTR+9),1./CURVXY,XCOMM,YCOMM,CORTRC(1,JTR),
     +         CORTRC(2,JTR),FI)
               IF ABS(DDR0).LT.DIMPCT
               THEN
C CIRCLE CLOSE ENOUGH TO COMMON POINT
C TRACK DIRECTION AT COMMON POINT
                  WRK(IDTR2+5)=COS(FI)
                  WRK(IDTR2+6)=SIN(FI)
C CALCULATE Z OF TRACK AT THE COMMON POINT
                  DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +            ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
                  UU=SQRT((CORTRC(1,JTR)-XP)**2+(CORTRC(2,JTR)-YP)**2)
                  ARGARG = .5*CURVXY*UU
                  IF( ABS(ARGARG).GT.1.) ARGARG = SIGN( 1., ARGARG )
                  IF(ABS(CURVXY*UU).GT.1.E-5)
     +            UU=2.*ARSIN(ARGARG)/CURVXY
CC                IF(ABS(CURVXY*UU).GT.1.E-5)
CC   +            UU=2.*ARSIN(.5*CURVXY*UU)/CURVXY
                  ZCOMM1=ADATA(IPTR+31)+ADATA(IPTR+30)*UU
                  IF ABS(ZCOMM1) .LT. ZCUT
                  THEN
                     WRK(IDTR2+7)=ZCOMM1
                     WRK(IDTR2+10)=CURVXY
                     ZCOMM=ZCOMM+ZCOMM1
                     NTRFIT=NTRFIT+1
                  ELSE
                     IWRK(IDTR2+2)=1
                  CIF
               ELSE
                  IWRK(IDTR2+2)=1
               CIF
            CIF
            IDTR2=IDTR2+LDTR2
         CFOR
C
C STARTING VALUE OF COMMON Z; NO CHECK IF Z OF TRACK CONSISTENT WITH IT
C COLLECT SUMS FOR COMMON Z FIT
         IF NTRFIT.GE.2
         THEN
            ZCOMM=ZCOMM/NTRFIT
COMIT       DZLIM=400.
COMIT       NTRFIT=0
            NHTOT=0
            IF IVNEED.EQ.1
            THEN
C VERTEX CONSTRAINT OF 10 MM ON THE COMMON Z
               S0=(20./10.)**2
               IF( LDATYP(DUMMY).EQ.2 ) S0=S0*4.
               S3=-ZCOMM*S0
               S7=ZCOMM**2*S0
            ELSE
               S0=0.
               S3=0.
               S7=0.
            CIF
            S5=0.
            S6=0.
            IDTR2=1
            FOR JTR=1,NTRKS
               IF IWRK(IDTR2+2).EQ.2
               THEN
C TRACK REQUESTED AND SURVIVED THE R-PHI DISTANCE AND Z CUTS
COMIT             IF ABS(WRK(IDTR2+7)-ZCOMM).LT.DZLIM
COMIT             THEN
C TRACK CLOSE ENOUGH IN Z
COMIT                NTRFIT=NTRFIT+1
                     IPTR=IWRK(IDTR2+1)
                     CURVXY=WRK(IDTR2+10)
                     CTGTH=ADATA(IPTR+30)
                     S1=0.
                     S2=0.
                     S4=0.
C LOOP OVER HITS
                     IPCO =IWRK(IDTR2+3)
                     IPCO9=IWRK(IDTR2+4)-LHIT
                     FOR IP=IPCO,IPCO9,LHIT
                        IF HWRK(2*IP+3).EQ.1
                        THEN
C HIT WAS USED IN THE SINGLE TRACK FIT
C CALCULATE TRACK LENGTH IN R-PHI COUNTED FROM COMMON POINT
                           UX=WRK(IP+3)-CORTRC(1,JTR)
                           UY=WRK(IP+4)-CORTRC(2,JTR)
                           UU=SQRT(UX**2+UY**2)
                           ARGARG = .5*CURVXY*UU
                           IF( ABS(ARGARG).GT.1.) ARGARG=SIGN(1.,ARGARG)
                           IF(ABS(CURVXY*UU).GT.1.E-5)
     +                     UU=2.*ARSIN(ARGARG)/CURVXY
CC                         IF(ABS(CURVXY*UU).GT.1.E-5)
CC   +                     UU=2.*ARSIN(.5*CURVXY*UU)/CURVXY
                           IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.)
     +                     UU=-UU
C RESIDUAL TO LINE WITH START PARAMETERS
                           DZ=WRK(IP+5)-ZCOMM-CTGTH*UU
                           W=WRK(IP+7)
                           NHTOT=NHTOT+1
                           S0=S0+W
                           S3=S3+DZ*W
                           S1=S1+UU*W
                           S2=S2+UU**2*W
                           S4=S4+DZ*UU*W
                           S7=S7+DZ**2*W
                        CIF
                     CFOR
                     WRK(IDTR2+7)=S4
                     WRK(IDTR2+8)=S1
                     WRK(IDTR2+9)=S2
                     S5=S5+S1*S4/S2
                     S6=S6+S1*S1/S2
COMIT             ELSE
COMIT                IWRK(IDTR2+2)=1
COMIT             CIF
               CIF
               IDTR2=IDTR2+LDTR2
            CFOR
C
C RESULTS OF COMMON FIT; FILL 'PATR' BANK
COMIT       IF NTRFIT.GE.2
COMIT       THEN
               DZCOMM=(S3-S5)/(S0-S6)
               ZCOMMR=ZCOMM+DZCOMM
C
               ISETOP=1
               IF(KOPT.EQ.4) ISETOP=ISET
               SQCHZS(2,ISETOP)=NHTOT
               SQCHZS(3,ISETOP)=ZCOMMR
               SQCHZS(1,ISETOP)=S7+DZCOMM*(DZCOMM*S0-2.*S3)
C
               IDTR2=1
               FOR JTR=1,NTRKS
                  IF IWRK(IDTR2+2).EQ.2
                  THEN
                     IPTR=IWRK(IDTR2+1)
                     PERFORM FITBNK
C
C SET SINGLE TRACK FIT FLAG IN   KTSTZS(.,2,.)
                     ITR=IWRK(IDTR2)
                     IF KOPT.EQ.4
                     THEN
                        FOR JT=1,NTSETI
                         IF(KTSTZS(JT,1,ISET).EQ.ITR)KTSTZS(JT,2,ISET)=200061100
                        CFOR
                     ELSE
                        KTSTZS(ITR,2,1)=2
                     CIF
C
                  CIF
                  IDTR2=IDTR2+LDTR2
               CFOR
C
               IF(SQCHZS(1,ISETOP).LT.1.E-5) SQCHZS(1,ISETOP)=1.E-5
               SQCHZS(1,ISETOP)=SQRT(SQCHZS(1,ISETOP)/(NHTOT-NTRFIT-1))
C
COMIT       CIF
         CIF
      CPROC
C=======================================================================
C
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
      DCTGTH=(WRK(IDTR2+7)-DZCOMM*WRK(IDTR2+8))/WRK(IDTR2+9)
C
      SQCHZS(1,ISETOP)=SQCHZS(1,ISETOP)+DCTGTH*(DCTGTH*WRK(IDTR2+9)+
     +2.*DZCOMM*WRK(IDTR2+8)-2.*WRK(IDTR2+7))
C
      CTGTH=ADATA(IPTR+30)+DCTGTH
      CSTH = 1./SQRT(CTGTH**2 + 1.)
      SNTH  = CSTH * CTGTH
      CURVXY=WRK(IDTR2+10)
C
C
C
N     COPY TRACK BANK
         IFREE=NDIWRK-100
         CALL MVCL(IWRK(IFREE),0,IDATA(IPTR+1),0,4*LDTR)
C
N     FILL FIT-BANK
         IP    = IFREE - 1
         IWRK(IP+ 2) = LOR(IWRK(IP+2),8192)
C FIRST POINT ON TRACK
         UX=WRK(IP+5)-CORTRC(1,JTR)
         UY=WRK(IP+6)-CORTRC(2,JTR)
         UU=SQRT(UX**2+UY**2)
         ARGARG = .5*CURVXY*UU
         IF( ABS(ARGARG).GT.1.) ARGARG = SIGN( 1., ARGARG )
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(ARGARG)/CURVXY
CC       IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(.5*CURVXY*UU)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+ 7) = ZCOMMR+CTGTH*UU
         A=SQRT(WRK(IP+8)**2+WRK(IP+9)**2)
         WRK (IP+ 8) = WRK (IP+ 8)/A*CSTH
         WRK (IP+ 9) = WRK (IP+ 9)/A*CSTH
         WRK (IP+10) = SNTH
C LAST POINT ON TRACK
         UX=WRK(IP+12)-CORTRC(1,JTR)
         UY=WRK(IP+13)-CORTRC(2,JTR)
         UU=SQRT(UX**2+UY**2)
         ARGARG = .5*CURVXY*UU
         IF( ABS(ARGARG).GT.1.) ARGARG = SIGN( 1., ARGARG )
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(ARGARG)/CURVXY
CC       IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(.5*CURVXY*UU)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+14) = ZCOMMR+CTGTH*UU
         A=SQRT(WRK(IP+15)**2+WRK(IP+16)**2)
         WRK (IP+15) = WRK (IP+15)/A*CSTH
         WRK (IP+16) = WRK (IP+16)/A*CSTH
         WRK (IP+17) = SNTH
C STORE COMMON FIT PARAMETERS
         IWRK(IP+29) = 2
         WRK (IP+30) = CTGTH
C GET CLOSEST POINT (XP,YP) TO ORIGIN
         DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +   ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
C CALCULATE TRACK LENGTH ALONG CIRCLE FROM FIRST POINT TO (XP,YP)
         UX=XP-CORTRC(1,JTR)
         UY=YP-CORTRC(2,JTR)
         UU=SQRT(UX**2+UY**2)
         ARGARG = .5*CURVXY*UU
         IF( ABS(ARGARG).GT.1.) ARGARG = SIGN( 1., ARGARG )
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(ARGARG)/CURVXY
CC       IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(.5*CURVXY*UU)/CURVXY
         IF(UX*WRK(IDTR2+5)+UY*WRK(IDTR2+6).LT.0.) UU=-UU
         WRK (IP+31) = ZCOMMR+CTGTH*UU
N     PUT RESULT INTO PATR-BANK
         CALL MVCL(IDATA(IPTR+1),0,IWRK(IFREE),0,4*LDTR)
      CPROC
      END
C   09/06/83 805241835  MEMBER NAME  ZSRFT1   (JADEGS)      SHELTRAN
C   09/06/83 803181242  MEMBER NAME  ZSRFT1   (S)           SHELTRAN
      SUBROUTINE ZSRFT1(IPTR,LDTR,IPCO0,IPRES,LHIT,IQUAL,IOPT)
C
C        S-Z ("HELIX") REFIT OF A SINGLE TRACK
C
C    20.5.88 WEIGHT RENORMAL. AFTER ALL ITERATIONS        J.H./E.E.
C            MC USE ZRS AS RESOLUTION                     J.H./E.E.
C    18.5.88 SMOOTH CUTOFF PROCEDURE FOR LSTOP            J.H./E.E.
C    13.5.88 (FRIDAY!) SEVERE BUG CORRECTED IN TWO PLACES J.H./J.O.
C            ARSIN INSTEAD OF SIN
C            IN ADDITION MISPRINT XP CHANGED INTO YP
C    18.3.88   PROPER RUN NUMBER HANDLING USING LDATYP      E E
C    22.2.88   MVC CHANGED TO MVCL (256 BYTES NOT ENOUGH!)  J.H./J.O.
C    TEST VERSION 1.
C
C                                J. SPITZER  12/4/87
C
C    COVARIANCE MATRIX FOR FIT PARAMETERS IF AREA (LDTR) LARGE ENOUGH
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL LIDHIT
C
#include "cdata.for"
#include "calibr.for"
C
      COMMON/CWORK/NDIWRK,WRK(200)
      DIMENSION IWRK(200),HWRK(400)
      EQUIVALENCE (WRK(1),IWRK(1),HWRK(1))
C
      COMMON / CBIN   / TIME(6),ZOF,ZRS,ZL,ZSC,EPSI(3),DOUB(3),IRN(3),
     +                BINDL8(6),RJITT, DLRSLN(3), DLZSLN(3)
C
      INTEGER DATE(5), IDAY /0/
C
      REAL RESCUT/600./
      DIMENSION ISORT1(71),ISORT2(3,71),IRESHT(71),ISORT3(91)
     +,ISORT4(2,91),KSORT3(91),KSZSRT(91,2)
      DATA IQHEAD/0/,MASK4/ZFFFFCFFF/
C
C
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
         LBINIT = 1
C
         IQHEAD = IBLN('HEAD')
C
         CALL DAY2(DATE)
         IDAY = DATE(1)*1000 + DATE(2)
C
         WRITE(6,137)
 137     FORMAT(/,' *** ZSRFT1 ***  (J.SPITZER) VERSION OF 20/5/88',
     +   ' SMOOTH CUTOFF PROCEDURE IMPLEMENTED')
      CIF
C
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      NEV  = HDATA(IPHEAD+11)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
C=======================================================================
      IF LDATYP(DUMMY) .EQ. 1
      THEN
         SRESO=24.
         STPFAC=.85
      ELSE
         SRESO=32.
         STPFAC=.92
      CIF
      IF( NRUN.LT.100 ) SRESO = SQRT(ZRS**2+1.)
      SIGMIN=(SRESO/1.6)**2
      SIGFAC=(.14/30.)**2
C
C
C-----------------------------------------------------------------------
C
C     GYMNASTICS FOR PRIVATE HIT QUALIFICATION
C     AND FOR HANDLING MORE HITS ON SAME WIRE
C
C  STARTING VALUES OF FIT PARAMETERS Z=P1*S+P2
C
      PAR1=ADATA(IPTR+30)
      PAR2=ADATA(IPTR+7)
C
      NHALL=0
      NHWIR=0
      NHPOT=0
      NHPOTT=0
      IPCO=IPCO0
      IPCO9=IPRES-LHIT
      REPEAT
         NHWIR=NHWIR+1
         IF NHWIR.GT.70
         THEN
            IQUAL=-1
            RETURN
         CIF
         ISORT1(NHWIR)=NHWIR
         ISORT2(1,NHWIR)=IPCO
         ISORT2(3,NHWIR)=0
         IW0=IWRK(IPCO)
         LFL=0
         LFL1=0
         LFL2=0
         WHILE IPCO.LE.IPCO9
            IW9=IWRK(IPCO)
            IF IW9.EQ.IW0
            THEN
N        HIT ON THE SAME WIRE
               NHALL=NHALL+1
               IF NHALL.GT.90
               THEN
                  IQUAL=-2
                  RETURN
               CIF
               IF(ISORT2(3,NHWIR).EQ.0) ISORT2(2,NHWIR)=NHALL
               ISORT2(3,NHWIR)=ISORT2(3,NHWIR)+1
C
               KSORT3(NHALL)= NHALL
               LZGOOD=HWRK(2*IPCO+1)
               IF LZGOOD.NE.0
               THEN
                  ISORT3(NHALL)=-1
                  KSZSRT(NHALL,1)= 100000
               ELSE
                  ISORT3(NHALL)= 1
                  IF LFL2.EQ.0
                  THEN
                     KSZSRT(NHALL,1)= WRK(IPCO+6)
                     KSZSRT(NHALL,2)= WRK(IPCO+5)
                     LFL2=1
                  ELSE
                     KSZSRT(NHALL,1)= 100000
                  CIF
                  LFL=1
                  IF ABS(WRK(IPCO+5)-PAR1*WRK(IPCO+6)-PAR2).LT.RESCUT
                  THEN
                     IF LFL1.EQ.0
                     THEN
                        LFL1=1
                        NHPOTT=NHPOTT+1
                     CIF
                  CIF
               CIF
C
               IPCO=IPCO+LHIT
            ELSE
               XWHILE
            CIF
         CWHILE
         IF LFL.EQ.1
         THEN
            NHPOT=NHPOT+1
         ELSE
            ISORT3(ISORT2(2,NHWIR))=-2
         CIF
      UNTIL IPCO.GT.IPCO9
C-----------------------------------------------------------------------
C
C IF LESS THAN 2 WIRES WITH GOOD Z MEASUREMENT, NOTHING DONE
      IF NHPOT.LT.2
      THEN
         IQUAL=-3
         RETURN
      CIF
C-----------------------------------------------------------------------
      KFLIP=2
C
      NHFIT=NHPOTT
      LFOUND=-1
      IF NHPOTT.LT.6.OR.NHPOTT.LT.NHPOT*.75
      THEN
C TRY TO FIND BETTER START VALUES
         PERFORM STVSEA
      CIF
C-----------------------------------------------------------------------
C
      INDMAX=NHFIT/4+1
      IF(INDMAX.GT.13) INDMAX=13
      INDFIT=0
      WHILE INDFIT.LT.INDMAX
         INDFIT=INDFIT+1
N    LINEAR FIT
         PERFORM LINFIT
         IF LNOCON.EQ.1
         THEN
C NO CONVERGENCE AS INDICATED BY LOSS OF TOO MANY HITS
            IQUAL=-4
            RETURN
         CIF
         IF(SIG.LT.SIGMIN) XWHILE
         IF INDFIT.GE.2
         THEN
            PERFORM LLSTOP
            IF LSTOP.EQ.1
            THEN
C      PREVIOUS FIT ACCEPTED, RESTORE ITS RESULTS
               INDFIT=INDFIT-1
               KFLIP=3-KFLIP
               NHFIT=NHFTLS
               PAR1=PAR1LS
               PAR2=PAR2LS
               SIG=SIGLST
               S0=S0LS
               S1=S1LS
               S2=S2LS
               S3=S3LS
               S4=S4LS
               SHID = SHIDLS
               S0ID = S0IDLS
               XWHILE
            CIF
         CIF
         IF(INDFIT.EQ.INDMAX.OR.NHFIT.EQ.2) XWHILE
N      SAVE FIT RESULTS
         NHFTLS=NHFIT
         PAR1LS=PAR1
         PAR2LS=PAR2
         SIGLST=SIG
         S0LS=S0
         S1LS=S1
         S2LS=S2
         S3LS=S3
         S4LS=S4
         SHIDLS = SHID
         S0IDLS = S0ID
N      HIT CLEANING
         PERFORM HITCLN
      CWHILE
C
C
N     SET UP PATR-BANK
      PERFORM FITBNK
      IQUAL=1
      RETURN
C=======================================================================
C
N     *************************
N     *      L I N F I T      *
N     *************************
C
C
N      LINEAR FIT
      PROC LINFIT
C
      LNOCON=0
N     GET EQUATIONS
      KFLIP=3-KFLIP
      NHF1=0
      SHID = 0.
      S0ID = 0.
      S0 = 0.
      S1 = 0.
      S2 = 0.
      S3 = 0.
      S4 = 0.
      FOR IHWIR=1,NHWIR
         IH=ISORT2(2,IHWIR)
         NNH=ISORT2(3,IHWIR)
         FOR JNH=1,NNH
            ISORT4(KFLIP,IH+JNH-1)=0
         CFOR
         IF ISORT3(IH).EQ.1 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
         THEN
            RESMIN=10000.
            LIDHIT = .TRUE.
            FOR JNH=1,NNH
               JH=IH+JNH-1
               IF ISORT3(JH).EQ.1
               THEN
                  IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                  SA = WRK(IPCO+6)
                  ZA = WRK(IPCO+5)
                  WA = WRK(IPCO+7)
                  DZRESA=ZA-PAR1*SA-PAR2
                  DF0=ABS(DZRESA)
                  IF DF0.LT.RESMIN
                  THEN
                     RESMIN=DF0
                     S=SA
                     W=WA
                     DZRES=DZRESA
                     JHUSE=JH
                     LIDHIT = HWRK(IPCO*2-1).LT.97
                  CIF
               CIF
            CFOR
            IF RESMIN.LT.RESCUT
            THEN
               NHF1=NHF1+1
               S0=S0+W
               S1=S1+S*W
               S2=S2+S**2*W
               S3=S3+DZRES*W
               S4=S4+DZRES*S*W
               IF LIDHIT
               THEN
                  SHID = SHID + 1.
                  S0ID = S0ID + W
               CIF
               ISORT4(KFLIP,JHUSE)=1
            ELSE
               ISORT3(IH)=-2
            CIF
         CIF
      CFOR
      IF NHF1.LT.2 .OR. S2.LT.1.
      THEN
         LNOCON=1
      ELSE
         NHFIT=NHF1
C
N        SOLVE EQUATIONS
         F1 = 1. / S2
         XX12 = S1*F1
         YY1  = S4*F1
         PARR2=(S3-S1*YY1)/(S0-S1*XX12)
         PAR1=YY1-PARR2*XX12+PAR1
         PAR2=PAR2+PARR2
C
N     CALC. CHISQ + SOLVE L/R AMBIGUITY
         CHISQ = 0.
         NHF1=0
         FOR IHWIR=1,NHWIR
            IRESHT(IHWIR)=-1
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            IF ISORT3(IH).GE.0 .OR. ISORT3(IH).EQ.-1.AND.NNH.GT.1
            THEN
               RESMIN=10000.
               FOR JNH=1,NNH
                  JH=IH+JNH-1
                  IF ISORT3(JH).GE.0
                  THEN
                     IFLG=ISORT3(JH)
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                     SA = WRK(IPCO+6)
                     ZA = WRK(IPCO+5)
                     WA = WRK(IPCO+7)
                     DF0=ABS(ZA-PAR1*SA-PAR2)
                     IF DF0.LT.RESMIN
                     THEN
                        RESMIN=DF0
                        W=WA
                     CIF
                  CIF
               CFOR
               IF RESMIN.LT.8000.
               THEN
                  RESMIN=RESMIN*SQRT(W)
                  IRESHT(IHWIR)=RESMIN*1.E4
                  IF IFLG.EQ.1
                  THEN
                     CHISQ=CHISQ+RESMIN**2
                     NHF1=NHF1+1
                  CIF
               CIF
            CIF
         CFOR
         IF NHF1.LT.2
         THEN
            LNOCON=1
         ELSE
            IF NHF1.EQ.2
            THEN
               SIG=1.E-5
            ELSE
               SIG=CHISQ/(NHF1-2)
            CIF
         CIF
         NHFIT=NHF1
      CIF
      CPROC
C=======================================================================
      PROC HITCLN
C      LABEL HITS NOT TO BE USED IN THE NEXT ITRATION
C-------------------------------------------------------------
C
N       SORT HITS ACCORDING TO RESIDUALS
C  EXCLUDE THE INDFIT LARGEST RESIDUAL HITS,
C  RESTORE THE OTHERS (EXLUDED FOR EVER HITS NOT COUNTED)
C
         CALL SHELL9(IRESHT,ISORT1,NHWIR)
         KOMIT=0
         FOR J1=1,NHWIR
            IHWIR=ISORT1(NHWIR+1-J1)
            IPCO=ISORT2(1,IHWIR)
            NNH=ISORT2(3,IHWIR)
            IH=ISORT2(2,IHWIR)
            LFLG=0
            FOR JNH=1,NNH
               IHA=IH+JNH-1
               IQA=ISORT3(IHA)
               IF IQA.GT.-1
               THEN
                  IF LFLG.EQ.0
                  THEN
                     LFLG=1
                     KOMIT=KOMIT+1
                  CIF
                  IF KOMIT.LE.INDFIT
                  THEN
                     ISORT3(IHA)=0
                  ELSE
                     ISORT3(IHA)=1
                  CIF
               CIF
            CFOR
         CFOR
      CPROC
C=======================================================================
      PROC LLSTOP
         SIGCHK = SIGLST*SIGFAC
         IF SIGCHK .LT. 0.002
         THEN
            RCHKPR = 0.0
         ELSE
            RCHKPR = 1.139 + 0.08862*ALOG(SIGCHK)
         CIF
         IF SIG/SIGLST*STPFAC.GT.RCHKPR
         THEN
            LSTOP=1
         ELSE
            LSTOP=0
         CIF
      CPROC
C=======================================================================
C
C
N     *************************
N     *      F I T B N K      *
N     *************************
C
C
N     SET UP FIT-BANK
      PROC FITBNK
C
      CTGTH=PAR1
      CSTH = 1./SQRT(CTGTH**2 + 1.)
      SNTH  = CSTH * CTGTH
C
C
N     NORMALIZE SIG FOR WEIGHT EQUAL 1
      IF( S0ID.GT.0. ) SIG = SIG*SHID/S0ID
N     COPY TRACK BANK
         IFREE=NDIWRK-100
         CALL MVCL(IWRK(IFREE),0,IDATA(IPTR+1),0,4*LDTR)
C
N     FILL FIT-BANK
         IP    = IFREE - 1
         IWRK(IP+ 2) = LAND(IWRK(IP+2),MASK4)
         IWRK(IP+ 2) = LOR(IWRK(IP+2),4096)
         WRK (IP+ 7) = PAR2
         A=SQRT(WRK(IP+8)**2+WRK(IP+9)**2)
         WRK (IP+ 8) = WRK (IP+ 8)/A*CSTH
         WRK (IP+ 9) = WRK (IP+ 9)/A*CSTH
         WRK (IP+10) = SNTH
C CALCULATE TRACK LENGTH IN R-PHI FROM FIRST TO LAST POINT ON TRACK
         CURVXY=WRK(IP+25)
         IF(ABS(CURVXY).LT.1.E-9) CURVXY = SIGN(1.E-9,CURVXY)
         UU=SQRT((WRK(IP+12)-WRK(IP+5))**2+(WRK(IP+13)-WRK(IP+6))**2)
         ARGARG = .5*CURVXY*UU
         IF(ABS(ARGARG).GT.1.) ARGARG = SIGN(1.,ARGARG)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(ARGARG)/CURVXY
C        IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(.5*CURVXY*UU)/CURVXY
         WRK (IP+14) = PAR2+UU*PAR1
         A=SQRT(WRK(IP+15)**2+WRK(IP+16)**2)
         WRK (IP+15) = WRK (IP+15)/A*CSTH
         WRK (IP+16) = WRK (IP+16)/A*CSTH
         WRK (IP+17) = SNTH
         IWRK(IP+33) = NHFIT
         WRK (IP+32) = SQRT(SIG)
C FIT TYPE WILL BE 2: "HELIX FIT"
         IWRK(IP+29) = 2
         WRK (IP+30) = PAR1
C GET CLOSEST POINT (XP,YP) TO ORIGIN
         DDR0=DISTXY(ADATA(IPTR+5),ADATA(IPTR+6),ADATA(IPTR+8),
     +   ADATA(IPTR+9),1./CURVXY,0.,0.,XP,YP,FI)
C CALCULATE TRACK LENGTH ALONG CIRCLE FROM FIRST POINT TO (XP,YP)
         UU=SQRT((XP-WRK(IP+5))**2+(YP-WRK(IP+6))**2)
         ARGARG = .5*CURVXY*UU
         IF(ABS(ARGARG).GT.1.) ARGARG = SIGN(1.,ARGARG)
         IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(ARGARG)/CURVXY
C        IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*ARSIN(.5*CURVXY*UU)/CURVXY
         WRK (IP+31) = PAR2-PAR1*UU
C
         IF LDTR.GE.59 .AND. NHFIT.GE.4 .AND. LNOCON.EQ.0
         THEN
C CALCULATE COVARIANCE MATRIX
            DET=S0*S2-S1**2
            FACT=SIG/DET
            WRK(IP+56)=SIG*(NHFIT-2)/20.**2
            WRK(IP+57)=(S2+2.*UU*S1+UU**2*S0)*FACT
            WRK(IP+58)=-(UU*S0+S1)*FACT
            WRK(IP+59)=S0*FACT
         CIF
C
C MARK HITS USED IN THE FIT
         LFL=0
         FOR IHWIR=1,NHWIR
            IH=ISORT2(2,IHWIR)
            NNH=ISORT2(3,IHWIR)
            FOR JNH=1,NNH
               IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
               HWRK(2*IPCO+3)=ISORT4(KFLIP,IH+JNH-1)
               IF(HWRK(2*IPCO-1).GT.100.AND.HWRK(2*IPCO+3).EQ.1) LFL=1
            CFOR
         CFOR
         IF(LFL.EQ.1) IWRK(IP+ 2) = LOR(IWRK(IP+2),16384)
C
N     PUT RESULT INTO PATR-BANK
         CALL MVCL(IDATA(IPTR+1),0,IWRK(IFREE),0,4*LDTR)
C
C CREATE Z-S BANK 'ZSPD'
C
         IF IOPT.GT.8
         THEN
            CALL CCRE(NPZSPD,'ZSPD',ITRK,5*NHALL+1,IERR)
            IF IERR.EQ.0
            THEN
               CALL BSAW(1,'ZSPD')
               NPZSP1=NPZSPD+1
               IDATA(NPZSP1)=5
               FOR IHWIR=1,NHWIR
                  IH=ISORT2(2,IHWIR)
                  NNH=ISORT2(3,IHWIR)
                  FOR JNH=1,NNH
                     IPCO=ISORT2(1,IHWIR)+(JNH-1)*LHIT
                     IFL=ISORT4(KFLIP,IH+JNH-1)
                     IF IFL.EQ.1
                     THEN
                        IFL=0
                     ELSE
                        IFL=16
                     CIF
                     IDATA(NPZSP1+1)=HWRK(2*IPCO+4)
                     ADATA(NPZSP1+2)=WRK(IPCO+6)
                     ADATA(NPZSP1+3)=WRK(IPCO+5)
                     IDATA(NPZSP1+4)=IFL
                     ADATA(NPZSP1+5)=WRK(IPCO+7)
                     NPZSP1=NPZSP1+5
                  CFOR
               CFOR
            CIF
         CIF
      CPROC
C=======================================================================
      PROC STVSEA
C  SEARCH FOR STARTING VALUES
C
C  ORDER ACCORDING TO S
         CALL SHELL9(KSZSRT(1,1),KSORT3,NHALL)
         NH9=NHALL
         WHILE KSZSRT(KSORT3(NH9),1).GT.99999
            NH9=NH9-1
         CWHILE
         NH99=NH9
         NH1=1
         LFOUND=0
         NHMAX=0
         CI2MAX=1.E10
         WHILE NH9.GT.NH1
            FOR KK=1,3
               IF KK.EQ.1
               THEN
                  JH1=NH1
                  JH9=NH9
               ELSE
                  IF KK.EQ.2
                  THEN
                     JH9=JH9-1
                  ELSE
                     JH9=JH9+1
                     JH1=JH1+1
                  CIF
               CIF
               IF JH9.GT.JH1
               THEN
                  AS1=KSZSRT(KSORT3(JH1),1)
                  AS9=KSZSRT(KSORT3(JH9),1)
                  IF(AS9.LT.AS1+5.) XWHILE
                  AZ1=KSZSRT(KSORT3(JH1),2)
                  AZ9=KSZSRT(KSORT3(JH9),2)
                  PAR1=(AZ9-AZ1)/(AS9-AS1)
                  PAR2=AZ1-PAR1*AS1
                  NHFIT=0
                  CI2=0.
                  FOR I=1,NH99
                     DZ=ABS(KSZSRT(KSORT3(I),2)-PAR1*KSZSRT(KSORT3(I),1)
     +               -PAR2)
                     IF DZ.LT.RESCUT
                     THEN
                        NHFIT=NHFIT+1
                        CI2=CI2+DZ**2
                     CIF
                  CFOR
                  IF NHFIT.GT.NHPOT*.75
                  THEN
                     LFOUND=1
                     XWHILE
                  CIF
                  IF NHFIT.GT.NHMAX .OR.NHFIT.EQ.NHMAX.AND.CI2.LT.CI2MAX
                  THEN
                     NHMAX=NHFIT
                     P1MAX=PAR1
                     P2MAX=PAR2
                     CI2MAX=CI2
                  CIF
               CIF
            CFOR
            NH1=NH1+1
            NH9=NH9-1
         CWHILE
         IF LFOUND.EQ.0 .AND. NHMAX.GT.0
         THEN
            NHFIT=NHMAX
            PAR1=P1MAX
            PAR2=P2MAX
         CIF
      CPROC
      END
C   03/03/80 106101800  MEMBER NAME  ZVERTF   (JADEGS)      SHELTRAN
C   12/11/79 911141755  MEMBER NAME  OLZVERTF (JADESR)      SHELTRAN
C   04/07/79 C9070601   MEMBER NAME  ZVERTY   (JADESR)      SHELTRAN
C   04/07/79 C9070401   MEMBER NAME  ZVERTZ   (UKSOR)       SHELTRAN
C   03/07/79 C9070401   MEMBER NAME  ZVERTY   (JADESR)      SHELTRAN
C   29/06/79 C9070201   MEMBER NAME  ZVERTF   (JADEGS)      SHELTRAN
C   28/03/79 C9062901   MEMBER NAME  ORZVERTF (JADESR)      SHELTRAN
C   13/03/79 C9032801   MEMBER NAME  ORZVERTF (JADESR)      SHELTRAN
C   07/03/79 C9031301   MEMBER NAME  ZVERTF8  (JADESR)      SHELTRAN
      SUBROUTINE ZVERTF
C
C     CALCULATION OF Z-VERTEX: P.STEFFEN(78/11/15)
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
#include "cgraph.for"
#include "czvpar.for"
#include "cjdrch.for"
#include "cdsmax.for"
#include "clbpgm.for"
C
#include "cworkzv.for"
C
C
N     SET PROGRAM LABEL
      LBPGM(2) = LBPGM(2) + 1
C
      DATA MKLAYR /15/
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT.EQ.0
      THEN
        LBINIT = 1
        IQJETC = IBLN('JETC')
N       COMMON FACTOR FOR Z(VERT.) CALC.
        IZCON = ZAL*.5
        IZMAX = ZMAX + 40.
      CIF
C
N       HISTOGRAM PARAMETERS
C
N     SET UP CYCLIC POINTER ARRAY
N     ADDRESS OF POINTERS TO CELLS (-1)
      IPJCA  = IDATA(IQJETC)
      IPJCA2 = IPJCA*2 + 2
C
N     COPY CELL POINTERS + CALC. LENGTH
      IP0 = IPJCA2 + 98
      ISEGO = IPJCA2
      FOR ISEG=1,96
        ISEGO = ISEGO + 1
        HPTSEC(ISEG) = HDATA(ISEGO) + IP0
      CFOR
      HPTSEC(97) = HDATA(ISEGO+1) + IP0
      HPTSEC(98) = 0
C
      FZRSLT(1) = 0.
      FZRSLT(2) = 1000000.
      FZRSLT(3) = 1000000.
      FZRSLT(4) = 0.
      FZRSLT(5) = 0.
      IZRSLT(6) =-2
      INDLB = 3
C
N     DET. ZVTX FROM R1 + R2
      DFIMX0 = DFIMAX
      PERFORM ZVTX12
C
N     CHECK IF NO GOOD VERTEX FOUND
      IF IZRSLT(6).LT.3
      THEN
N       NO GOOD VERTEX FOUND, SAVE RESULTS
        IZRSLT( 7) = IZRSLT( 1)
        IZRSLT( 8) = IZRSLT( 2)
        IZRSLT( 9) = IZRSLT( 3)
        IZRSLT(10) = IZRSLT( 4)
        IZRSLT(11) = IZRSLT( 5)
        IZRSLT(12) = IZRSLT( 6)
N       TRY ZVTX FROM R2 + R3
        DFIMX0 = 1000.
        PERFORM ZVTX23
N       CHECK IF WORSE VERTEX
        IF IZRSLT(6).LE.IZRSLT(12)
        THEN
N         RESTORE OLD RESULTS
          IZRSLT( 1) = IZRSLT( 7)
          IZRSLT( 2) = IZRSLT( 8)
          IZRSLT( 3) = IZRSLT( 9)
          IZRSLT( 4) = IZRSLT(10)
          IZRSLT( 5) = IZRSLT(11)
          IZRSLT( 6) = IZRSLT(12)
        CIF
      CIF
C
N     SET BOS BANK OF RESULTS
      NWRES = 6
      CALL CCRE(IPHT,'ZVTX',0,NWRES,IERR)
      IF(IERR.NE.0) RETURN
      CALL BSAW(1,'ZVTX')
         ADATA(IPHT+1) = FZRSLT(1)
         ADATA(IPHT+2) = FZRSLT(2)
         ADATA(IPHT+3) = FZRSLT(3)
         ADATA(IPHT+4) = FZRSLT(4)
         ADATA(IPHT+5) = FZRSLT(5)
         ADATA(IPHT+6) = FZRSLT(6)
C
N     SET STOP LABEL
C---  PSTPS(INDLB) = .TRUE.
C
      RETURN
C
N     *************************
N     *      Z V T X 1 2      *
N     *************************
N     DETERMINE ZVTX FROM R1 + R2
C
      PROC ZVTX12
C
      BINZIV = 1. / BINZ
      PAR1 = FSENSW(1) / (FSENSW(2) - FSENSW(1))
      PAR2 =  RINCR(1) / (FSENSW(2) - FSENSW(1))
      HUFLO = 0
      HOFLO = 0
      MAXZ = 0
      NBACK = 0
N     ZERO HISTOGRAM
      CALL SETS(HIST(1),0,200,0)
C
N     CHECK IF AT LEAST 10 HITS IN 1. RING
      IF HPTSEC(25)-HPTSEC(1) .LT. NWRDR1
      THEN
        IZRSLT(6) = -2
        INDLB = 1
      ELSE
C
N
N     LOOP OVER ALLSECTOR(RING1): JSEC = SECTOR NUMBER + 1
      JSEC=1
      REPEAT
N       # OF WORDS IN SECT
        NWRD1 = HPTSEC(JSEC+1) - HPTSEC(JSEC)
        NWRD2 = HPTSEC(JSEC+25) - HPTSEC(JSEC+24)
N       CHECK IF MORE THAN 1 HIT
        IF NWRD1.GE.LWRDC0
        THEN
N         # OF WORDS IN LEFT ADJ SECT
          IF JSEC.EQ.1
          THEN
            NWRD1L= HPTSEC(25) - HPTSEC(24)
            NWRD2L= HPTSEC(49) - HPTSEC(48)
          ELSE
            NWRD1L= HPTSEC(JSEC   ) - HPTSEC(JSEC- 1)
            NWRD2L= HPTSEC(JSEC+24) - HPTSEC(JSEC+23)
          CIF
N         # OF WORDS IN RGHT ADJ SECT
          IF JSEC.EQ.24
          THEN
            NWRD1R= HPTSEC( 2) - HPTSEC( 1)
            NWRD2R= HPTSEC(26) - HPTSEC(25)
          ELSE
            NWRD1R= HPTSEC(JSEC+ 2) - HPTSEC(JSEC+ 1)
            NWRD2R= HPTSEC(JSEC+26) - HPTSEC(JSEC+25)
          CIF
N         CHECK IF MORE THAN 12 HITS
          IF NWRD1+NWRD1L.GE.LWRDC1 .OR. NWRD1+NWRD1R.GE.LWRDC1
          THEN
N           CHECK IF .GT. 10 HITS IN CORRSP SECT
            IF NWRD2L+NWRD2.GE.LWRDC2 .OR. NWRD2R+NWRD2.GE.LWRDC2
            THEN
N             COLLECT ALL Z IN RING 1 + 2
              PERFORM ZCOL12
N             FILL HISTOGRAM
              PERFORM HSTFLL
            CIF
          CIF
        CIF
      JSEC = JSEC + 1
      UNTIL JSEC.GT.24
C
      CALL MVC(HTMP(1),0,HIST(1),0,200)
      PERFORM HEVAL
C
      CIF
      CPROC
C
N     *************************
N     *      Z C O L 1 2      *
N     *************************
C
N     ****  COLLECT ALL Z IN RING 1 + 2  *****
      PROC ZCOL12
C
N       ZERO HIT COUNTERS FOR EACH LAYER
        FOR ILAYR=1,16
          NZ1(ILAYR) = 0
          NZ2(ILAYR) = 0
        CFOR
N       DRIFT SPACE BINS
        DSBIN1 = TIMDEL(1,1)
        DSBIN2 = TIMDEL(2,1)
N       SET POINTERS OF SC1
        IPT0 = HPTSEC(JSEC)
        IPT9 = HPTSEC(JSEC+1) - 1
N       LOOP OVER ALL HITS IN SC1
        FOR IPT = IPT0,IPT9,4
N         SET WIRE #
          IWIRE = HDATA(IPT)
          IWIRE = SHFTR(IWIRE,3)
          ILAYR = LAND(IWIRE,MKLAYR) + 1
N         STORE HIT
          IAMPL = HDATA(IPT+1)
          IAMPR = HDATA(IPT+2)
          IF IAMPL.GT.0 .AND. IAMPR.GT.0
          THEN
            IZ1 = (IZCON * (IAMPR-IAMPL)) / (IAMPR+IAMPL)
N           CHECK IF Z INSIDE ID
            IF IABS(IZ1).LT.IZMAX
            THEN
              NZ1(ILAYR) = NZ1(ILAYR) + 1
              IHIT = NZ1(ILAYR)
              HZ1(IHIT,ILAYR) = IZ1
N             FI ANGLE
              IF(ILAYR.GT.8) DSBIN1 = DSBIN2
              DRLAY = (ILAYR-1) * RINCR(1)
              FI1(IHIT,ILAYR) = HDATA(IPT+3)*DSBIN1 / (FSENSW(1)+DRLAY)
            CIF
          CIF
        CFOR
N       LOOP OVER ADJACENT SECTORS
        JSECA=1
        REPEAT
          JSEC2 = JSEC + JSECA + 22
          IF(JSEC2.LT.25) JSEC2 = 48
          IF(JSEC2.GT.48) JSEC2 = 25
N         SET POINTERS OF SC2
          IPT0 = HPTSEC(JSEC2)
          IPT9 = HPTSEC(JSEC2+1) - 1
          IF IPT9.GT.IPT0
          THEN
N           DRIFT SPACE BINS
            DSBIN1 = TIMDEL(1,2)
            DSBIN2 = TIMDEL(2,2)
N           LOOP OVER ALL HITS IN SC1
            FOR IPT = IPT0,IPT9,4
N             SET WIRE #
              IWIRE = HDATA(IPT)
              IWIRE = SHFTR(IWIRE,3)
              ILAYR = LAND(IWIRE,MKLAYR) + 1
N             STORE HIT
              IAMPL = HDATA(IPT+1)
              IAMPR = HDATA(IPT+2)
              IF IAMPL.GT.0 .AND. IAMPR.GT.0
              THEN
                IZ2 = (IZCON * (IAMPR-IAMPL)) / (IAMPR+IAMPL)
N               CHECK IF Z INSIDE ID
                IF IABS(IZ2).LT.IZMAX
                THEN
                  NZ2(ILAYR) = NZ2(ILAYR) + 1
                  IHIT = NZ2(ILAYR)
                  HZ2(IHIT,ILAYR) = IZ2
N                 FI ANGLE
                  IF(ILAYR.GT.8) DSBIN1 = DSBIN2
                  DS = HDATA(IPT+3)*DSBIN1
                  IF(JSECA.NE.2)
     ?                     DS = DSMAX(ILAYR,2,1)+DSMAX(ILAYR,2,2) - DS
                  DRLAY = (ILAYR-1) * RINCR(2)
                  FI2(IHIT,ILAYR) = DS / (FSENSW(2)+DRLAY)
                CIF
              CIF
            CFOR
          CIF
        JSECA = JSECA + 1
        UNTIL JSECA.GT.3
C
      CPROC
C
N     *************************
N     *      Z V T X 2 3      *
N     *************************
C
N     DETERMINE ZVTX FROM R2 + R3
      PROC ZVTX23
C
N     INITIALIZE HISTOGRAM
      BINZIV = .5 / BINZ
      PAR1 = FSENSW(2) / (FSENSW(3) - FSENSW(2))
      PAR2 =  RINCR(2) / (FSENSW(3) - FSENSW(2))
      HUFLO = 0
      HOFLO = 0
      MAXZ = 0
      NBACK = 0
N     ZERO HISTOGRAM
      CALL SETS (HIST(1),0,200,0)
      CALL SETSL(FI1(1,1),0,1024,0)
C
N     CHECK IF AT LEAST 10 HITS IN 3. RING
      IF HPTSEC(97)-HPTSEC(49) .LT. NWRDR1
      THEN
        IZRSLT(6) = -2
        INDLB = 1
      ELSE
C
N
N     LOOP OVER ALL SECTORS(RING1): ICLL = SECTOR NUMBER + 1
      ICLL=49
      REPEAT
N       # OF WORDS IN SECT
        NWRD1 = HPTSEC(ICLL+1) - HPTSEC(ICLL)
N       CHECK IF MORE THAN 1 HIT
        IF NWRD1.GE.LWRDC0
        THEN
N         # OF WORDS IN LEFT ADJ SECT
          IF ICLL.EQ.49
          THEN
            NWRD1L= HPTSEC(97) - HPTSEC(96)
          ELSE
            NWRD1L= HPTSEC(ICLL   ) - HPTSEC(ICLL- 1)
          CIF
N         # OF WORDS IN RGHT ADJ SECT
          IF ICLL.EQ.96
          THEN
            NWRD1R= HPTSEC(50) - HPTSEC(49)
          ELSE
            NWRD1R= HPTSEC(ICLL+ 2) - HPTSEC(ICLL+ 1)
          CIF
N         CHECK IF MORE THAN 12 HITS
          IF NWRD1+NWRD1L.GE.LWRDC1 .OR. NWRD1+NWRD1R.GE.LWRDC1
          THEN
N           CHECK IF .GT. 10 HITS IN CORRSP SECT
            IF ICLL.EQ.49 .OR. ICLL.EQ.96
            THEN
              NWRD2 = HPTSEC(25)-HPTSEC(24) + HPTSEC(49)-HPTSEC(48)
            ELSE
              ICLL2L = ICLL/2
              NWRD2 = HPTSEC(ICLL2L+2) - HPTSEC(ICLL2L)
            CIF
            IF NWRD2.GE.LWRDC2
            THEN
N             COLLECT ALL Z IN RING 1 + 2
              PERFORM ZCOL23
N             FILL HISTOGRAM
              PERFORM HSTFLL
            CIF
          CIF
        CIF
      ICLL = ICLL + 1
      UNTIL ICLL.GT.96
C
      CALL MVC(HTMP(1),0,HIST(1),0,200)
      PERFORM HEVAL
C
      CIF
      CPROC
C
N     *************************
N     *      Z C O L 2 3      *
N     *************************
C
C
N     ****  COLLECT ALL Z IN RING 2 + 3  *****
      PROC ZCOL23
C
N       ZERO HIT COUNTERS FOR EACH LAYER
        FOR ILAYR=1,16
          NZ1(ILAYR) = 0
          NZ2(ILAYR) = 0
        CFOR
N       DRIFT SPACE BINS
N       SET POINTERS OF SC1
        IPT0 = HPTSEC(ICLL)
        IPT9 = HPTSEC(ICLL+1) - 1
N       LOOP OVER ALL HITS IN SC1
        FOR IPT = IPT0,IPT9,4
N         SET WIRE #
          IWIRE = HDATA(IPT)
          IWIRE = SHFTR(IWIRE,3)
          ILAYR = LAND(IWIRE,MKLAYR) + 1
N         STORE HIT
          IAMPL = HDATA(IPT+1)
          IAMPR = HDATA(IPT+2)
          IF IAMPL.GT.0 .AND. IAMPR.GT.0
          THEN
            IZ1 = (IZCON * (IAMPR-IAMPL)) / (IAMPR+IAMPL)
N           CHECK IF Z INSIDE ID
            IF IABS(IZ1).LT.IZMAX
            THEN
              NZ2(ILAYR) = NZ2(ILAYR) + 1
              IHIT = NZ2(ILAYR)
              HZ2(IHIT,ILAYR) = IZ1
            CIF
          CIF
        CFOR
N       LOOP OVER ADJACENT SECTORS
        ICLLA=0
        REPEAT
          ICLL2 = ICLL/2 + ICLLA
          IF(ICLL2.LT.25) ICLL2 = 48
          IF(ICLL2.GT.48) ICLL2 = 25
N         SET POINTERS OF SC2
          IPT0 = HPTSEC(ICLL2)
          IPT9 = HPTSEC(ICLL2+1) - 1
          IF IPT9.GT.IPT0
          THEN
N           LOOP OVER ALL HITS IN SC1
            FOR IPT = IPT0,IPT9,4
N             SET WIRE #
              IWIRE = HDATA(IPT)
              IWIRE = SHFTR(IWIRE,3)
              ILAYR = LAND(IWIRE,MKLAYR) + 1
N             STORE HIT
              IAMPL = HDATA(IPT+1)
              IAMPR = HDATA(IPT+2)
              IF IAMPL.GT.0 .AND. IAMPR.GT.0
              THEN
                IZ1 = (IZCON * (IAMPR-IAMPL)) / (IAMPR+IAMPL)
N               CHECK IF Z INSIDE ID
                IF IABS(IZ1).LT.IZMAX
                THEN
                  NZ1(ILAYR) = NZ1(ILAYR) + 1
                  IHIT = NZ1(ILAYR)
                  HZ1(IHIT,ILAYR) = IZ1
                CIF
              CIF
            CFOR
          CIF
        ICLLA = ICLLA + 1
        UNTIL ICLLA.GT.1
C
      CPROC
C
N     *************************
N     *      H S T F L L      *
N     *************************
C
N     FILL HISTOGRAM
      PROC HSTFLL
N       LOOP OVER ALL LAYERS
        FOR ILAYR=1,16
          MZ1 = NZ1(ILAYR)
          MZ2 = NZ2(ILAYR)
N         CHECK IF HITS
          IF MZ1.GT.0 .AND. MZ2.GT.0
          THEN
N           CALC. COMMON FACTOR
            FACT = (ILAYR-1)*PAR2 + PAR1
N           LOOP OVER HITS(SC1)
            FOR IHIT1=1,MZ1
              Z1 = HZ1(IHIT1,ILAYR)
              FI01 = FI1(IHIT1,ILAYR)
N             LOOP OVER HITS(SC2)
              FOR IHIT2=1,MZ2
N               MOMENTUM CUT (DFI .LT. DFIMAX)
                DFI = ABS(FI01 - FI2(IHIT2,ILAYR))
                IF DFI.LT.DFIMX0
                THEN
                  Z2 = HZ2(IHIT2,ILAYR)
N                 Z(VERTEX)
                  ZV = Z1 - (Z2-Z1)*FACT
N                 CALC. HIST. INDEX + PLOT
                  IZV = (ZV-ZLOW) * BINZIV + 1
                  IF IZV.GT.0 .AND. IZV.LE.100
                  THEN
                    HIST(IZV) = HIST(IZV) + 1
                  ELSE
                    IF(IZV.LE.  0) HUFLO = HUFLO + 1
                    IF(IZV.GT.NBINZ) HOFLO = HOFLO + 1
                  CIF
                CIF
              CFOR
            CFOR
          CIF
        CFOR
      CPROC
C
N     *****  H I S T. E V A L U A T I O N  *****
      PROC HEVAL
      IZCNT=0
      ICODE=0
      ZPREV=-1000000.
C     PRINT 2001, HIST
C2001 FORMAT('0HIST:',50I2,/,6X,50I2)
      WHILE IZCNT.LT.5
N       FIND BIN WITH MAX.CONTENT
        MAXHST = 0
        NHIST1 = 0
        FOR IHIST = 1,NBINZ
          NHIST1 = NHIST1 + HTMP(IHIST)
          IF(HTMP(IHIST).GT.MAXHST) MAXHST =HTMP(IHIST)
        CFOR
        MAXZ = MAXHST
N       FIND PEAK
        NPEAK = 0
        IH9 = NBINZ-11
        FOR IH=7,IH9
       IHSUM = HTMP(IH)+HTMP(IH+1)+HTMP(IH+2)+HTMP(IH+3)+HTMP(IH+4)
          IF IHSUM.GT.NPEAK
          THEN
N           MEMORIZE PEAK
            NPEAK = IHSUM
            HPEAK = IH
          CIF
        CFOR
        IF NPEAK.EQ.0
        THEN
        XWHILE
        CIF
        PEAK = NPEAK
          H1 = HPEAK - 7
          H2 = HPEAK + 7
       NBACK = HTMP(H1  )+HTMP(H1+1)+HTMP(H1+2)+HTMP(H1+3)+HTMP(H1+4)
     +       + HTMP(H2  )+HTMP(H2+1)+HTMP(H2+2)+HTMP(H2+3)+HTMP(H2+4)
          BACK = .5 * NBACK
C
N         CALC. ACCURATE PEAK POS.
          ZV = HTMP(HPEAK+1)   +HTMP(HPEAK+2)*2
     +        +HTMP(HPEAK+3)*3 +HTMP(HPEAK+4)*4
          ZV = ZV / PEAK
          ZVTX      = (HPEAK+ZV-.5)/BINZIV + ZLOW
          DZ =HTMP(HPEAK  )*(ZV   )**2 +HTMP(HPEAK+1)*(ZV-1.)**2
     +       +HTMP(HPEAK+2)*(ZV-2.)**2 +HTMP(HPEAK+3)*(ZV-3.)**2
     +       +HTMP(HPEAK+4)*(ZV-4.)**2
          IF(NPEAK.GT.NPKMIN) ICODE=ICODE+1
          SGN  = (PEAK - BACK)**2
          DSGN = BACK*.5 + PEAK
          IF(SGN/DSGN.GE.SBRAT .AND. NPEAK.GT.2) ICODE=ICODE+2
C     PRINT 2002, HPEAK,ZVTX,PEAK,BACK,SGN,DSGN
C2002 FORMAT('0PEAK:',I6,5F8.2)
          IF ICODE.GE.IZRSLT(6)
          THEN
          IF ICODE.EQ.IZRSLT(6).AND.ABS(ZVTX).GT.ABS(ZPREV)
          THEN
          XWHILE
          CIF
          IZCNT=IZCNT+1
          SCPEAK = BACK * .2
          HTMP(HPEAK-2)=SCPEAK
          HTMP(HPEAK-1)=SCPEAK
          HTMP(HPEAK  )=SCPEAK
          HTMP(HPEAK+1)=SCPEAK
          HTMP(HPEAK+2)=SCPEAK
          HTMP(HPEAK+3)=SCPEAK
          HTMP(HPEAK+4)=SCPEAK
          HTMP(HPEAK+5)=SCPEAK
          HTMP(HPEAK+6)=SCPEAK
          FZRSLT(2) = DZ / (BINZIV**2 * PEAK)
          FZRSLT(3) = FZRSLT(2) / PEAK
          FZRSLT(2) = SQRT(FZRSLT(2))
          FZRSLT(3) = SQRT(FZRSLT(3))
          FZRSLT(4) = PEAK
          FZRSLT(5) = BACK
          FZRSLT(1) = ZVTX
          IND = ABS(ZVTX)*.01 + 1.
          IF(IND.GT.4) IND = 4
          IF(IZRSLT(6).EQ.1) IND = IND + 4
          INDLB = IND + 2
      ZPREV=ZVTX
      IZRSLT(6)=ICODE
      ICODE=0
      ELSE
      XWHILE
        CIF
      CWHILE
      CPROC
C
      END
C
      SUBROUTINE INITZV
C
C     INITIALIZATION OF ZVERT LIMITS
#include "czvpar.for"
C
N     INITIALIZE DEFAULT PARAMETERS
N       SET DEFAULT LABEL TO 1
        LBZVDF = 1
N       HISTOGR. PARAMETERS
        ZLOW = -3500.
        BINZ = 70.
        NBINZ = 100
N       MIN. (NUMBER OF HITS)*4  IN RING 1
        NWRDR1 = 24
N       MIN. (NUMBER OF HITS)*4  IN SELECTED CELLS
        LWRDC0 = 8
N       MIN. (NUMBER OF HITS)*4  IN ADJ. CELLS OF R1
        LWRDC1 = 16
N       MIN. (NUMBER OF HITS)*4  IN ADJ. CELLS OF R2
        LWRDC2 = 16
N       LIMITS FOR TOO CLOSE HITS IN RING 1 + 2
        IDZ1LM = 80
        IDZ2LM = 140
N       MINIMUM PEAK HEIGHT
        NPKMIN = 8
N       MIN.: ((PEAK-BACK) / SIGMA)**2
        SBRAT  = 6.25
N       MAX. FI INCLINATION: P > 1GEV
        DFIMAX = .1
C
      RETURN
      END
C   28/04/81            MEMBER NAME  ZVERTFPR (JADEGS)      SHELTRAN
C   03/03/80 104271818  MEMBER NAME  ZVERTFPR (JADESR)      SHELTRAN
      SUBROUTINE ZVERTF
C
C     CALCULATION OF Z-VERTEX: P.STEFFEN(78/11/15)
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
#include "cgraph.for"
#include "czvpar.for"
#include "cjdrch.for"
#include "cdsmax.for"
#include "clbpgm.for"
C
#include "cworkzv.for"
C
C
N     SET PROGRAM LABEL
      LBPGM(2) = LBPGM(2) + 1
C
      DATA MKLAYR /15/
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT.EQ.0
      THEN
        LBINIT = 1
        IQJETC = IBLN('JETC')
N       COMMON FACTOR FOR Z(VERT.) CALC.
        IZCON = ZAL*.5
        IZMAX = ZMAX + 40.
      CIF
C
N       HISTOGRAM PARAMETERS
C
N     SET UP CYCLIC POINTER ARRAY
N     ADDRESS OF POINTERS TO CELLS (-1)
      IPJCA  = IDATA(IQJETC)
      IPJCA2 = IPJCA*2 + 2
C
N     COPY CELL POINTERS + CALC. LENGTH
      IP0 = IPJCA2 + 98
      ISEGO = IPJCA2
      FOR ISEG=1,96
        ISEGO = ISEGO + 1
        HPTSEC(ISEG) = HDATA(ISEGO) + IP0
      CFOR
      HPTSEC(97) = HDATA(ISEGO+1) + IP0
      HPTSEC(98) = 0
      PRINT 2901, HPTSEC
      I0 = IPJCA2 + 1
      I9 = I0 + 97
      PRINT 2901, (HDATA(I1),I1=I0,I9)
 2901 FORMAT(1H0,/,(1X,24I5))
C
      FZRSLT(1) = 0.
      FZRSLT(2) = 1000000.
      FZRSLT(3) = 1000000.
      FZRSLT(4) = 0.
      FZRSLT(5) = 0.
      IZRSLT(6) =-2
      INDLB = 3
C
N     DET. ZVTX FROM R1 + R2
      PRINT 2008
 2008 FORMAT('0PERFORM ZVTX12')
      DFIMX0 = DFIMAX
      PERFORM ZVTX12
C
N     CHECK IF NO GOOD VERTEX FOUND
      IF IZRSLT(6).LT.3
      THEN
N       NO GOOD VERTEX FOUND, SAVE RESULTS
        IZRSLT( 7) = IZRSLT( 1)
        IZRSLT( 8) = IZRSLT( 2)
        IZRSLT( 9) = IZRSLT( 3)
        IZRSLT(10) = IZRSLT( 4)
        IZRSLT(11) = IZRSLT( 5)
        IZRSLT(12) = IZRSLT( 6)
N       TRY ZVTX FROM R2 + R3
      PRINT 2009
 2009 FORMAT('0PERFORM ZVTX23')
        DFIMX0 = 1000.
        PERFORM ZVTX23
N       CHECK IF WORSE VERTEX
        IF IZRSLT(6).LE.IZRSLT(12)
        THEN
N         RESTORE OLD RESULTS
          IZRSLT( 1) = IZRSLT( 7)
          IZRSLT( 2) = IZRSLT( 8)
          IZRSLT( 3) = IZRSLT( 9)
          IZRSLT( 4) = IZRSLT(10)
          IZRSLT( 5) = IZRSLT(11)
          IZRSLT( 6) = IZRSLT(12)
        CIF
      CIF
C
N     SET BOS BANK OF RESULTS
      NWRES = 6
      CALL CCRE(IPHT,'ZVTX',0,NWRES,IERR)
      IF(IERR.NE.0) RETURN
      CALL BSAW(1,'ZVTX')
         ADATA(IPHT+1) = FZRSLT(1)
         ADATA(IPHT+2) = FZRSLT(2)
         ADATA(IPHT+3) = FZRSLT(3)
         ADATA(IPHT+4) = FZRSLT(4)
         ADATA(IPHT+5) = FZRSLT(5)
         ADATA(IPHT+6) = FZRSLT(6)
C
N     SET STOP LABEL
C---  PSTPS(INDLB) = .TRUE.
C
      RETURN
C
N     *************************
N     *      Z V T X 1 2      *
N     *************************
N     DETERMINE ZVTX FROM R1 + R2
C
      PROC ZVTX12
C
      BINZIV = 1. / BINZ
      PAR1 = FSENSW(1) / (FSENSW(2) - FSENSW(1))
      PAR2 =  RINCR(1) / (FSENSW(2) - FSENSW(1))
      HUFLO = 0
      HOFLO = 0
      MAXZ = 0
      NBACK = 0
N     ZERO HISTOGRAM
      CALL SETS(HIST(1),0,200,0)
C
N     CHECK IF AT LEAST 10 HITS IN 1. RING
      IF HPTSEC(25)-HPTSEC(1) .LT. NWRDR1
      THEN
        IZRSLT(6) = -2
        INDLB = 1
      ELSE
C
N
N     LOOP OVER ALLSECTOR(RING1): JSEC = SECTOR NUMBER + 1
      JSEC=1
      REPEAT
N       # OF WORDS IN SECT
        NWRD1 = HPTSEC(JSEC+1) - HPTSEC(JSEC)
        NWRD2 = HPTSEC(JSEC+25) - HPTSEC(JSEC+24)
N       CHECK IF MORE THAN 1 HIT
        IF NWRD1.GE.LWRDC0
        THEN
N         # OF WORDS IN LEFT ADJ SECT
          IF JSEC.EQ.1
          THEN
            NWRD1L= HPTSEC(25) - HPTSEC(24)
            NWRD2L= HPTSEC(49) - HPTSEC(48)
          ELSE
            NWRD1L= HPTSEC(JSEC   ) - HPTSEC(JSEC- 1)
            NWRD2L= HPTSEC(JSEC+24) - HPTSEC(JSEC+23)
          CIF
N         # OF WORDS IN RGHT ADJ SECT
          IF JSEC.EQ.24
          THEN
            NWRD1R= HPTSEC( 2) - HPTSEC( 1)
            NWRD2R= HPTSEC(26) - HPTSEC(25)
          ELSE
            NWRD1R= HPTSEC(JSEC+ 2) - HPTSEC(JSEC+ 1)
            NWRD2R= HPTSEC(JSEC+26) - HPTSEC(JSEC+25)
          CIF
N         CHECK IF MORE THAN 12 HITS
          IF NWRD1+NWRD1L.GE.LWRDC1 .OR. NWRD1+NWRD1R.GE.LWRDC1
          THEN
N           CHECK IF .GT. 10 HITS IN CORRSP SECT
            IF NWRD2L+NWRD2.GE.LWRDC2 .OR. NWRD2R+NWRD2.GE.LWRDC2
            THEN
N             COLLECT ALL Z IN RING 1 + 2
              PERFORM ZCOL12
N             FILL HISTOGRAM
              PERFORM HSTFLL
            CIF
          CIF
        CIF
      JSEC = JSEC + 1
      UNTIL JSEC.GT.24
C
      CALL MVC(HTEMP(1),0,HIST(1),0,200)
      PERFORM HEVAL
C
      CIF
      CPROC
C
N     *************************
N     *      Z C O L 1 2      *
N     *************************
C
N     ****  COLLECT ALL Z IN RING 1 + 2  *****
      PROC ZCOL12
C
N       ZERO HIT COUNTERS FOR EACH LAYER
        FOR ILAYR=1,16
          NZ1(ILAYR) = 0
          NZ2(ILAYR) = 0
        CFOR
N       DRIFT SPACE BINS
        DSBIN1 = TIMDEL(1,1)
        DSBIN2 = TIMDEL(2,1)
N       SET POINTERS OF SC1
        IPT0 = HPTSEC(JSEC)
        IPT9 = HPTSEC(JSEC+1) - 1
N       LOOP OVER ALL HITS IN SC1
        FOR IPT = IPT0,IPT9,4
N         SET WIRE #
          IWIRE = HDATA(IPT)
          IWIRE = SHFTR(IWIRE,3)
          ILAYR = LAND(IWIRE,MKLAYR) + 1
N         STORE HIT
          IAMPL = HDATA(IPT+1)
          IAMPR = HDATA(IPT+2)
          IF IAMPL.GT.0 .AND. IAMPR.GT.0
          THEN
            IZ1 = (IZCON * (IAMPR-IAMPL)) / (IAMPR+IAMPL)
N           CHECK IF Z INSIDE ID
            IF IABS(IZ1).LT.IZMAX
            THEN
              NZ1(ILAYR) = NZ1(ILAYR) + 1
              IHIT = NZ1(ILAYR)
              HZ1(IHIT,ILAYR) = IZ1
N             FI ANGLE
              IF(ILAYR.GT.8) DSBIN1 = DSBIN2
              DRLAY = (ILAYR-1) * RINCR(1)
              FI1(IHIT,ILAYR) = HDATA(IPT+3)*DSBIN1 / (FSENSW(1)+DRLAY)
            CIF
          CIF
        CFOR
N       LOOP OVER ADJACENT SECTORS
        JSECA=1
        REPEAT
          JSEC2 = JSEC + JSECA + 22
          IF(JSEC2.LT.25) JSEC2 = 48
          IF(JSEC2.GT.48) JSEC2 = 25
N         SET POINTERS OF SC2
          IPT0 = HPTSEC(JSEC2)
          IPT9 = HPTSEC(JSEC2+1) - 1
          IF IPT9.GT.IPT0
          THEN
N           DRIFT SPACE BINS
            DSBIN1 = TIMDEL(1,2)
            DSBIN2 = TIMDEL(2,2)
N           LOOP OVER ALL HITS IN SC1
            FOR IPT = IPT0,IPT9,4
N             SET WIRE #
              IWIRE = HDATA(IPT)
              IWIRE = SHFTR(IWIRE,3)
              ILAYR = LAND(IWIRE,MKLAYR) + 1
N             STORE HIT
              IAMPL = HDATA(IPT+1)
              IAMPR = HDATA(IPT+2)
              IF IAMPL.GT.0 .AND. IAMPR.GT.0
              THEN
                IZ2 = (IZCON * (IAMPR-IAMPL)) / (IAMPR+IAMPL)
N               CHECK IF Z INSIDE ID
                IF IABS(IZ2).LT.IZMAX
                THEN
                  NZ2(ILAYR) = NZ2(ILAYR) + 1
                  IHIT = NZ2(ILAYR)
                  HZ2(IHIT,ILAYR) = IZ2
N                 FI ANGLE
                  IF(ILAYR.GT.8) DSBIN1 = DSBIN2
                  DS = HDATA(IPT+3)*DSBIN1
                  IF(JSECA.NE.2)
     ?                     DS = DSMAX(ILAYR,2,1)+DSMAX(ILAYR,2,2) - DS
                  DRLAY = (ILAYR-1) * RINCR(2)
                  FI2(IHIT,ILAYR) = DS / (FSENSW(2)+DRLAY)
                CIF
              CIF
            CFOR
          CIF
        JSECA = JSECA + 1
        UNTIL JSECA.GT.3
C
      CPROC
C
N     *************************
N     *      Z V T X 2 3      *
N     *************************
C
N     DETERMINE ZVTX FROM R2 + R3
      PROC ZVTX23
C
N     INITIALIZE HISTOGRAM
      BINZIV = .5 / BINZ
      PAR1 = FSENSW(2) / (FSENSW(3) - FSENSW(2))
      PAR2 =  RINCR(2) / (FSENSW(3) - FSENSW(2))
      HUFLO = 0
      HOFLO = 0
      MAXZ = 0
      NBACK = 0
N     ZERO HISTOGRAM
      CALL SETS (HIST(1),0,200,0)
      CALL SETSL(FI1(1,1),0,1024,0)
C
N     CHECK IF AT LEAST 10 HITS IN 3. RING
      IF HPTSEC(97)-HPTSEC(49) .LT. NWRDR1
      THEN
        IZRSLT(6) = -2
        INDLB = 1
        PRINT 2911, HPTSEC(49),HPTSEC(97),NWRDR1
 2911   FORMAT(' REJECT -2',10I6)
      ELSE
C
N
N     LOOP OVER ALL SECTORS(RING1): ICLL = SECTOR NUMBER + 1
      ICLL=49
      REPEAT
N       # OF WORDS IN SECT
        NWRD1 = HPTSEC(ICLL+1) - HPTSEC(ICLL)
N       CHECK IF MORE THAN 1 HIT
        IF NWRD1.GE.LWRDC0
        THEN
N         # OF WORDS IN LEFT ADJ SECT
          IF ICLL.EQ.49
          THEN
            NWRD1L= HPTSEC(97) - HPTSEC(96)
          ELSE
            NWRD1L= HPTSEC(ICLL   ) - HPTSEC(ICLL- 1)
          CIF
N         # OF WORDS IN RGHT ADJ SECT
          IF ICLL.EQ.96
          THEN
            NWRD1R= HPTSEC(50) - HPTSEC(49)
          ELSE
            NWRD1R= HPTSEC(ICLL+ 2) - HPTSEC(ICLL+ 1)
          CIF
N         CHECK IF MORE THAN 12 HITS
          IF NWRD1+NWRD1L.GE.LWRDC1 .OR. NWRD1+NWRD1R.GE.LWRDC1
          THEN
N           CHECK IF .GT. 10 HITS IN CORRSP SECT
            IF ICLL.EQ.49 .OR. ICLL.EQ.96
            THEN
              NWRD2 = HPTSEC(25)-HPTSEC(24) + HPTSEC(49)-HPTSEC(48)
            ELSE
              ICLL2L = ICLL/2
              NWRD2 = HPTSEC(ICLL2L+2) - HPTSEC(ICLL2L)
            CIF
            IF NWRD2.GE.LWRDC2
            THEN
N             COLLECT ALL Z IN RING 1 + 2
      PRINT 2912, ICLL,ICLL2L,NWRD1,NWRD1R,NWRD1L,NWRD2
 2912 FORMAT(' PERFORM ZCOL23',10I6)
              PERFORM ZCOL23
N             FILL HISTOGRAM
              PERFORM HSTFLL
            CIF
          CIF
        CIF
      ICLL = ICLL + 1
      UNTIL ICLL.GT.96
C
      CALL MVC(HTEMP(1),0,HIST(1),0,200)
      PERFORM HEVAL
C
      CIF
      CPROC
C
N     *************************
N     *      Z C O L 2 3      *
N     *************************
C
C
N     ****  COLLECT ALL Z IN RING 2 + 3  *****
      PROC ZCOL23
C
N       ZERO HIT COUNTERS FOR EACH LAYER
        FOR ILAYR=1,16
          NZ1(ILAYR) = 0
          NZ2(ILAYR) = 0
        CFOR
N       DRIFT SPACE BINS
N       SET POINTERS OF SC1
        IPT0 = HPTSEC(ICLL)
        IPT9 = HPTSEC(ICLL+1) - 1
N       LOOP OVER ALL HITS IN SC1
        FOR IPT = IPT0,IPT9,4
N         SET WIRE #
          IWIRE = HDATA(IPT)
          IWIRE = SHFTR(IWIRE,3)
          ILAYR = LAND(IWIRE,MKLAYR) + 1
N         STORE HIT
          IAMPL = HDATA(IPT+1)
          IAMPR = HDATA(IPT+2)
          IF IAMPL.GT.0 .AND. IAMPR.GT.0
          THEN
            IZ1 = (IZCON * (IAMPR-IAMPL)) / (IAMPR+IAMPL)
N           CHECK IF Z INSIDE ID
            IF IABS(IZ1).LT.IZMAX
            THEN
              NZ2(ILAYR) = NZ2(ILAYR) + 1
              IHIT = NZ2(ILAYR)
              HZ2(IHIT,ILAYR) = IZ1
            CIF
          CIF
        CFOR
N       LOOP OVER ADJACENT SECTORS
        ICLLA=0
        REPEAT
          ICLL2 = ICLL/2 + ICLLA
          IF(ICLL2.LT.25) ICLL2 = 48
          IF(ICLL2.GT.48) ICLL2 = 25
N         SET POINTERS OF SC2
          IPT0 = HPTSEC(ICLL2)
          IPT9 = HPTSEC(ICLL2+1) - 1
          IF IPT9.GT.IPT0
          THEN
N           LOOP OVER ALL HITS IN SC1
            FOR IPT = IPT0,IPT9,4
N             SET WIRE #
              IWIRE = HDATA(IPT)
              IWIRE = SHFTR(IWIRE,3)
              ILAYR = LAND(IWIRE,MKLAYR) + 1
N             STORE HIT
              IAMPL = HDATA(IPT+1)
              IAMPR = HDATA(IPT+2)
              IF IAMPL.GT.0 .AND. IAMPR.GT.0
              THEN
                IZ1 = (IZCON * (IAMPR-IAMPL)) / (IAMPR+IAMPL)
N               CHECK IF Z INSIDE ID
                IF IABS(IZ1).LT.IZMAX
                THEN
                  NZ1(ILAYR) = NZ1(ILAYR) + 1
                  IHIT = NZ1(ILAYR)
                  HZ1(IHIT,ILAYR) = IZ1
                CIF
              CIF
            CFOR
          CIF
        ICLLA = ICLLA + 1
        UNTIL ICLLA.GT.1
C
      CPROC
C
N     *************************
N     *      H S T F L L      *
N     *************************
C
N     FILL HISTOGRAM
      PROC HSTFLL
N       LOOP OVER ALL LAYERS
        FOR ILAYR=1,16
          MZ1 = NZ1(ILAYR)
          MZ2 = NZ2(ILAYR)
N         CHECK IF HITS
          IF MZ1.GT.0 .AND. MZ2.GT.0
          THEN
N           CALC. COMMON FACTOR
            FACT = (ILAYR-1)*PAR2 + PAR1
N           LOOP OVER HITS(SC1)
            FOR IHIT1=1,MZ1
              Z1 = HZ1(IHIT1,ILAYR)
              FI01 = FI1(IHIT1,ILAYR)
N             LOOP OVER HITS(SC2)
              FOR IHIT2=1,MZ2
N               MOMENTUM CUT (DFI .LT. DFIMAX)
                DFI = ABS(FI01 - FI2(IHIT2,ILAYR))
                IF DFI.LT.DFIMX0
                THEN
                  Z2 = HZ2(IHIT2,ILAYR)
N                 Z(VERTEX)
                  ZV = Z1 - (Z2-Z1)*FACT
N                 CALC. HIST. INDEX + PLOT
                  IZV = (ZV-ZLOW) * BINZIV + 1
                  IF IZV.GT.0 .AND. IZV.LE.100
                  THEN
                    HIST(IZV) = HIST(IZV) + 1
                  ELSE
                    IF(IZV.LE.  0) HUFLO = HUFLO + 1
                    IF(IZV.GT.NBINZ) HOFLO = HOFLO + 1
                  CIF
                CIF
              CFOR
            CFOR
          CIF
        CFOR
      CPROC
C
N     *****  H I S T. E V A L U A T I O N  *****
      PROC HEVAL
      IZCNT=0
      ICODE=0
      ZPREV=-1000000.
      PRINT 2904, LBZVDF,ZLOW,  BINZ,  NBINZ,
     ,            NWRDR1,LWRDC0,LWRDC1,LWRDC2,
     ,            IDZ1LM,IDZ2LM,NPKMIN,SBRAT,DFIMX0
2904  FORMAT('0PARAMETERS:',I6,2F6.0,8I6,F6.2,F6.3)
      WHILE IZCNT.LT.5
      PRINT 2001, HTEMP
 2001 FORMAT('0HIST:',50I2,/,6X,50I2)
N       FIND BIN WITH MAX.CONTENT
        MAXHST = 0
        NHIST1 = 0
        FOR IHIST = 1,NBINZ
          NHIST1 = NHIST1 + HTEMP(IHIST)
          IF(HTEMP(IHIST).GT.MAXHST) MAXHST =HTEMP(IHIST)
        CFOR
        MAXZ = MAXHST
N       FIND PEAK
        NPEAK = 0
        IH9 = NBINZ-11
        FOR IH=7,IH9
       IHSUM = HTEMP(IH)+HTEMP(IH+1)+HTEMP(IH+2)+HTEMP(IH+3)+HTEMP(IH+4)
          IF IHSUM.GT.NPEAK
          THEN
N           MEMORIZE PEAK
            NPEAK = IHSUM
            HPEAK = IH
          CIF
        CFOR
        IF NPEAK.EQ.0
        THEN
        XWHILE
        CIF
        PEAK = NPEAK
          H1 = HPEAK - 7
          H2 = HPEAK + 7
       NBACK = HIST(H1  )+HIST(H1+1)+HIST(H1+2)+HIST(H1+3)+HIST(H1+4)
     +       + HIST(H2  )+HIST(H2+1)+HIST(H2+2)+HIST(H2+3)+HIST(H2+4)
          BACK = .5 * NBACK
C
N         CALC. ACCURATE PEAK POS.
          ZV = HTEMP(HPEAK+1)   +HTEMP(HPEAK+2)*2
     +        +HTEMP(HPEAK+3)*3 +HTEMP(HPEAK+4)*4
          ZV = ZV / PEAK
          ZVTX      = (HPEAK+ZV-.5)/BINZIV + ZLOW
          DZ =HTEMP(HPEAK  )*(ZV   )**2 +HTEMP(HPEAK+1)*(ZV-1.)**2
     +       +HTEMP(HPEAK+2)*(ZV-2.)**2 +HTEMP(HPEAK+3)*(ZV-3.)**2
     +       +HTEMP(HPEAK+4)*(ZV-4.)**2
          IF(NPEAK.GT.NPKMIN) ICODE=ICODE+1
          SGN  = (PEAK - BACK)**2
          DSGN = BACK*.5 + PEAK
          IF(SGN/DSGN.GE.SBRAT .AND. NPEAK.GT.2) ICODE=ICODE+2
      PRINT 2002, HPEAK,ZVTX,PEAK,BACK,SGN,DSGN,ICODE
 2002 FORMAT('0PEAK:',I6,5F8.2,I3)
          IF ICODE.GE.IZRSLT(6)
          THEN
          IF ICODE.EQ.IZRSLT(6).AND.ABS(ZVTX).GT.ABS(ZPREV)
          THEN
          XWHILE
          CIF
          IZCNT=IZCNT+1
          SCPEAK = BACK * .2
          HTEMP(HPEAK  )=SCPEAK
          HTEMP(HPEAK+1)=SCPEAK
          HTEMP(HPEAK+2)=SCPEAK
          HTEMP(HPEAK+3)=SCPEAK
          HTEMP(HPEAK+4)=SCPEAK
          FZRSLT(2) = DZ / (BINZIV**2 * PEAK)
          FZRSLT(3) = FZRSLT(2) / PEAK
          FZRSLT(2) = SQRT(FZRSLT(2))
          FZRSLT(3) = SQRT(FZRSLT(3))
          FZRSLT(4) = PEAK
          FZRSLT(5) = BACK
          FZRSLT(1) = ZVTX
          IND = ABS(ZVTX)*.01 + 1.
          IF(IND.GT.4) IND = 4
          IF(IZRSLT(6).EQ.1) IND = IND + 4
          INDLB = IND + 2
      ZPREV=ZVTX
      IZRSLT(6)=ICODE
      ICODE=0
      ELSE
      XWHILE
        CIF
      CWHILE
      CPROC
C
      END
C
      SUBROUTINE INITZV
C
C     INITIALIZATION OF ZVERT LIMITS
#include "czvpar.for"
C
N     INITIALIZE DEFAULT PARAMETERS
N       SET DEFAULT LABEL TO 1
        LBZVDF = 1
N       HISTOGR. PARAMETERS
        ZLOW = -3500.
        BINZ = 70.
        NBINZ = 100
N       MIN. (NUMBER OF HITS)*4  IN RING 1
        NWRDR1 = 24
N       MIN. (NUMBER OF HITS)*4  IN SELECTED CELLS
        LWRDC0 = 8
N       MIN. (NUMBER OF HITS)*4  IN ADJ. CELLS OF R1
        LWRDC1 = 16
N       MIN. (NUMBER OF HITS)*4  IN ADJ. CELLS OF R2
        LWRDC2 = 16
N       LIMITS FOR TOO CLOSE HITS IN RING 1 + 2
        IDZ1LM = 80
        IDZ2LM = 140
N       MINIMUM PEAK HEIGHT
        NPKMIN = 8
N       MIN.: ((PEAK-BACK) / SIGMA)**2
        SBRAT  = 6.25
N       MAX. FI INCLINATION: P > 1GEV
        DFIMAX = .1
C
      RETURN
      END
