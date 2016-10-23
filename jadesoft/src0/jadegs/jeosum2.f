C   22/03/97 703221845  MEMBER NAME  JEOSUM2  (JADEGS)      FORTRAN
C   07/11/86 701231748  MEMBER NAME  AKOP     (S)           SHELTRAN
      SUBROUTINE AKOP( P, NP, NMAXP, A, AXIS, ITMERR )
C *---------------------------------------------------------
C *
C *  VERSION OF 21/05/79   LAST MOD 02/06/81   E.ELSEN/S.BETHKE
C *  INPUT : P(4,NP)   NP MOMENTA PX,PY,PZ,ABS(P)
C *          NP        NUMBER OF MOMENTA
C *          NMAXP     CUTOFF FOR PERMUTED MOMENTA
C *                    =0 MEANS NO CUTOFF
C *  OUTPUT: AKOP      AKOPLANARITY VALUE
C *          AXIS      AKOP AXIS
C *          ITMERR    =1, IF LEFT TIME IS NOT ENOUGH TO CALCULATE THRST
C *                    (NOW MIN.2 SECONDS ARE REQUIRED.)
C *  METHOD: MOMENTUM CONSERVATION IS TAKEN CARE OF BY INTRODUCING
C *          MISSING MOMENTUM VECTOR AT THE END OF P ARRAY.
C *          AKOPLANARITY VALUE IS FOUND BY LOOKING THROUGH ALL
C *          PERMUTATIONS OF THE MOMENTA - PRODUCTS (PI X PJ) * PK
C *---------------------------------------------------------
      DIMENSION P(1), AXIS(3)
      DIMENSION PIN(320), IPERM(200),IPS(320),PS(320)
      COMMON / CWORK / WORK(840)
      EQUIVALENCE (WORK(1),PIN(1)),(WORK(321),IPERM(1))
      EQUIVALENCE (WORK(521),PS(1),IPS(1))
C
      DATA IPINC / 4 /
      DATA NLIMIT / 20 /
      DATA NSEC / 2 /
      DATA IER0 / 0/, IER1 /0/, IER2 /0/
C
C
      ITMERR = 0
C
      AXIS(1) = 1.
      AXIS(2) = 1.
      AXIS(3) = 1.
      A = 1000.
C
      IF NP .LE. 0
      THEN
      IF IER0 .LT. 10
      THEN
      WRITE(6,9103)
 9103 FORMAT(' +++ ERROR IN AKOP.  CALLED WITH ZERO PARTICLES +++')
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
 9101 FORMAT(' +++++  WARNING FROM AKOP ROUTINE  +++++'/
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
 9104 FORMAT(' +++ ERROR IN AKOP. MOMENTUM SUM AFTER BALANCE=0. +++')
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
N     AKOP AND AXIS FOR PIN ARRAY
      PERFORM AKOPL
      PERFORM AKOEXP
      ELSE
N     COPY FROM P TO PIN ARRAY
      CALL UCOPY( P, PIN, NTOT*4 )
N     AKOP   FOR PIN ARRAY
      PERFORM AKOPL
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
      PROC AKOPL
C----------------
C
      IF A .NE. 0.
      THEN
      K1 = NTOT-1
      FOR INDI=1,K1
      K2 = INDI+1
      FOR INDJ=K2,NTOT
N     CALCULATE PIJ = PI X PJ
      INDI4 = INDI * 4
      P11 = PIN(INDI4-3)
      P12 = PIN(INDI4-2)
      P13 = PIN(INDI4-1)
      INDJ4 = INDJ * 4
      P21 = PIN(INDJ4-3)
      P22 = PIN(INDJ4-2)
      P23 = PIN(INDJ4-1)
      PIJ1 = P12 * P23 - P13 * P22
      PIJ2 = P13 * P21 - P11 * P23
      PIJ3 = P11 * P22 - P12 * P21
      PIJ4 = SQRT(PIJ1*PIJ1 + PIJ2*PIJ2 + PIJ3*PIJ3)
      AIJ = 0.
      IF PIJ4 .GT. 1.E-5
      THEN
      FOR INDK=1,NTOT
N     CALCULATE SUM(PIJ*PK)
      INDK4 = INDK * 4
      AIJ = AIJ + ABS(PIJ1 * PIN(INDK4-3) + PIJ2 * PIN(INDK4-2)
     *              + PIJ3 * PIN(INDK4-1))
      CFOR
C
      AIJ = AIJ/PIJ4
      IF AIJ .LT. A
      THEN
      AXIS(1) = PIJ1/PIJ4
      AXIS(2) = PIJ2/PIJ4
      AXIS(3) = PIJ3/PIJ4
      A = AIJ
      CIF
      ELSE
      IF NTOT .LE. 4
      THEN
      AXIS(1) = 0.
      AXIS(2) = 0.
      AXIS(3) = 0.
      A = 0.
      CIF
      CIF
C
      CFOR
      CFOR
      A = 4. * ((A / PSUM)**2)
      CIF
      CPROC
C
C
C--------------
C
      PROC AKOEXP
C--------------
N     COMPUTE AKOPL EXPLICITLY
      NIPINC = NP1*IPINC
      AA = 0.
      FOR J=1,NIPINC,IPINC
      AA = AA + ABS( AXIS(1)*P(J)+AXIS(2)*P(J+1)+AXIS(3)*P(J+2) )
      CFOR
      A = 4. * ((AA / PSUM)**2)
      CPROC
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
C   07/11/86 701231748  MEMBER NAME  AKOP     (S)           SHELTRAN
      SUBROUTINE AKOP( P, NP, NMAXP, A, AXIS, ITMERR )
C *---------------------------------------------------------
C *
C *  VERSION OF 21/05/79   LAST MOD 02/06/81   E.ELSEN/S.BETHKE
C *  INPUT : P(4,NP)   NP MOMENTA PX,PY,PZ,ABS(P)
C *          NP        NUMBER OF MOMENTA
C *          NMAXP     CUTOFF FOR PERMUTED MOMENTA
C *                    =0 MEANS NO CUTOFF
C *  OUTPUT: AKOP      AKOPLANARITY VALUE
C *          AXIS      AKOP AXIS
C *          ITMERR    =1, IF LEFT TIME IS NOT ENOUGH TO CALCULATE THRST
C *                    (NOW MIN.2 SECONDS ARE REQUIRED.)
C *  METHOD: MOMENTUM CONSERVATION IS TAKEN CARE OF BY INTRODUCING
C *          MISSING MOMENTUM VECTOR AT THE END OF P ARRAY.
C *          AKOPLANARITY VALUE IS FOUND BY LOOKING THROUGH ALL
C *          PERMUTATIONS OF THE MOMENTA - PRODUCTS (PI X PJ) * PK
C *---------------------------------------------------------
      DIMENSION P(1), AXIS(3)
      DIMENSION PIN(320), IPERM(200),IPS(320),PS(320)
      COMMON / CWORK / WORK(840)
      EQUIVALENCE (WORK(1),PIN(1)),(WORK(321),IPERM(1))
      EQUIVALENCE (WORK(521),PS(1),IPS(1))
C
      DATA IPINC / 4 /
      DATA NLIMIT / 20 /
      DATA NSEC / 2 /
      DATA IER0 / 0/, IER1 /0/, IER2 /0/
C
C
      ITMERR = 0
C
      AXIS(1) = 1.
      AXIS(2) = 1.
      AXIS(3) = 1.
      A = 1000.
C
      IF NP .LE. 0
      THEN
      IF IER0 .LT. 10
      THEN
      WRITE(6,9103)
 9103 FORMAT(' +++ ERROR IN AKOP.  CALLED WITH ZERO PARTICLES +++')
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
 9101 FORMAT(' +++++  WARNING FROM AKOP ROUTINE  +++++'/
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
 9104 FORMAT(' +++ ERROR IN AKOP. MOMENTUM SUM AFTER BALANCE=0. +++')
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
N     AKOP AND AXIS FOR PIN ARRAY
      PERFORM AKOPL
      PERFORM AKOEXP
      ELSE
N     COPY FROM P TO PIN ARRAY
      CALL UCOPY( P, PIN, NTOT*4 )
N     AKOP   FOR PIN ARRAY
      PERFORM AKOPL
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
      PROC AKOPL
C----------------
C
      IF A .NE. 0.
      THEN
      K1 = NTOT-1
      FOR INDI=1,K1
      K2 = INDI+1
      FOR INDJ=K2,NTOT
N     CALCULATE PIJ = PI X PJ
      INDI4 = INDI * 4
      P11 = PIN(INDI4-3)
      P12 = PIN(INDI4-2)
      P13 = PIN(INDI4-1)
      INDJ4 = INDJ * 4
      P21 = PIN(INDJ4-3)
      P22 = PIN(INDJ4-2)
      P23 = PIN(INDJ4-1)
      PIJ1 = P12 * P23 - P13 * P22
      PIJ2 = P13 * P21 - P11 * P23
      PIJ3 = P11 * P22 - P12 * P21
      PIJ4 = SQRT(PIJ1*PIJ1 + PIJ2*PIJ2 + PIJ3*PIJ3)
      AIJ = 0.
      IF PIJ4 .GT. 1.E-5
      THEN
      FOR INDK=1,NTOT
N     CALCULATE SUM(PIJ*PK)
      INDK4 = INDK * 4
      AIJ = AIJ + ABS(PIJ1 * PIN(INDK4-3) + PIJ2 * PIN(INDK4-2)
     *              + PIJ3 * PIN(INDK4-1))
      CFOR
C
      AIJ = AIJ/PIJ4
      IF AIJ .LT. A
      THEN
      AXIS(1) = PIJ1/PIJ4
      AXIS(2) = PIJ2/PIJ4
      AXIS(3) = PIJ3/PIJ4
      A = AIJ
      CIF
      ELSE
      IF NTOT .LE. 4
      THEN
      AXIS(1) = 0.
      AXIS(2) = 0.
      AXIS(3) = 0.
      A = 0.
      CIF
      CIF
C
      CFOR
      CFOR
      A = 4. * ((A / PSUM)**2)
      CIF
      CPROC
C
C
C--------------
C
      PROC AKOEXP
C--------------
N     COMPUTE AKOPL EXPLICITLY
      NIPINC = NP1*IPINC
      AA = 0.
      FOR J=1,NIPINC,IPINC
      AA = AA + ABS( AXIS(1)*P(J)+AXIS(2)*P(J+1)+AXIS(3)*P(J+2) )
      CFOR
      A = 4. * ((AA / PSUM)**2)
      CPROC
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
C   28/04/87 803181309  MEMBER NAME  AMPS2Z   (JADEGS)      SHELTRAN
      SUBROUTINE AMPS2Z( IP, NPJETC, Z, W, IFLAG )
C-----------------------------------------------------------
C  VERSION OF 21/04/87         LAST MOD 10/03/88   E ELSEN
C  Convert the amplitudes stored in HW(IP+1) and HW(IP+2)
C  into Z and calculate the weight W associated with this
C  measurement.
C  NPJETC = IW(IBLN('JETC'))
C  Flag IFLAG is 0 if the hit passes some quality criteria
C  Weighting will only work if ZSFIT has been called. In that
C  case CZSCAL has been initialised ( ZALPDI=1400 )
C  Note the different effective wire length that is used
C  for the two calibrations.
C  Two track cut now at 3.6 mm to avoid differences
C  between MC and Data.
C-----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
C                                           from zsfit
C                                           zal name conflict
C     COMMON /CZSCAL/ IPVERS,ZAL,RESFAC,SECH(5)
      COMMON /CZSCAL/ IPVERS, ZALPDI, RESFAC, SECH(5)
#include "czsprm.for"
C
C
#include "cjdrch.for"
C
      COMMON / BCS / HW(1)
C
      REAL EXTRMZ / 1250. /, ZALDEF / 1400. /
C                                           SECOND HIT DISTANCE
      REAL DISMAX / 3.6 /
C
      LOGICAL FIRST / .TRUE. /
C
      IF FIRST
      THEN
         FIRST = .FALSE.
         WRITE(6,9101) NZSPRD, ZALPDI, ZALDEF
 9101    FORMAT(' +++ AMPS2Z    NZSPRD=',I3,' ZALPDI=',F10.3,
     *          ' ZALDEF=',F10.3)
      CIF

C
      Z = 0.
      W = 1.
      IFLAG = 16
C
      AL = HW(IP+2)/8.
      AR = HW(IP+1)/8.
C                                          check whether CZSCAL has been
C                                          initialised
      IF  AL.GT.0. .AND. AR.GT.0.
      THEN
        IF  ZALPDI .EQ. ZALDEF
        THEN
          Z = ZALPDI*(AL-AR)/(AL+AR)
          IF NZSPRD .LE. 2
          THEN
            W = ( AZSSAV(NZSPRD) /
     *              ( AZSRS0(NZSPRD)+
     *                AZSRSA(NZSPRD)*SQRT(AL**2+AR**2)/(AL+AR)**2
     *              )
     *          )**2
          ELSE
            W = (AZSSAV(NZSPRD)/
     *             (AZSRS0(NZSPRD)+AZSRSA(NZSPRD)/(AL+AR)))**2
          CIF
        ELSE
          Z = .5*ZAL*(AL-AR)/(AL+AR)
        CIF
        IF  ABS( Z ) .LT. EXTRMZ
        THEN
          IF NZSPRD.GT.2
          THEN
C                                           ANY CLOSE HIT?
            ISEC = 0
            NP = IP - 4
            IF  NP.GT.NPJETC*2+100 .AND. HW(NP)/8 .EQ. HW(IP)/8
            THEN
               ICELL = HW(IP)/128 + 1
               DIS = (HW(NP+3)-HW(IP+3))*
     *               (DRIVEL(ICELL,1)+DRIVEL(ICELL,2))/2.
               IF( ABS(DIS).LT.DISMAX ) ISEC = ISEC + 1
            CIF
            NP = IP + 4
            IF  NP.LT.NPJETC*2+100+HW(NPJETC*2+99) .AND.
     *          HW(NP)/8 .EQ. HW(IP)/8
            THEN
               ICELL = HW(IP)/128 + 1
               DIS = (HW(NP+3)-HW(IP+3))*
     *               (DRIVEL(ICELL,1)+DRIVEL(ICELL,2))/2.
               IF( ABS(DIS).LT.DISMAX ) ISEC = ISEC + 1
            CIF
            IF( ISEC .EQ. 0 ) IFLAG = 0
          ELSE
             IFLAG = 0
          CIF
        CIF
C                                           OVERFLOW HITS
        IF(IFLAG.EQ.0 .AND.
     *     ( HW(IP+1).EQ. 32760 .OR. HW(IP+2).EQ. 32760 ) ) IFLAG = 32
      CIF
      RETURN
      END
      SUBROUTINE AMPS2Z( IP, NPJETC, Z, W, IFLAG )
C-----------------------------------------------------------
C-     COPIED FROM F22ELS.ZSLIB.S    7.5.87   --------------
C-----------------------------------------------------------
C  VERSION OF 21/04/87                             E ELSEN
C  BLOCK DATA MOVED TO JADEBD  LAST MOD 16/06/87   E ELSEN
C  Convert the amplitudes stored in HW(IP+1) and HW(IP+2)
C  into Z and calculate the weight W associated with this
C  measurement.
C  NPJETC = IW(IBLN('JETC'))
C  Flag IFLAG is 0 if the hit passes some quality criteria
C  Weighting will only work if ZSFIT has been called. In that
C  case CZSCAL has been initialised ( ZALPDI=1400 )
C  Note the different effective wire length that is used
C  for the two calibrations.
C-----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
C                                           from zsfit
C                                           zal name conflict
C     COMMON /CZSCAL/ IPVERS,ZAL,RESFAC,SECH(5)
      COMMON /CZSCAL/ IPVERS, ZALPDI, RESFAC, SECH(5)
#include "czsprm.for"
C
C
#include "cjdrch.for"
C
      COMMON / BCS / HW(1)
C
      REAL EXTRMZ / 1250. /, ZALDEF / 1400. /
C                                           ROUGHLY 3.6MM
      INTEGER IDTMAX / 600 /
C
      Z = 0.
      W = 1.
      IFLAG = 16
C
      AL = HW(IP+2)/8.
      AR = HW(IP+1)/8.
C                                          check whether CZSCAL has been
C                                          initialised
      IF  AL.GT.0. .AND. AR.GT.0.
      THEN
        IF  ZALPDI .EQ. ZALDEF
        THEN
          Z = ZALPDI*(AL-AR)/(AL+AR)
          IF NZSPRD .LE. 2
          THEN
            W = ( AZSSAV(NZSPRD) /
     *              ( AZSRS0(NZSPRD)+
     *                AZSRSA(NZSPRD)*SQRT(AL**2+AR**2)/(AL+AR)**2
     *              )
     *          )**2
          ELSE
            W = (AZSSAV(NZSPRD)/
     *             (AZSRS0(NZSPRD)+AZSRSA(NZSPRD)/(AL+AR)))**2
          CIF
        ELSE
          Z = .5*ZAL*(AL-AR)/(AL+AR)
        CIF
        IF  ABS( Z ) .LT. EXTRMZ
        THEN
          IF NZSPRD.GT.2
          THEN
C                                           ANY CLOSE HIT?
            ISEC = 0
            NP = IP - 4
            IDT = HW(NP+3)-HW(IP+3)
            IF( NP.GT.NPJETC*2+100 .AND. HW(NP)/8 .EQ. HW(IP)/8.AND.
     *          IABS(IDT).LT.IDTMAX ) ISEC = ISEC + 1
            NP = IP + 4
            IDT = HW(NP+3)-HW(IP+3)
            IF( NP.LT.NPJETC*2+100+HW(NPJETC*2+99) .AND.
     *          HW(NP)/8 .EQ. HW(IP)/8.AND.
     *          IABS(IDT).LT.IDTMAX ) ISEC = ISEC + 1
            IF( ISEC .EQ. 0 ) IFLAG = 0
          ELSE
             IFLAG = 0
          CIF
        CIF
      CIF
      RETURN
      END
      SUBROUTINE AMPS2Z( IP, NPJETC, Z, W, IFLAG )
C-----------------------------------------------------------
C-     COPIED FROM F22ELS.ZSLIB.S   20.6.87   --------------
C-----------------------------------------------------------
C  VERSION OF 21/04/87         LAST MOD 05/06/87   E ELSEN
C  Convert the amplitudes stored in HW(IP+1) and HW(IP+2)
C  into Z and calculate the weight W associated with this
C  measurement.
C  NPJETC = IW(IBLN('JETC'))
C  Flag IFLAG is 0 if the hit passes some quality criteria
C  Weighting will only work if ZSFIT has been called. In that
C  case CZSCAL has been initialised ( ZALPDI=1400 )
C  Note the different effective wire length that is used
C  for the two calibrations.
C-----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
C                                           from zsfit
C                                           zal name conflict
C     COMMON /CZSCAL/ IPVERS,ZAL,RESFAC,SECH(5)
      COMMON /CZSCAL/ IPVERS, ZALPDI, RESFAC, SECH(5)
#include "czsprm.for"
C
C
#include "cjdrch.for"
C
      COMMON / BCS / HW(1)
C
      REAL EXTRMZ / 1250. /, ZALDEF / 1400. /
C                                           ROUGHLY 3.6MM
      INTEGER IDTMAX / 600 /
C
      Z = 0.
      W = 1.
      IFLAG = 16
C
      AL = HW(IP+2)/8.
      AR = HW(IP+1)/8.
C                                          check whether CZSCAL has been
C                                          initialised
      IF  AL.GT.0. .AND. AR.GT.0.
      THEN
        IF  ZALPDI .EQ. ZALDEF
        THEN
          Z = ZALPDI*(AL-AR)/(AL+AR)
          IF NZSPRD .LE. 2
          THEN
            W = ( AZSSAV(NZSPRD) /
     *              ( AZSRS0(NZSPRD)+
     *                AZSRSA(NZSPRD)*SQRT(AL**2+AR**2)/(AL+AR)**2
     *              )
     *          )**2
          ELSE
            W = (AZSSAV(NZSPRD)/
     *             (AZSRS0(NZSPRD)+AZSRSA(NZSPRD)/(AL+AR)))**2
          CIF
        ELSE
          Z = .5*ZAL*(AL-AR)/(AL+AR)
        CIF
        IF  ABS( Z ) .LT. EXTRMZ
        THEN
          IF NZSPRD.GT.2
          THEN
C                                           ANY CLOSE HIT?
            ISEC = 0
            NP = IP - 4
            IDT = HW(NP+3)-HW(IP+3)
            IF( NP.GT.NPJETC*2+100 .AND. HW(NP)/8 .EQ. HW(IP)/8.AND.
     *          IABS(IDT).LT.IDTMAX ) ISEC = ISEC + 1
            NP = IP + 4
            IDT = HW(NP+3)-HW(IP+3)
            IF( NP.LT.NPJETC*2+100+HW(NPJETC*2+99) .AND.
     *          HW(NP)/8 .EQ. HW(IP)/8.AND.
     *          IABS(IDT).LT.IDTMAX ) ISEC = ISEC + 1
            IF( ISEC .EQ. 0 ) IFLAG = 0
          ELSE
             IFLAG = 0
          CIF
        CIF
C                                           OVERFLOW HITS
        IF(IFLAG.EQ.0 .AND.
     *     ( HW(IP+1).EQ. 32760 .OR. HW(IP+2).EQ. 32760 ) ) IFLAG = 32
      CIF
      RETURN
      END
C   09/06/83 801291914  MEMBER NAME  CNEWID   (JADEGS)      SHELTRAN
      SUBROUTINE CNEWID(INDEX)
C
C        J. SPITZER                         86/10/16
C
C        HANDELS NEW ID CALIBRATION CONSTANTS WHEN AVAILABLE.
C        IN THE LATTER CASE SETS NEW RUN VERTEX TOO.
C        IT IS CALLED FOR EACH EVENT FROM KALIBR AFTER KLREAD WITH
C                                                            INDEX=0
C        EXTRA CALL AT ANY TIME WITH
C            INDEX=-1 : OLD CALIBRATION IS FORCED (CURRENT DEFAULT)
C                  -2 : NEW CALIBRATION WHEN AVAILABLE, OLD OTHERWISE
C               LE -3 : AS -2 BUT NEW RUN VERTEX IS NOT SET
C        THESE SETTINGS REMAIN IN EFFECT UNTIL AN OTHER CALL WITH
C        INDEX <0.   NEW OPERATION MODE SHOULD BE ASKED BEFORE
C        THE CALL TO KALIBR OTHERWEISE THE NEW SETTING WILL APPLY
C        STARTING AT THE NEXT EVENT ONLY. (AT THE CALLS WITH <0
C        JUST THE SETTING IS CHANGED, THE NORMAL TASKS ARE NOT
C        PERFORMED.)
C
C        THE NEW CALIBRATION IS IN EFFECT CURRENTLY ONLY IF JFETCH
C        IS USED FOR RECONSTRACTING THE HIT COORDINATES.
C        JFETCH HAS BEEN MODIFIED TO CALL THE NEW ROUTINE JFTNEW
C        IN CASE JESCAL POSITIVE (AS SET BY THIS ROUTINE).
C
C
C NEW CALIBRATION (ISTEAR=-2) DEFAULT 12.4.1987      J.S., J.O.
C LAST MOD   C.B.   29/01/88     PRINT BLANK LINE BEFORE MESSAGE
C
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
#include "calibr.for"
C
C
C     DATA ISTEAR/-1/,IRLAST/-99999/,IPLAST/-99999/,IPOINT/0/,IRPLST/0/
      DATA ISTEAR/-2/,IRLAST/-99999/,IPLAST/-99999/,IPOINT/0/,IRPLST/0/
C
      DATA ICLV /0/
      DATA NCAL1/0/,IPLHDR/0/,IPRLIM/100/,IPRLIN/0/
C
      COMMON/JSCALD/ JESCAL,JESKEY,JESDRW
      INTEGER NVERTS/0/,IVERTS/0/,NVRUN(300,2)/600*0/
      REAL XVRUN(300)/300*0./,YVRUN(300)/300*0./
      INTEGER IUPDJS(6,40),NUPDJS/0/
      REAL RUPDJS(6,40)
      EQUIVALENCE (IUPDJS(1,1),RUPDJS(1,1))
C
C
C-----------------------------------------------------------------------
C                        ----  CHANGE OF OPERATION MODE----
      IF INDEX.LT.0
      THEN
         ISTEAR=INDEX
         IF ISTEAR.LE.-2
         THEN
            PRINT 110,IRLAST
            IF ISTEAR.EQ.-2
            THEN
               PRINT 111
            ELSE
               PRINT 112
            CIF
         ELSE
            PRINT 120,IRLAST
         CIF
110   FORMAT(//,' **** USE NEW CALIBRATION WHEN AVAILABLE AFTER RUN',I7)
111   FORMAT(6X,'WITH NEW RUN VERTEX',//)
112   FORMAT(6X,'WITHOUT SETTING NEW RUN VERTEX',//)
120   FORMAT(//,' **** USE OF OLD CALIBRATION FORCED AFTER RUN',I7,//)
         RETURN
      CIF
C
C-----------------------------------------------------------------------
C
      IF NCAL1.EQ.0
      THEN
         NCAL1=1
         IPLHDR=IBLN('HEAD')
         JESKEY=54321
N     * GET POINTER OF NEW CALIBR. DATA IN ACALIB
         IPOINT=ICALIB(13)
      CIF
C-----------------------------------------------------------------------
C
      IPHDR2=IDATA(IPLHDR)*2
      IRUN=HDATA(IPHDR2+10)
C
C                --- NO NEW CALIBRATION IF OLD IS FORCED OR MONTE CARLO
C
      IF ISTEAR.GT.-2 .OR. IRUN.LE.100
      THEN
         JESCAL=0
         RETURN
      CIF
C
C-----------------------------------------------------------------------
C    --- CHECK IF VALID CALIBRATION RECORD PRESENT ---
      ICDNUM=ICALIB(IPOINT+1)
      IF ICDNUM.LT.654320.OR.ICDNUM.GT.654322
      THEN
         IF IPRLIN.LT.IPRLIM
         THEN
            PRINT 9821,IRUN
9821        FORMAT(' *** NO VALID RECORD FOR NEW CALIBRATION',
     +      ' PRESENT AT RUN',I8)
            IPRLIN=IPRLIN+1
         CIF
         JESCAL=0
         RETURN
      CIF
C
C-----------------------------------------------------------------------
C                              --- NEW OR FIRST RUN ---
      IF IRUN.NE.IRLAST
      THEN
         IRLAST=IRUN
         IF ICALIB(IPOINT+2).NE.IPLAST
         THEN
C                            ---  NEW CAL. PREIOD ---
            IPLAST=ICALIB(IPOINT+2)
            IRPLST=ICALIB(IPOINT+3)
            IF ICDNUM.EQ.654320
            THEN
C                            ---  NO NEW CAL. AVAILABLE YET ---
               JESCAL=0
               PRINT 721, IPLAST,IRPLST
721            FORMAT(/' **** OLD ID CALIBRATION WILL BE USED FOR',
     +         ' RUNS',I6,' TO',I6)
            ELSE
C                            ---  THERE IS NEW CALIBRATION ---
               JESCAL=IPLAST
C                     --- COPY RUN VERTICES ---
               NVERTS=ICALIB(IPOINT+4)
               IF NVERTS.GT.0
               THEN
                  FOR J=1,NVERTS
                     NVRUN(J,1)=ICALIB(3*J+IPOINT+10275)
                     XVRUN(J)=  ACALIB(3*J+IPOINT+10276)
                     YVRUN(J)=  ACALIB(3*J+IPOINT+10277)
                  CFOR
C
                  NVRUN(1,1)=IPLAST
                  I=1
                  WHILE I.LT.NVERTS
                     NVRUN(I,2)=NVRUN(I+1,1)-1
                     I=I+1
                  CWHILE
                  NVRUN(NVERTS,2)=IRPLST
C                     --- SELECT RUN VERTEX PERIOD ---
                  IVERTS=1
                  WHILE IRUN.GT.NVRUN(IVERTS,2)
                     IVERTS=IVERTS+1
                  CWHILE
               CIF
C                            --- COPY UPDATES ---
               NUPDJS=ICALIB(IPOINT+5)
               IF NUPDJS.GT.0
               THEN
                  FOR J2=1,NUPDJS
                     FOR J1=1,6
                        IUPDJS(J1,J2)=ICALIB(6*J2+J1+IPOINT+11171)
                     CFOR
                  CFOR
               CIF
C                    --- NO/YES DISTORTIONS AVAILABLE ---
               JESDRW=ICDNUM-654321
               PRINT 720, JESCAL,IRPLST
720            FORMAT(/' **** NEW ID CALIBRATION WILL BE USED FOR',
     +         ' RUNS',I6,' TO',I6)
            CIF
         CIF
C                      --- CHECK IF RUN OUT OF RANGE ---
         IF IRUN.LT.IPLAST.OR.IRUN.GT.IRPLST
         THEN
            IF IPRLIN.LT.IPRLIM
            THEN
               PRINT 9823,IRUN,IPLAST,IRPLST
9823           FORMAT(' *** RUN',I7,' OUT OF RANGE OF CURRENT',
     +         ' CALIBRATION PERIOD:',I7,' TO',I7)
               IF(JESCAL.GT.0) PRINT 9824
9824           FORMAT(' FIRST RUN IN THIS PERIOD ASSUMED FOR',
     +         ' VERTEX AND UPDATES')
               IPRLIN=IPRLIN+1
            CIF
            IRUN=IPLAST
         CIF
C                      --- CHECK IF CHANGE IN RUN VERTEX PERIOD ---
         IF JESCAL.GT.0 .AND. NVERTS.GT.0
         THEN
            IF IRUN.GT.NVRUN(IVERTS,2)
            THEN
               REPEAT
                  IVERTS=IVERTS+1
               UNTIL IRUN.LE.NVRUN(IVERTS,2)
            ELSE
               IF IRUN.LT.NVRUN(IVERTS,1)
               THEN
                  REPEAT
                     IVERTS=IVERTS-1
                  UNTIL IRUN.GE.NVRUN(IVERTS,1)
               CIF
            CIF
         CIF
C             --- HANDLE UPDATES (WITHIN CURRENT CAL. PER.) ---
         IF JESCAL.GT.0 .AND. NUPDJS.GT.0
         THEN
            FOR J=1,NUPDJS
               IND=IUPDJS(4,J)*96+IUPDJS(3,J)-91+IPOINT
               IF IRUN.GE.IUPDJS(1,J).AND.IRUN.LE.IUPDJS(2,J)
               THEN
                  ACALIB(IND)=RUPDJS(5,J)
               ELSE
                  ACALIB(IND)=RUPDJS(6,J)
               CIF
            CFOR
         CIF
      CIF
C
C-----------------------------------------------------------------------
C                            --- SET RUN VERTEX FOR EACH EVENT ---
      IF JESCAL.GT.0 .AND. NVERTS.GT.0 .AND. ISTEAR.EQ.-2
      THEN
N *** SET RUN VERTEX
      ICLV = ICLV + 1
      IF(ICLV.EQ.1) WRITE(6,864)
864   FORMAT('  CNEWID ***:  RUN VERTICES CORRESPONDING TO NEW ID CALIBR
     $ATION SET IN /CALIBR/')
         IPV  = ICALIB(10)
         ACALIB(IPV+ 1) = XVRUN(IVERTS)
         ACALIB(IPV+ 3) = YVRUN(IVERTS)
      CIF
C
      RETURN
      END
C   09/06/83 703131344  MEMBER NAME  DISTXY   (S)           SHELTRAN
      FUNCTION DISTXY(X,Y,DX,DY,R,X0,Y0,XP,YP,PHIP)
C                                   J. SPITZER
C     INPUT:
C        X,Y,DX,DY  POSITION AND DIRECTION FOR 1. MEASURED POINT
C        R= 1/CURVATURE
C        X0,Y0 ANY POINT
C     OUTPUT
C        DISTXY  CLOSEST DISTANCE OF TRACK TO (X0,Y0)
C        XP,YP,PHIP (0,2PI) CLOSEST POINT AND ANGLE OF TRACK THERE
C
      DATA PIVALU/3.141593/
      ALEN=SQRT(DX**2+DY**2)
      SINOM=DY/ALEN
      COSOM=DX/ALEN
      G=(X0-X)**2+(Y0-Y)**2+2.*R*((Y0-Y)*COSOM-(X0-X)*SINOM)
      DISTXY=SQTVAL(ABS(R),G,1.,1.E-4)
      IF R.GT.0.
      THEN
         SGNR=1.
      ELSE
         SGNR=-1.
      CIF
      A1=DISTXY/ABS(R)
      A2=1./(1.+A1)
      XP=A2*(X0+X*A1+SINOM*SGNR*DISTXY)
      YP=A2*(Y0+Y*A1-COSOM*SGNR*DISTXY)
      SINOMP=A2*((X-X0)/R+SINOM)
      COSOMP=A2*((Y0-Y)/R+COSOM)
      IF(COSOMP.GT.1.) COSOMP=1.
      IF(COSOMP.LT.-1.) COSOMP=-1.
      PHIP=ACOS(COSOMP)
      IF(SINOMP.LT.0.) PHIP=2.*PIVALU-PHIP
      RETURN
      END
C   12/12/79 106101754  MEMBER NAME  EXJHTL   (JADEGS)      SHELTRAN
C   10/09/79 912120822  MEMBER NAME  EXJHTL   (JADESR)      SHELTRAN
C   06/09/79 C9091001   MEMBER NAME  EXJHTL   (JADESR)      SHELTRAN
      SUBROUTINE EXJHTL(IERR)
C
C     SUBROUTINE FOR EXTENSION OF HIT LABEL ARRAY IN /BCS/
C     PETER STEFFEN   6/ 9/79
C
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
C
#include "cdata.for"
C
C2007 FORMAT(1H ,12X,2I6,10(4X,Z4))
C2008 FORMAT(' HIT LABEL OF TRELS:',4I6,/,(12X,20(2X,Z4)))
C
N     INITIALIZE ERROR FLAG
      IERR = 0
      IPJHTL = IDATA(IBLN('JHTL'))
      IPJETC = IDATA(IBLN('JETC'))
      IF(IPJETC.LE.0) RETURN
      IF(IPJHTL.LE.0) RETURN
N     # OF HITS
      NHIT  = (HDATA(IPJETC*2+99)) / 4
C
N     LOOP OVER ALL EXISTING 'JHTL'-BANKS
      REPEAT
        NWHTL = IDATA(IPJHTL)
C
N       PRINTOUT
        I0 = IPJHTL*2 + 1
        I9 = IDATA(IPJHTL)*2 + I0 - 1
C       PRINT 2008, NHIT,NWHTL,I0,I9,(HDATA(I1),I1=I0,I9)
C
N       'JHTL' ALREADY IN NEW FORMAT ?
        IF(NWHTL.GE.NHIT) RETURN
C
N       EXTEND LENGTH
        NWDIFF = NHIT+1 - NWHTL
        CALL BCHM(IPJHTL,NWDIFF,IERR)
        IF(IERR.NE.0) RETURN
C
N       REFORMAT 'JHTL'
        IP0 =  IPJHTL*2+2
        IP1 =  IP0 + NHIT
        IP2 =  IP0 + NHIT*2
        REPEAT
          LBHIT = HDATA(IP1)
          IF LBHIT.EQ.0
          THEN
            HDATA(IP2-1) = 0
            HDATA(IP2  ) = 0
          ELSE
            LBZ   = LAND (LBHIT, 1)
            LBB12 = SHFTR(LBHIT,13)
            LBB12 = SHFTL(LBB12, 9)
            LBHT1 = LAND (LBHIT,63)
            LBHT1 = LOR  (LBHT1,LBB12)
            IF(TBIT(LBHIT,25)) LBHT1 = BITON(LBHT1,23)
            LBHT2 = SHFTR(LBHIT, 6)
            LBHT2 = LAND (LBHT2,62)
            IF LBHT2.NE.0
            THEN
              LBHT2 = LOR  (LBHT2,LBZ)
              LBHT2 = LOR  (LBHT2,LBB12)
              IF(TBIT(LBHIT,19)) LBHT2 = BITON(LBHT2,23)
            CIF
            HDATA(IP2-1) = LBHT1
            HDATA(IP2  ) = LBHT2
          CIF
        DATA NPR /0/
        NPR = NPR + 1
C       IF(LBB12.NE.0) PRINT 2007, IP1,IP2,LBHIT,LBZ,LBB12,LBHT1,LBHT2
        IP2 = IP2 - 2
        IP1 = IP1 - 1
        UNTIL IP1.LE.IP0
C
N       PRINTOUT
        I0 = IPJHTL*2 + 1
        I9 = IDATA(IPJHTL)*2 + I0 - 1
C       PRINT 2008, NHIT,NWHTL,I0,I9,(HDATA(I1),I1=I0,I9)
C
      IPJHTL = IDATA(IPJHTL-1)
      UNTIL IPJHTL.LE.0
C
      RETURN
C
      END
C   19/08/82 601231124  MEMBER NAME  FITEVF   (JADEGS)      SHELTRAN
      SUBROUTINE FITEVF(MODE)
C---
C---     GENERAL REFIT OF ALL TRACKS IN R-PHI
C---     INPUT :
C---     MODE   = 0 : OVERWRITE OLD PATR-BANK WITH NEW RESULTS
C---     MODE   = 1 : CREATE NEW PATR-BANK WITH NEW RESULTS
C---                                           P. STEFFEN 82/08/19
C---
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
#include "cworkpr.for"
#include "cworkeq.for"
C
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(HPZR0 ,HPWRK(20)),(HPZR9 ,HPWRK(21)),(HLDZR ,HPWRK(22))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
C
C2000 FORMAT('0CALL FITEVF(',2I6,1X,I4,' )')
C2001 FORMAT(1H0,2I3,I8,2(I4,3F6.1,3F6.3),
C    ,     /,14X,I3,4E13.5,F6.2,I3,4E13.5,
C    ,     /,14X,I3,2F8.3,F6.1,I3,10X,6I3,8I6,2X,Z4)
C2003 FORMAT(' FRFITO RESULT:',F6.0,F6.3,F6.0,F6.3)
C
C
N     INITIALIZE POINTER
      DATA LBINIT /0/
      IF LBINIT.EQ.0
      THEN
        LBINIT = 1
        IQPATR = IBLN('PATR')
        IQJHTL = IBLN('JHTL')
      CIF
C
C
N       CHECK IF PATR-BANK
        IF(IDATA(IQPATR).LE.0) RETURN
C
N       CREATE NEW PATR BANK FOR MODE=1
        IF LAND(MODE,1) .NE. 0
        THEN
          IPPAT0 = IDATA(IQPATR)
          NBNK1  = IDATA(IPPAT0-2) - 1
          NWRD   = IDATA(IPPAT0)
          NBYTE  = NWRD*4
          CALL CCRE(IPPATR,'PATR',NBNK1,NWRD,IERR)
          IF IERR.NE.0
          THEN
C           PRINT 2900, IERR
 2900 FORMAT(' FITEVF(PS): CREATION OF NEW PATR-BANK RESULTED',
     ,       ' IN ERROR',I3)
            INDEX = 1
            RETURN
          CIF
C
N         COPY CONTENTS OF 'PATR'-BANK
          CALL MVCL(IDATA(IPPATR+1),0,IDATA(IPPAT0+1),0,NBYTE)
C
        CIF
C
      IPPATR = IDATA(IQPATR)
      IPTR1  = IDATA(IPPATR+1) + IPPATR
      LDTR   = IDATA(IPPATR+3)
      NTR    = IDATA(IPPATR+2)
      IPJHTL = IDATA(IQJHTL)
C     PRINT 2000,IPPATR,IPJHTL,MODE
C     CALL PRPATR
      IF NTR.GT.0
      THEN
C
        FOR ITR=1,NTR
C
N         CHECK IF PT > 20 MEV
          IF ABS(ADATA(IPTR1+25)) .LT. .007
          THEN
C
N           REFIT TRACK WITH VERTEX CONSTRAINT
            CALL REFIT(IPTR1,IPJHTL)
C           I0 = HPTR0
C           I9 = I0 + 47
C           PRINT 2001, (WRK(I1),I1=I0,I9)
C
N           REPLACE BANK IF GOOD RESULT
            IP    = HPTR0 - 1
            IF LAND(IWRK(IP+ 2),16).EQ.0
            THEN
              SIG   = 1000.
              SIG0  =    0.
            ELSE
              SIG   =  WRK(IP+23)
              SIG0  = ADATA(IPTR1+23)
              ANHT  = IWRK(IP+24)
              ANHT0 = IDATA(IPTR1+24)
C             PRINT 2003, ANHT,SIG,ANHT0,SIG0
              IF(ANHT/ANHT0.LT..8) SIG = SIG + .5
            CIF
            IF SIG0.GT.SIG .OR. SIG.LT..35
            THEN
C
              CALL MVC(IDATA(IPTR1+1),0,IWRK(HPTR0),0,112)
C
            CIF
          CIF
        IPTR1 = IPTR1 + LDTR
        CFOR
C
      CIF
      RETURN
C
      END
C   13/08/82 601231126  MEMBER NAME  FITEVR   (JADEGS)      SHELTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE FITEVR( NTRVTX, MODE )
C-----------------------------------------------------------------------
C
C        FIT TRACKS WITH CONSTRAINT TO RUN VERTEX
C                   AND COMMON Z-VERTEX (MODE + 2)
C        INPUT :
C        MODE   = 0 : OVERWRITE OLD PATR-BANK WITH NEW RESULTS
C        MODE   = 1 : CREATE NEW PATR-BANK WITH NEW RESULTS
C        MODE   + 2 : DO ALSO COMMON Z-FIT
C        MODE   + 4 : VERTEX WEAKLY CONSTRAINED (ERRFAC = 100.0)
C        MODE   + 8 : RERUN PATREC IN CASE OF BAD JHTL-BANK
C        MODE   +16 : NO VERTEX CONSTRAINT (ERRFAC = 1000.0 )
C        OUTPUT:
C        NTRVTX = NUMBER OF TRACKS FROM EVENT VERTEX
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
C
#include "calibr.for"
C
#include "cworkpr.for"
#include "cworkeq.for"
C
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(HPZR0 ,HPWRK(20)),(HPZR9 ,HPWRK(21)),(HLDZR ,HPWRK(22))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
C
        DATA LBINIT /0/
        DATA NPRMES /0/
C
N       INITIALIZATION
        IF LBINIT .LE.0
        THEN
C
          LBINIT = 1
          IQHEAD = IBLN('HEAD')
          IQPATR = IBLN('PATR')
          IQJHTL = IBLN('JHTL')
          IQZVTX = IBLN('ZVTX')
C
        CIF
C
N       IF MODE=8, ITERATE WITH NEW PATREC
        NITER = 0
        REPEAT
        NITER = NITER + 1
C
C
C
N         INITIALIZE NTRVTX
          NTRVTX = 0
C
N         CHECK IF PATR-BANK
          IF(IDATA(IQPATR).LE.0) RETURN
C
C
N         CREATE NEW PATR BANK
          IF LAND(MODE,1) .NE. 0
          THEN
            IPPAT0 = IDATA(IQPATR)
            NBNK1  = IDATA(IPPAT0-2) - 1
            NWRD   = IDATA(IPPAT0)
            NBYTE  = NWRD*4
            CALL CCRE(IPPATR,'PATR',NBNK1,NWRD,IERR)
            IF IERR.NE.0
            THEN
              PRINT 2900, IERR
 2900   FORMAT(' USFITO3(PS): CREATION OF NEW PATR-BANK RESULTED',
     ,         ' IN ERROR',I3)
              INDEX = 1
              RETURN
            CIF
C
N           COPY CONTENTS OF 'PATR'-BANK
            CALL MVCL(IDATA(IPPATR+1),0,IDATA(IPPAT0+1),0,NBYTE)
C
          CIF
C
          IPPATR = IDATA(IQPATR)
          IPTR1  = IDATA(IPPATR+1) + IPPATR
          LDTR   = IDATA(IPPATR+3)
          NTR    = IDATA(IPPATR+2)
C
N         CHECK IF 1 TRACK
          IF(NTR.LT.1) RETURN
C
N         SEARCH FOR TRACKS FROM MAIN VERTEX
          MTRV = 0
C
N         RUN VERTEX
          IPV  = ICALIB(10)
          X0   = ACALIB(IPV+ 1)
          Y0   = ACALIB(IPV+ 3)
C
          FOR ITR=1,NTR
C
N           CHECK IF MOMENTUM >100 MEV
            IF ABS(ADATA(IPTR1+25)).LT..00143 .AND.IDATA(IPTR1+24).GT.16
            THEN
C
N             MARK + COUNT TRACKS FROM MAIN VERTEX
              CALL DRTRCK(IPTR1,X0,Y0,DR0)
              IF ABS(DR0).LT.25. .OR. IDATA(IPTR1+4).EQ.1
              THEN
                MTRV = MTRV + 1
                IDATA(IPTR1+4) =-1
              CIF
            CIF
          IPTR1 = IPTR1 + LDTR
          CFOR
C
N         PERFORM SUPERFIT IF >1 TRACK
          IF(MTRV.LE.0) XREPEAT
            Z0 = 0.
            IPZVTX = IDATA(IQZVTX)
            IF(IPZVTX.GT.0.AND.IDATA(IPZVTX+6).EQ.3) Z0=ADATA(IPZVTX+1)
            ERRFAC = 1.0
            IF(LAND(MODE,4)  .NE. 0) ERRFAC =  100.0
            IF(LAND(MODE,16) .NE. 0) ERRFAC = 1000.0
            MODEF = MODE
            CALL FZFITV(IPPATR,IDATA(IQJHTL),NTRVTX,Z0,ERRFAC,MODEF)
C     DATA NPRPAT/0/
C     NPRPAT = NPRPAT + 1
C     IF(NPRPAT.LE.2) CALL PRPATR
C
C
N           STOP IF NO BAD TRACK
            IF(MODEF       .GE.0) XREPEAT
N           ERROR IN JHTL-BANK, CHECK IF ITER. WITH NEW PATREC
            IPHEAD = IDATA(IQHEAD) * 2
            NPRMES = NPRMES + 1
            IF(NPRMES.LE.10)
     ,      PRINT 2009, HDATA(IPHEAD+10),HDATA(IPHEAD+11),MODEF
 2009 FORMAT(' FITEVR(PST): ERROR IN JHTL-BANK FOR EVENT',3I6)
            IF(LAND(MODE,8).EQ.0) XREPEAT
N           STOP AFTER 1. ITERATION
            IF(NITER       .GE.2) XREPEAT
C
N           DELETE NEW CREATED PATR-BANK
            IF LAND(MODE,1).NE.0
            THEN
              IPPATR = IDATA(IQPATR)
              NBNK   = IDATA(IPPATR-2)
              CALL BDLS('PATR',NBNK)
            CIF
C
N           DELETE OLD PATR-BANK + JHTL-BANK
            IPPATR = IDATA(IQPATR)
            NBNK   = IDATA(IPPATR-2)
            CALL BDLS('PATR',NBNK)
            IPJHTL = IDATA(IQPATR)
            CALL BDLS('JHTL',NBNK)
C
N           REPEAT PATREC
            IND = 1
            CALL PATRCO(IND)
C
        UNTIL NITER .GE.2
C
      RETURN
C
      END
C   16/08/82 608111933  MEMBER NAME  FRFITV   (JADEGS)      SHELTRAN
C   18/02/81 208111542  MEMBER NAME  REFITV   (JETCALSR)    SHELTRAN
      SUBROUTINE FRFITV(IPTR,IPJHTL,ERRFAC)
C
C        REFIT TRACK ITRK IN 'PATR'-BANK USING ORIGIN
C                   ONLY INTERMEDIATE VALUES STORED
C                   FOR POSITION + DIRECTION AT 1. AND LAST HIT
C                   THIS ROUTINE IS ONLY USED WITH SUBSEQUENT ZRFIT
C                   USE REFITV IF ONLY R-PHI-FIT WANTED
C        P. STEFFEN                    22/08/80
C  CHANGED 11.8.86  = 32  IS REPLACED WITH OR(, 32)    J.OLSSON
C
      IMPLICIT INTEGER*2 (H)
      LOGICAL DEADCL
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
C
      INTEGER DATE(5), IDAY /0/
      DIMENSION  NHTRNG(3)
C
N     JET-CHAMBER AND VERTEX RESOLUTION
      DATA RESJ0 /.200/, RESV0 /.300/
C
N     MASK FOR L/R BIT IN HIT LABEL
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
C
N     MASK FOR TRACKS AT CELL WALL
      INTEGER MKBDCL(3) /Z10,Z20,Z40/
      INTEGER MKDDCL(3) /Z01,Z02,Z04/
C
C     IF(IDATA(IPTR+1).LT. 4) RETURN
C     I0 = IPTR + 1
C     I9 = IPTR + 48
C     PRINT 2001, (IDATA(I1),I1=I0,I9)
C     I0 = IPJHTL*2 + 1
C     I9 = I0 + IDATA(IPJHTL)*2 - 1
C     PRINT 2000, IPJHTL,I0,I9,(HDATA(I1),I1=I0,I9)
C     IPJETC = IDATA(IBLN('JETC'))
C     I0 = IPJETC*2 + 1
C     I9 = I0 + 109
C     PRINT 2000, IPJETC,I0,I9,(HDATA(I1),I1=I0,I9)
C2000 FORMAT('0REFIT:',3I8,/,(20(1X,Z4)))
C2001 FORMAT(1H0,2I3,I8,2(I4,3F6.1,3F6.3),
C    ,     /,14X,I3,4E13.5,F6.2,I3,4E13.5,
C    ,     /,14X,I3,2F8.3,F6.1,I3,10X,6I3,8I6,2X,Z4)
C2002 FORMAT('0FETCH:',2I3,2I5,12F9.5)
C2003 FORMAT('0ROTATION:',12F10.5)
C2004 FORMAT('0CIRC.CENTRE:',2I3, F10.5,2F10.0,F8.1,2F8.1)
C2005 FORMAT('0TRACK:',I6,/,(1X,3I6,4F8.3,I4,F8.3,2I4,F8.3,I6,F8.2))
C2006 FORMAT(1X,I6,5F8.3,F12.1,5F8.3)
C2007 FORMAT(' FETCH:',I3,9F8.4,F10.5,F6.0)
C2008 FORMAT(' FIT:',2I3,2F8.3,F5.0,3E12.5,F6.3,F6.3)
C2009 FORMAT(' JHTL:',I8,1X,Z8,3I5)
C2010 FORMAT(' HIT:',I6,12F8.2)
C2011 FORMAT('0ABERR:',10F10.6)
C2012 FORMAT('0ERROR:',10E13.6)
C2014 FORMAT('0FIT-BANK:',5F8.3,5X,5F8.3,5X,F8.5,2F8.1)
C2016 FORMAT('0ITRCLL =',6I8,/,(9X,6F8.3))
C2107 FORMAT(' SIGLM:',10F8.3)
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
        LBINIT = 1
        PERFORM INIT
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
      HPFRE1 = HPFREE
C
N     GET X-Y-VERTEX AND DETERMINE ERROR
      IPV    = ICALIB(10)
      XO     = ACALIB(IPV+ 1)
      YO     = ACALIB(IPV+ 3)
C     I0 = IPV + 1
C     I9 = IPV + 6
C     PRINT 2029, XO,YO,(ACALIB(I1),I1=I0,I9)
      PTRANS = ABS(0.0299792458*BKGAUS/ADATA(IPTR+25)) * .001
      RESV   = RESV0**2 + RESMS / PTRANS**2
      WGHT0  = RESJ0**2 / RESV
      F1     = ERRFAC
      IF(F1 .LT. .10) F1 = .10
      WGHT0  = WGHT0 / F1**2
C     PRINT 2029, XO,YO,WGHT0,F1,RESV,RESMS,PTRANS
C2029 FORMAT(' VERTEX',9E13.5)
C     PRINT 2011,ABERR
C
C
N     FETCH HITS, CALCULATE COORDINATES, AND
N     FILL ARRAY IN /CWORK/
      HPCO0  = HPFREE
      LHIT   = 14
      INDFET = 3
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
      X0     = WRK (IPRES+ 9)
      Y0     = WRK (IPRES+10)
      XOR    =- XT*CSROT -  YT*SNROT
      YOR    =  XT*SNROT -  YT*CSROT
C     PRINT 2003, CSROT,SNROT,XX,YY,XT,YT,X0,Y0,XO,YO,XOR,YOR,WGHT0
C
N     INITIALIZE FIT PARAMETERS IN CWORK
      WRK(HPAR0+ 4) = 0.
      WRK(HPAR0+ 5) = 0.
      WRK(HPAR0+ 6) = 0.
      WRK(HPAR0+ 7) = 1000.
      WRK(HPAR0+ 8) = 0.
      CSTH   = WRK (IPRES+11)
      SNTH   = WRK (IPRES+12)
C     PRINT 2007, HPCO0,HPCO9,HDLCO,HPFREE,(WRK(I1),I1=HPAR0,HPAR9)
C
C
N     COPY TRACK BANK
      HPTR0 = HPFREE
      CALL MVC(IWRK(HPTR0),0,IDATA(IPTR+1),0,192)
      IWRK(HPTR0+ 1) = 0
      HPFREE = HPFREE + 48
      IWRK(HPTR0+1) = 0
C
C       PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
C
N     1. PARABOLA FIT
N     LAST RING INCLUDED IN FIT
      JRINGL = 3
      PERFORM FPARA0
C
N     RELABEL HITS
      ALBLM1 = 0.6
      ALBLM2 = 3.0
      PERFORM LABEL
      WHILE NHGOOD.LT.8 .AND. NHFIT-NHGOOD.GT.8
        ALBLM1 = ALBLM1 + 1.0
        ALBLM2 = ALBLM2 + 1.0
        PERFORM LABEL
      CWHILE
      ALBLM1 = 0.6
      ALBLM2 = 3.0
C     PRINT 2005, NHFIT,(WRK(I),I=HPCO0,HPCO9)
C

      REPEAT
N       REFIT PARABOLA
        PERFORM FPARA0
C
N       RELABEL HITS
        PERFORM LABEL
C
N       REPEAT FIT IF >3 NEW GOOD HITS
      UNTIL NHGOOD-NHFIT .LT.4
C
C     PRINT 2005, NHGOOD,(WRK(I),I=HPCO0,HPCO9)
C
N     SET UP FIT-BANK
      LBADFT = 0
      IF(IDATA(IPTR+24)-NHGOOD   .GT. 8) LBADFT = 1
      IF(IDATA(IPTR+24) .GT. 1.3*NHGOOD) LBADFT = 1
      IF SIG.LT.1. .AND. LBADFT .EQ.0
      THEN
        PERFORM FITBNK
      CIF
C
N     CHECK IF BAD FIT AND LOW MOMENTUM
      IF ABS(PAR1).GT..00030 .AND. NHTRNG(1)+NHTRNG(2).GT.16
      THEN
        ALBLM1 = 1.5
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 2
        PERFORM FPARA0
        ALBLM1 = 0.6
        PERFORM LABEL
        PERFORM FPARA0
        PERFORM LABEL
        LBADFT = 0
        IF(IDATA(IPTR+24)-NHGOOD        .GT.   8) LBADFT = 1
        IF(IDATA(IPTR+24)/FLOAT(NHGOOD) .GT. 1.3) LBADFT = 1
        IF SIG.LT..10 .AND. LBADFT .EQ.0
        THEN
          PERFORM FITBK1
          IWRK(IP+ 4) = 32
        CIF
      CIF
      IF ABS(PAR1).GT..00150 .AND. NHTRNG(1)+NHTRNG(2).GT.9
      THEN
        ALBLM1 = 1.5
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 1
        NHTFIT = NHTRNG(1)
        IF NHTFIT.LE.5
        THEN
          NHTFIT = NHTFIT + NHTRNG(2)
          JRINGL = 2
        CIF
        IF NHTFIT.GT.9
        THEN
          PERFORM FPARA0
          ALBLM1 = 0.6
          PERFORM LABEL
          PERFORM FPARA0
          PERFORM LABEL
          LBADFT = 0
          IF(IDATA(IPTR+24)-NHGOOD        .GT.   8) LBADFT = 1
          IF(IDATA(IPTR+24)/FLOAT(NHGOOD) .GT. 1.3) LBADFT = 1
          IF SIG.LT..10 .AND. LBADFT .EQ.0
          THEN
            PERFORM FITBK1
            IWRK(IP+ 4) = 48
          CIF
        CIF
      CIF
C
C     PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
      HPFREE = HPFRE1
      RETURN
C
N     *************************
N     *      F P A R A 0      *
N     *************************
C
C
N     PARABOLA FIT THROUG ORIGIN
      PROC FPARA0
C
N     GET EQUATIONS
N     WEIGHT ORIGIN AS POINT OF PARABOLA
      S0 = WGHT0
      S1 = X0*WGHT0
      S2 = S1*X0
      S3 = S2*X0
      S4 = S3*X0
      S7 = Y0 * WGHT0
      S6 = S7*X0
      S5 = S6*X0
      IPCO = HPCO0
      REPEAT
       IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
       THEN
          X = WRK(IPCO+3)
          Y = WRK(IPCO+4)
          X2 = X**2
          S1 = S1 + X
          S2 = S2 + X2
          S3 = S3 + X*X2
          S4 = S4 + X2**2
          S5 = S5 + Y*X2
          S6 = S6 + Y*X
          S7 = S7 + Y
          S0 = S0 + 1.
        CIF
      IPCO = IPCO + HLDCO
      UNTIL IPCO.GT.HPCO9
      IF S0.LT.2.5
      THEN
        SIG = 1000.
      ELSE
C
N       SOLVE EQUATIONS FOR PARABOLA FIT
        F1 = 1. / S4
        XX12 = S3*F1
        XX13 = S2*F1
        YY1  = S5*F1
        XX22 = S2 - S3*XX12
        XX23 = S1 - S3*XX13
        YY2  = S6 - S3*YY1
        XX32 = S1 - S2*XX12
        XX33 = S0 - S2*XX13
        YY3  = S7 - S2*YY1
        IF XX22.GT.XX32
        THEN
          XX23 = XX23 / XX22
          YY2  = YY2  / XX22
          PAR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
          PAR2 = YY2 - XX23*PAR3
        ELSE
          XX33 = XX33 / XX32
          YY3  = YY3  / XX32
          PAR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
          PAR2 = YY3 - XX33*PAR3
        CIF
        PAR1 = YY1 - XX12*PAR2 - XX13*PAR3
        DEG   = S0 - WGHT0 - 2.
        NHFIT = S0 - WGHT0 + .1
C
C
N       CALC. CHISQ + SOLVE L/R AMBIGUITY
        CHISQ = 0.
        DCHIM1 = 0.
        IHITM1 = 0
        XST    = 999999.
        XEN    =-999999.
        IPCO = HPCO0
        REPEAT
         IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
         THEN
            X = WRK(IPCO+3)
            IF(X.LT.XST) XST = X
            IF(X.GT.XEN) XEN = X
            Y = WRK(IPCO+4)
            F = (PAR1 *X + PAR2 )*X + PAR3
            DCHI = Y - F
            WRK(IPCO+13) = DCHI
N           SUM FOR RMS
            CHISQ = CHISQ + DCHI**2
N           KEEP BIGGEST RMS
C           IF ABS(DCHI).GE.DCHIM1
C           THEN
C             DCHIM1 = ABS(DCHI)
C             IHITM1 = IPCO
C           CIF
C     PRINT 2006, IPCO,X,Y,F,DCHI,CHISQ
          CIF
        IPCO = IPCO + HLDCO
        UNTIL IPCO.GT.HPCO9
        SIG    =      CHISQ  / DEG
C     PRINT 2008, ITRK,NHFIT,XST,SIG,DEG,PAR1,PAR2,PAR3,WGHT0,Y0
C
N       SET LIMIT FOR SIGMA
        SIGLM = TRELLM(16)**2
      CIF
C
      CPROC
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
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
      YEN  = (PAR1 *XEN + PAR2 )*XEN + PAR3
N     DIRECTION AT START + END POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
      TGEN = PAR1*XEN*2 + PAR2
      DXEN = 1./SQRT(TGEN**2+1.)
      DYEN = DXEN * TGEN
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
C     CURV =-PAR1 * 2.
      CVZW = TGST**2+1.
      CVST =-PAR1 * 2 / (SQRT(CVZW)*CVZW)
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      IWRK(IP+ 1) = ITRK
      IWRK(IP+ 2) = LOR(IWRK(IP+ 2),32)
C     IWRK(IP+ 2) = 32
      IWRK(IP+ 3) = IDAY
      IWRK(IP+ 4) = 16
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = XST - X0
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)
      WRK (IP+10) = SNTH
      IWRK(IP+11) = 0
      WRK (IP+12) = XEN *CSROT - YEN *SNROT + XT
      WRK (IP+13) = XEN *SNROT + YEN *CSROT + YT
      WRK (IP+14) = XEN - X0
      WRK (IP+15) = (DXEN*CSROT - DYEN*SNROT)
      WRK (IP+16) = (DXEN*SNROT + DYEN*CSROT)
      WRK (IP+17) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
C     IF(SIG  .LT.0) PRINT 2021,WRK(IP+1),LBPR,S0,SIG
C2021 FORMAT(' -VE SQRT:',2I4,5E13.5)
      WRK (IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CVST
C     IF(SIG11.LT.0) PRINT 2021,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SQRT(SIG*SIG11) * 2.
      WRK (IP+27) = CVST
      WRK (IP+28) = CVST
      WRK (IP+31) = XOR - X0
C       I0 = IP+ 1
C       I9 = IP+48
C       PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
N     *************************
N     *      F I T B K 1      *
N     *************************
C
C
N     CHANGE FIT BANK (1.POINT)
      PROC FITBK1
C
N     START POINT
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
N     DIRECTION AT START POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
C     CURV =-PAR1 * 2.
      CVZW = TGST**2+1.
      CVST =-PAR1 * 2 / (SQRT(CVZW)*CVZW)
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV,
C    ,            XMIN,YMIN
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = XST - X0
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)
      WRK (IP+10) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
C     IF(SIG  .LT.0) PRINT 2022,WRK(IP+1),S0,SIG
C2022 FORMAT(' -VE SQRT(1):',I4,5E13.5)
      WRK (IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CVST
C     IF(SIG11.LT.0) PRINT 2022,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SQRT(SIG*SIG11) * 2.
      WRK (IP+27) = CVST
C     I0 = IP+ 1
C     I9 = IP+48
C     PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
C
N     *************************
N     *      L A B E L        *
N     *************************
C
C
N     LABEL USED HITS
      PROC LABEL
C
N       PRESET LAST HIT POINTER
        IWL = -999
        NHTRNG(1) = 0
        NHTRNG(2) = 0
        NHTRNG(3) = 0
C
N       PRESET LAST HIT POINTER
        IWL = -999
        NHGOOD = 0
        FOR IP = HPCO0,HPCO9,HLDCO
          IW0 = IWRK(IP)
          X   = WRK(IP+3)
          Y   = WRK(IP+4)
          F   = (PAR1*X + PAR2)*X + PAR3
          DF  = F - Y
N         SELECT CLOSEST HIT
          LBGOOD = 4
          IF(ABS(DF).LT.ALBLM2) LBGOOD = 1
          IF(ABS(DF).LT.ALBLM1) LBGOOD = 0
          IWRK(IP+ 10) = LBGOOD
          IF(LBGOOD.EQ.0) NHGOOD = NHGOOD + 1
          WRK (IP+13) = DF
          IRNG = IWRK(IP+12)
          IF(LBGOOD.LE.1) NHTRNG(IRNG) = NHTRNG(IRNG) + 1
C
N         CHECK IF 2 HITS FROM SAME WIRE
          IF IWL.EQ.IW0
          THEN
N           SELECT CLOSEST HIT
            IF ABS(DFL).LT.ABS(DF)
            THEN
              IF(IWRK(IP +10).EQ.0) NHGOOD = NHGOOD - 1
              IWRK(IP +10) = 16
            ELSE
              IF(IWRK(IPL+10).EQ.0) NHGOOD = NHGOOD - 1
              IWRK(IPL+10) = 16
            CIF
          CIF
N         STORE LAST POINTERS + DF
          IWL = IW0
          IPL = IP
          DFL = DF
        CFOR
C
      CPROC
C
C
N     *************************
N     *      I N I T          *
N     *************************
C
C
N     INITIALIZE CONSTANTS
      PROC INIT
C
        IQJETC = IBLN('JETC')
        IQHEAD = IBLN('HEAD')
C
        CALL DAY2(DATE)
        IDAY = DATE(1)*1000 + DATE(2)
C
N       MULT. SCATTERING CONSTANTS
        RESMS = .020**2/2. * .16 * (1. + ALOG10(.16) / 9) * 155.45**2
      CPROC
C
      END
      SUBROUTINE FRFITV(IPTR,IPJHTL,ERRFAC)
C
C        REFIT TRACK ITRK IN 'PATR'-BANK USING ORIGIN
C                   ONLY INTERMEDIATE VALUES STORED
C                   FOR POSITION + DIRECTION AT 1. AND LAST HIT
C                   THIS ROUTINE IS ONLY USED WITH SUBSEQUENT ZRFIT
C                   USE REFITV IF ONLY R-PHI-FIT WANTED
C        P. STEFFEN                    22/08/80
C
      IMPLICIT INTEGER*2 (H)
      LOGICAL DEADCL
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
C
      INTEGER DATE(5), IDAY /0/
      DIMENSION ITRCLL(6), NCNCK(24), NHTRNG(3)
C
N     JET-CHAMBER AND VERTEX RESOLUTION
      DATA RESJ0 /.200/, RESV0 /.300/
C
N     MASK FOR L/R BIT IN HIT LABEL
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
C
N     MASK FOR TRACKS AT CELL WALL
      INTEGER MKBDCL(3) /Z10,Z20,Z40/
      INTEGER MKDDCL(3) /Z01,Z02,Z04/
C
C     IF(IDATA(IPTR+1).LT. 4) RETURN
C     I0 = IPTR + 1
C     I9 = IPTR + 48
C     PRINT 2001, (IDATA(I1),I1=I0,I9)
C     I0 = IPJHTL*2 + 1
C     I9 = I0 + IDATA(IPJHTL)*2 - 1
C     PRINT 2000, IPJHTL,I0,I9,(HDATA(I1),I1=I0,I9)
C     IPJETC = IDATA(IBLN('JETC'))
C     I0 = IPJETC*2 + 1
C     I9 = I0 + 109
C     PRINT 2000, IPJETC,I0,I9,(HDATA(I1),I1=I0,I9)
C2000 FORMAT('0REFIT:',3I8,/,(20(1X,Z4)))
C2001 FORMAT(1H0,2I3,I8,2(I4,3F6.1,3F6.3),
C    ,     /,14X,I3,4E13.5,F6.2,I3,4E13.5,
C    ,     /,14X,I3,2F8.3,F6.1,I3,10X,6I3,8I6,2X,Z4)
C2002 FORMAT('0FETCH:',2I3,2I5,12F9.5)
C2003 FORMAT('0ROTATION:',12F10.5)
C2004 FORMAT('0CIRC.CENTRE:',2I3, F10.5,2F10.0,F8.1,2F8.1)
C2005 FORMAT('0TRACK:',I6,/,(1X,3I6,4F8.1,I4,F6.2,2I4,F8.3,I6,F8.1))
C2006 FORMAT(1X,I6,5F8.2,F12.1,5F8.2)
C2007 FORMAT(' FETCH:',I3,9F8.4,F10.5,F6.0)
C2008 FORMAT(' FIT:',2I3,F8.2,F5.0,F10.6,F7.3,F5.1,F6.3,F5.1)
C2009 FORMAT(' JHTL:',I8,1X,Z8,3I5)
C2010 FORMAT(' HIT:',I6,12F8.2)
C2011 FORMAT('0ABERR:',10F10.6)
C2012 FORMAT('0ERROR:',10E13.6)
C2014 FORMAT('0FIT-BANK:',5F8.3,5X,5F8.3,5X,F8.5,2F8.1)
C2016 FORMAT('0ITRCLL =',6I8,/,(9X,6F8.3))
C2107 FORMAT(' SIGLM:',10F8.3)
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
        LBINIT = 1
        PERFORM INIT
      CIF
C
N     RESERVE SPACE IN CWORK
      HPFREE = 1
      HPFRE1 = HPFREE
      HPCO0  = HPFREE
      HLDCO  = 14
      HPFREE = HLDCO*100 + HPCO0
      HPCO9 = HPFREE - 1
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
C
N     COPY TRACK BANK
      HPTR0 = HPFREE
      CALL MVC(IWRK(HPTR0),0,IDATA(IPTR+1),0,192)
      IWRK(HPTR0+1) = 0
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
N     CENTRE OF CIRCLE (USED FOR ANGULAR CORRECTION)
      IF IDATA(IPTR+18).EQ.1
      THEN
N       CIRCLE PARAMETERS
        ALFA  = ADATA(IPTR+21)
        CRV   = ADATA(IPTR+19)
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        RAD   =  1./CRV + ADATA(IPTR+20)
        XCIRC = COS(ALFA) * RAD
        YCIRC = SIN(ALFA) * RAD
        CHARGE = SIGN(1.,ADATA(IPTR+25))
      ELSE
N       PARABOLA PARAMETERS
        CRV   = ADATA(IPTR+22)*2.
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        ALFA  = ADATA(IPTR+19)
        XCIRC =-SIN(ALFA)/CRV + ADATA(IPTR+20)
        YCIRC = COS(ALFA)/CRV + ADATA(IPTR+21)
        CHARGE =-SIGN(1.,ADATA(IPTR+22))
      CIF
C
N     ZVERT, THETA + DIR. COSINES
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTHI = SQRT(TGTH**2 + 1.)
      CSTH  = 1. / CSTHI
      SNTH  = CSTH * TGTH
C     PRINT 2004,ITRK,IDATA(IPTR+18),ALFA,XCIRC,YCIRC,ZVERT,TGTH,CSTHI
C
N     GET X-Y-VERTEX AND DETERMINE ERROR
      IPV    = ICALIB(10)
      XO     = ACALIB(IPV+ 1)
      YO     = ACALIB(IPV+ 3)
C     I0 = IPV + 1
C     I9 = IPV + 6
C     PRINT 2029, XO,YO,(ACALIB(I1),I1=I0,I9)
      PTRANS = ABS(0.0299792458*BKGAUS/ADATA(IPTR+25)) * .001
      RESV   = RESV0**2 + RESMS / PTRANS**2
      WGHT0  = RESJ0**2 / RESV
      F1     = ERRFAC
      IF(F1 .LT. .10) F1 = .10
      WGHT0  = WGHT0 / F1**2
C     PRINT 2029, XO,YO,WGHT0,F1,RESV,RESMS,PTRANS
C2029 FORMAT(' VERTEX',9E13.5)
C     PRINT 2011,ABERR
C
N     ROTATION ANGLE (USING LAST POINT OF TRACK)
      XT    = (ADATA(IPTR+12) + XO) * .5
      YT    = (ADATA(IPTR+13) + YO) * .5
      XX    =  ADATA(IPTR+12) - XO
      YY    =  ADATA(IPTR+13) - YO
      RR    = SQRT(XX**2+YY**2)
      CSROT = XX / RR
      SNROT = YY / RR
      XOT = XO - XT
      YOT = YO - YT
      X0   = XOT*CSROT + YOT*SNROT
      Y0   =-XOT*SNROT + YOT*CSROT
      XOR  =- XT*CSROT -  YT*SNROT
      YOR  =  XT*SNROT -  YT*CSROT
C     PRINT 2003, CSROT,SNROT,XX,YY,XT,YT,X0,Y0,XO,YO,WGHT0
C
N     FILL CELL ARRAY
      PERFORM SELCLL
C
N     LOOP OVER ALL CELLS + FETCH HITS
      KCLL = 0
      NHIT = 0
      IPCO = HPCO0
C
N     LOOP OVER RINGS
      JRING = 0
N     TRACKS AT CELL WALLS
      LBCELL = 0
      REPEAT
      JRING = JRING + 1
        NHTRNG(JRING) = 0
        NHRNG = 0
        NCLL = 0
        REPEAT
        NCLL = NCLL + 1
        KCLL = KCLL + 1
          JCELL = ITRCLL(KCLL)
          IF JCELL.NE.0
          THEN
            PERFORM FETCH
            NHRNG = NHRNG + JHIT
          CIF
        UNTIL NCLL.EQ.2
N       SET LABEL FOR TRACK AT CELL BOUND.
        IF(JCELL.NE.0) LBCELL = LOR(MKBDCL(JRING),LBCELL)
      UNTIL KCLL.EQ.6
      HPCO9 = IPCO - 1
C     PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N     1. PARABOLA FIT
N     LAST RING INCLUDED IN FIT
      JRINGL = 3
      PERFORM FPARA0
C
N     RELABEL HITS
      ALBLM1 = 0.6
      ALBLM2 = 3.0
      PERFORM LABEL
C     PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N     REFIT PARABOLA
      PERFORM FPARA0
C
N     RELABEL HITS
      PERFORM LABEL
C     PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N     SET UP FIT-BANK
      IF SIG.LT.1.
      THEN
        PERFORM FITBNK
      CIF
C
N     CHECK IF BAD FIT AND LOW MOMENTUM
      IF ABS(PAR1).GT..00030 .AND. NHTRNG(1)+NHTRNG(2).GT.16
      THEN
        ALBLM1 = 1.5
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 2
        PERFORM FPARA0
        ALBLM1 = 0.6
        PERFORM LABEL
        PERFORM FPARA0
        PERFORM LABEL
        IF SIG.LT..10
        THEN
          PERFORM FITBK1
          IWRK(IP+ 4) = 32
        CIF
      CIF
      IF ABS(PAR1).GT..00150 .AND. NHTRNG(1)+NHTRNG(2).GT.9
      THEN
        ALBLM1 = 1.5
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 1
        NHTFIT = NHTRNG(1)
        IF NHTFIT.LE.5
        THEN
          NHTFIT = NHTFIT + NHTRNG(2)
          JRINGL = 2
        CIF
        IF NHTFIT.GT.9
        THEN
          PERFORM FPARA0
          ALBLM1 = 0.6
          PERFORM LABEL
          PERFORM FPARA0
          PERFORM LABEL
          IF SIG.LT..10
          THEN
            PERFORM FITBK1
            IWRK(IP+ 4) = 48
          CIF
        CIF
      CIF
C
      HPFREE = HPFRE1
      RETURN
C
N     *************************
N     *      F P A R A 0      *
N     *************************
C
C
N     PARABOLA FIT THROUG ORIGIN
      PROC FPARA0
C
N     GET EQUATIONS
N     WEIGHT ORIGIN AS POINT OF PARABOLA
      S0 = WGHT0
      S1 = X0*WGHT0
      S2 = S1*X0
      S3 = S2*X0
      S4 = S3*X0
      S7 = Y0 * WGHT0
      S6 = S7*X0
      S5 = S6*X0
      IPCO = HPCO0
      REPEAT
       IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
       THEN
          X = WRK(IPCO+3)
          Y = WRK(IPCO+4)
          X2 = X**2
          S1 = S1 + X
          S2 = S2 + X2
          S3 = S3 + X*X2
          S4 = S4 + X2**2
          S5 = S5 + Y*X2
          S6 = S6 + Y*X
          S7 = S7 + Y
          S0 = S0 + 1.
        CIF
      IPCO = IPCO + HLDCO
      UNTIL IPCO.GT.HPCO9
      IF S0.LT.2.5
      THEN
        SIG = 1000.
      ELSE
C
N       SOLVE EQUATIONS FOR PARABOLA FIT
        F1 = 1. / S4
        XX12 = S3*F1
        XX13 = S2*F1
        YY1  = S5*F1
        XX22 = S2 - S3*XX12
        XX23 = S1 - S3*XX13
        YY2  = S6 - S3*YY1
        XX32 = S1 - S2*XX12
        XX33 = S0 - S2*XX13
        YY3  = S7 - S2*YY1
        IF XX22.GT.XX32
        THEN
          XX23 = XX23 / XX22
          YY2  = YY2  / XX22
          PAR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
          PAR2 = YY2 - XX23*PAR3
        ELSE
          XX33 = XX33 / XX32
          YY3  = YY3  / XX32
          PAR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
          PAR2 = YY3 - XX33*PAR3
        CIF
        PAR1 = YY1 - XX12*PAR2 - XX13*PAR3
        DEG = S0 - WGHT0 - 2.
C
C
N       CALC. CHISQ + SOLVE L/R AMBIGUITY
        CHISQ = 0.
        DCHIM1 = 0.
        IHITM1 = 0
        XST    = 999999.
        XEN    =-999999.
        IPCO = HPCO0
        REPEAT
         IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
         THEN
            X = WRK(IPCO+3)
            IF(X.LT.XST) XST = X
            IF(X.GT.XEN) XEN = X
            Y = WRK(IPCO+4)
            F = (PAR1 *X + PAR2 )*X + PAR3
            DCHI = Y - F
            WRK(IPCO+13) = DCHI
N           SUM FOR RMS
            CHISQ = CHISQ + DCHI**2
N           KEEP BIGGEST RMS
C           IF ABS(DCHI).GE.DCHIM1
C           THEN
C             DCHIM1 = ABS(DCHI)
C             IHITM1 = IPCO
C           CIF
C     PRINT 2006, IPCO,X,Y,F,DCHI,CHISQ
          CIF
        IPCO = IPCO + HLDCO
        UNTIL IPCO.GT.HPCO9
        SIG    =      CHISQ  / DEG
C     PRINT 2008, ITRK,XST,SIG,DEG,PAR1,PAR2,PAR3,WGHT0,Y0
C
N       SET LIMIT FOR SIGMA
        SIGLM = TRELLM(16)**2
      CIF
C
      CPROC
C
N     *************************
N     *      S E L C L L      *
N     *************************
C
C
N     SELECT CELLS CONTAINING TRACK
      PROC SELCLL
C
        FOR I=1,6
          ITRCLL(I) = 0
        CFOR
        IPC0 = IPTR + 34
        IPC9 = IPC0 +  5
        ICELL = 0
        FOR IPC = IPC0,IPC9
          JCELL = IDATA(IPC)
          IF JCELL.GT. 0 .AND. JCELL.LE.96
          THEN
            JRING = 1
            IF(JCELL.GT.24) JRING = 2
            IF(JCELL.GT.48) JRING = 3
            JPC = JRING*2 - 1
            IF ITRCLL(JPC).EQ.0
            THEN
              ITRCLL(JPC) = JCELL
            ELSE
              IF(ITRCLL(JPC).NE.JCELL) ITRCLL(JPC+1) = JCELL
            CIF
            ICELL = JCELL
            IRING = JRING
          CIF
        CFOR
C
C     PRINT 2016, ITRCLL
      CPROC
C
N     *************************
N     *      F E T C H        *
N     *************************
C
C
N     FETCH HITS IN CELL
      PROC FETCH
C
N       DIR. OF SENSEW. + DRIFTSP.
        IF JRING.NE.3
        THEN
          IC1 = JCELL
          IF(IC1.GT.24) IC1 = IC1 - 24
          CSROT0 = DIRWR1(IC1,1)
          SNROT0 = DIRWR1(IC1,2)
        ELSE
          IC1 = JCELL - 48
          CSROT0 = DIRWR3(IC1,1)
          SNROT0 = DIRWR3(IC1,2)
        CIF
        DRICS  = TRMATC(JCELL,2)
        DRISN  = TRMATS(JCELL,2)
        DRITG  = DRISN/DRICS
        DRISNF = DRISN * .05
C
N       LOAD RADIUS AND WIRE SPACING
        R0 = FSENSW(JRING)
        DR = RINCR (JRING)
C
N       ANGLE OF TRACK IN RING
        R1   = DR*7.5 + R0
        DX   = R1 * CSROT0 - XCIRC
        DY   = R1 * SNROT0 - YCIRC
        RR   = SQRT(DX**2 + DY**2) * CHARGE
        CSB  = DX / RR
        SNB  = DY / RR
        TGB  = CSB/SNB
C
N       SET DRIFT SPACE BIN
        DSBIN1 = DRIVEL(JCELL,1)
        IF(NRUN.GT.100) DS0 = T0FIX(JRING)*DSBIN1*64.
        IF(NRUN.LE.100) DS0 = DSBIN1*.5
N       ANGLE(TRACK,DRIFT DIRECT.)
        TANBET = ABS((TGB-DRITG)/(TGB*DRITG+1.))
C     PRINT 2007, JCELL,CSROT0,SNROT0,DRICS,DRISN,CSB,SNB,CHARGE,TANBET,
C    ,            DSBIN1,DS0
N       CORRECTION CONSTANTS FOR JCELL
C
        IPJCOR = ICALIB(5) + JCELL
        CCST01 = ACALIB(IPJCOR    ) * TANBET
        CCST02 = ACALIB(IPJCOR+ 96) * TANBET
        CCST11 = ACALIB(IPJCOR+192)
        CCST12 = ACALIB(IPJCOR+288)
        CCST21 = ACALIB(IPJCOR+384)
        CCST22 = ACALIB(IPJCOR+480)
        CCST51 = ACALIB(IPJCOR+576) * 10.
        CCST52 = ACALIB(IPJCOR+672) / 121.15
        CCST61 = ACALIB(IPJCOR+768) * 10.
        CCST62 = ACALIB(IPJCOR+864) / 121.15
C     PRINT 2002, JRING,JCELL,IP,IPJCOR,CCST01,CCST02,CCST11,CCST12,
C    ,            CCST21,CCST22,CCST51,CCST52,CCST61,CCST62
N       COUNTER FOR NUMBER OF HITS FOUND
        JHIT = 0
        NHIT   = 0
        NHGOOD = 0
N       PRESET LAST LAYER
        ILAYL =-99
N       LOOP OVER ALL HITS OF CELL
        IPCO = IPCO - HLDCO
        IPJETC = IDATA(IQJETC)*2
        IP0    = IPJETC + 100
        IPCLL  = IPJETC + 2 + JCELL
        IP     = HDATA(IPCLL  ) + IP0
        IP9    = HDATA(IPCLL+1) + IP0
        IPHL   = IPJHTL + 2 + HDATA(IPCLL)/4
C     PRINT 2002, JRING,JCELL,IP,IP9,TGB,SNB,CSB,DRISN,DRICS
        WHILE IP.LT.IP9
C
N         CHECK TRACK # OF HIT LABEL
          LB   = IDATA(IPHL)
          ITR1 = LAND(SHFTR(LB,17),127)
          ITR2 = LAND(SHFTR(LB, 1),127)
C     PRINT 2009, IPHL,LB,ITR1,ITR2,ITRK
          IF ITR1.EQ.ITRK .OR. ITR2.EQ.ITRK
          THEN
C
N           L/R FROM HIT LABEL
            LBLR = 0
            IF(ITR1.EQ.ITRK) LBLR = LAND(LB,MKLRT1)
            IF(ITR2.EQ.ITRK) LBLR = LAND(LB,MKLRT2)
            LBSIDE =-1
            IF(LBLR.NE.0) LBSIDE = 1
            LBLR = LBSIDE
C
            IWIR = HDATA(IP)
            IWIR = SHFTR(IWIR,3)
N           LAYER NUMBER WITHIN RING 3
            ILAY = LAND(IWIR,15)
N           AMPLITUDES
            IAMPL = HDATA(IP+1)
            IAMPR = HDATA(IP+2)
N           DRIFT SPACE
            DS =(HDATA(IP+3)) * DSBIN1
C     DATA NPRHT /0/
C     NPRHT = NPRHT + 1
C     IF(NPRHT.LE.25) PRINT 2019, IWIR,ILAY,JCELL,HDATA(IP+3),DS,DSBIN1
C2019 FORMAT(' HIT ',4I6,F6.1)
            X1   = ILAY * DR + R0
            Z1   = X1*TGTH + ZVERT
N           CORRECTION FOR TOF + PROPAG. ALONG WIRE
            DDS = (1222.9-ABS(Z1))*ABERR(1) + ABERR(6)*R1*CSTHI
            DSC = DS - DDS + DS0
            Y1   = SWDEPL
            IF(LAND(ILAY,1).NE.0) Y1 =-Y1
            Y1   = (7-ILAY)*(CCST52*Z1+CCST51) - CCST62*Z1-CCST61 + Y1
            X    = X1*CSROT0 - Y1*SNROT0
            Y    = X1*SNROT0 + Y1*CSROT0
            IF DS.LE.DRC
            THEN
              IF DS.LT.4.0
              THEN
                IF DS.GT.DSD1
                THEN
                  DSC = (DSD1-DSD0)*DRV0 + (DS-DSD1)*DRV1
                ELSE
                  DSC = (DS-DSD0)*DRV0
                CIF
                IF(DSC.LT.0.1) DSC = 0.1
              CIF
              DXR  = DSC * CSB
              DYR  = DSC * SNB
              DXL =-DXR
              DYL =-DYR
            ELSE
C
N             EDGE WIRE FIELD DISTORTION
              IF ILAY.LT. 3
              THEN
                DILAY =-(ILAY- 3)**2
                DSCL  = (DILAY*CCST11 + 1.) * DSC
                DSCR  = (DILAY*CCST12 + 1.) * DSC
              ELSE
              IF ILAY.GT.12
              THEN
                DILAY =-(ILAY-12)**2
                DSCL  = (DILAY*CCST21 + 1.) * DSC
                DSCR  = (DILAY*CCST22 + 1.) * DSC
              ELSE
                DSCL = DSC
                DSCR = DSC
              CIF
              CIF
C
N             FIELD DISTORTIONS AT LARGE DRIFT TIMES
              IF DSC.GT.ABERR(7)
              THEN
                DWIR  = ILAY - 7.5
                DWIRC = DSC*DRISNF
                DWIRL = DWIR + DWIRC
                DWIRR = DWIR - DWIRC
                DSCL  = (DSCL-ABERR(7))*DWIRL*CCST01 + DSCL
                DSCR  =-(DSCR-ABERR(7))*DWIRR*CCST02 + DSCR
              CIF
              DXR  = (DSCR-DRC)*DRISN + DRC*CSB
              DYR  = (DSCR-DRC)*DRICS + DRC*SNB
              DXL  =-(DSCL-DRC)*DRISN - DRC*CSB
              DYL  =-(DSCL-DRC)*DRICS - DRC*SNB
            CIF
C     PRINT 2010, ILAY,DS,DSC,DSCL,DSCR,XL,XR,X,Y,DXL,DXR,DYL,DYR
            XL   = DXL + X - XT
            YL   = DYL + Y - YT
            XXL  = XL*CSROT + YL*SNROT
            YYL  =-XL*SNROT + YL*CSROT
            XR   = DXR + X - XT
            YR   = DYR + Y - YT
            XXR  = XR*CSROT + YR*SNROT
            YYR  =-XR*SNROT + YR*CSROT
C
N           CALCULATE Z COORDINATE
            IF IAMPR.LE.0.OR.IAMPL.LE.0
            THEN
              ZZ     = 0.
              LZGOOD = 16
            ELSE
              ZZ = IAMPR + IAMPL
              ZZ = FLOAT(IAMPR-IAMPL) * ZAL*.5 / ZZ
              LZGOOD = 0
              IF(ABS(ZZ).GT.1250.) LZGOOD = 16
            CIF
N           SET ARRAY
C     PRINT 2010, ILAY,DS,XXL,YYL,X1,Z1,XXR,YYR,Y1
C
N           CHECK IF LEFT + RIGHT SOLUTION POSSIBLE
            NLRSOL = 1
            IF(DS.LT.2.0) NLRSOL = 2
C
N           LOOP OVER LEFT +/OR RIGHT SOLUTION
            ILRSOL = 0
            REPEAT
            ILRSOL = ILRSOL + 1
C
N             SELECT SIDE
              IF NLRSOL.EQ.1 .AND. LBSIDE.LT.0  .OR.
     ?           NLRSOL.EQ.2 .AND. ILRSOL.EQ.1
              THEN
N               LEFT SIDE
                LBSIDE =-1
                XX  = XXL
                YY  = YYL
              ELSE
N               RIGHT SIDE
                LBSIDE = 1
                XX  = XXR
                YY  = YYR
              CIF
C
N             HIT QUALITY:
              LBGOOD = 0
              IF(LBSIDE.NE.LBLR) LBGOOD = 1
N             NEW LAYER?
              IF ILAY.NE.ILAYL .OR. LBGDL.LE.1.AND.LBGOOD.LE.1
              THEN
                LBREG = 1
N               INCREASE HIT COUNTER
                JHIT = JHIT + 1
                IPCO = IPCO + HLDCO
              ELSE
N               2 HITS IN SAME LAYER, SELECT CLOSEST
                LBREG = 0
                IF(LBGOOD.LT.IWRK(IPCO+10)) LBREG = 1
              CIF
N             REGISTER NEW HIT?
              IF LBREG.NE.0
              THEN
                NHIT   = NHIT   + 1
                IF(LBGOOD.LE.1) NHGOOD = NHGOOD + 1
                IWRK(IPCO   ) = ILAY
                IWRK(IPCO+ 1) = IP
                IWRK(IPCO+ 2) = LBSIDE
                WRK (IPCO+ 3) = XX
                WRK (IPCO+ 4) = YY
                WRK (IPCO+ 5) = ZZ
                WRK (IPCO+ 6) = XX - X0
                IWRK(IPCO+ 7) = LZGOOD
                WRK (IPCO+ 8) = DS
                IWRK(IPCO+ 9) = JCELL
                IWRK(IPCO+10) = LBGOOD
                WRK (IPCO+11) = TGB
                IWRK(IPCO+12) = JRING
                WRK (IPCO+13) = 0.
                ILAYL = ILAY
                LBGDL = LBGOOD
                NHTRNG(JRING) = NHTRNG(JRING) + 1
              CIF
C
            UNTIL ILRSOL.GE.NLRSOL
C
          CIF
C
        IPHL = IPHL + 1
        IP   = IP   + 4
        CWHILE
N       SET IPCO TO 1. FREE LOCATION
        IPCO = IPCO + HLDCO
C
N     MASK FOR TRACKS AT CELL WALL + IN DEAD CELLS
C
N       SET LABEL FOR DEAD CELL
        IF NHIT.LE.2
        THEN
          IF DEADCL(JCELL,NRUN)
          THEN
            LBCELL = LOR(LBCELL,MKDDCL(JRING))
            JHIT = 16
            NHIT = 16
C     PRINT 2019, JCELL,JRING,NRUN,LBCELL
          CIF
        CIF
C
      CPROC
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
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
      YEN  = (PAR1 *XEN + PAR2 )*XEN + PAR3
N     DIRECTION AT START + END POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
      TGEN = PAR1*XEN*2 + PAR2
      DXEN = 1./SQRT(TGEN**2+1.)
      DYEN = DXEN * TGEN
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
      CURV =-PAR1 * 2.
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      IWRK(IP+ 1) = ITRK
      IWRK(IP+ 2) = 32
      IWRK(IP+ 3) = IDAY
      IWRK(IP+ 4) = 16
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = XST - X0
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)
      WRK (IP+10) = SNTH
      IWRK(IP+11) = 0
      WRK (IP+12) = XEN *CSROT - YEN *SNROT + XT
      WRK (IP+13) = XEN *SNROT + YEN *CSROT + YT
      WRK (IP+14) = XEN - X0
      WRK (IP+15) = (DXEN*CSROT - DYEN*SNROT)
      WRK (IP+16) = (DXEN*SNROT + DYEN*CSROT)
      WRK (IP+17) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
C     IF(SIG  .LT.0) PRINT 2021,WRK(IP+1),S0,SIG
C2021 FORMAT(' -VE SQRT:',I4,5E13.5)
      WRK (IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CURV
C     IF(SIG11.LT.0) PRINT 2021,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SQRT(SIG*SIG11) * 2.
      WRK (IP+27) = CURV
      WRK (IP+28) = CURV
      WRK (IP+31) = XOR - X0
C     I0 = IP+ 1
C     I9 = IP+48
C     PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
N     *************************
N     *      F I T B K 1      *
N     *************************
C
C
N     CHANGE FIT BANK (1.POINT)
      PROC FITBK1
C
N     START POINT
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
N     DIRECTION AT START POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
      CURV =-PAR1 * 2.
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV,
C    ,            XMIN,YMIN
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = XST - X0
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)
      WRK (IP+10) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
C     IF(SIG  .LT.0) PRINT 2022,WRK(IP+1),S0,SIG
C2022 FORMAT(' -VE SQRT(1):',I4,5E13.5)
      WRK (IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CURV
C     IF(SIG11.LT.0) PRINT 2022,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SQRT(SIG*SIG11) * 2.
      WRK (IP+27) = CURV
C     I0 = IP+ 1
C     I9 = IP+48
C     PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
C
N     *************************
N     *      L A B E L        *
N     *************************
C
C
N     LABEL USED HITS
      PROC LABEL
C
N       PRESET LAST HIT POINTER
        IWL = -999
C
N       PRESET LAST HIT POINTER
        IWL = -999
        FOR IP = HPCO0,HPCO9,HLDCO
          IW0 = IWRK(IP)
          X   = WRK(IP+3)
          Y   = WRK(IP+4)
          F   = (PAR1*X + PAR2)*X + PAR3
          DF  = F - Y
N         SELECT CLOSEST HIT
          LBGOOD = 4
          IF(ABS(DF).LT.ALBLM2) LBGOOD = 1
          IF(ABS(DF).LT.ALBLM1) LBGOOD = 0
          IWRK(IP+ 10) = LBGOOD
          WRK (IP+13) = DF
C
N         CHECK IF 2 HITS FROM SAME WIRE
          IF IWL.EQ.IW0
          THEN
N           SELECT CLOSEST HIT
            IF ABS(DFL).LT.ABS(DF)
            THEN
              IWRK(IP +10) = 16
            ELSE
              IWRK(IPL+10) = 16
            CIF
          CIF
N         STORE LAST POINTERS + DF
          IWL = IW0
          IPL = IP
          DFL = DF
        CFOR
C
      CPROC
C
C
N     *************************
N     *      I N I T          *
N     *************************
C
C
N     INITIALIZE CONSTANTS
      PROC INIT
C
        IQJETC = IBLN('JETC')
        IQHEAD = IBLN('HEAD')
C
        CALL DAY2(DATE)
        IDAY = DATE(1)*1000 + DATE(2)
C
N       MULT. SCATTERING CONSTANTS
        RESMS = .020**2/2. * .16 * (1. + ALOG10(.16) / 9) * 155.45**2
C
N       RADIUS AROUND WIRE FOR CORR. OF DRIFTSPACE
        DRC = RINCR(1)*.5 * DRICOS
C       CONST. FOR VAR. OF DRIFT VEL.
        IPHEAD = IDATA(IQHEAD)*2
        NRUN = HDATA(IPHEAD+10)
        IF NRUN.LE.100
        THEN
          DSD0   = .0
          DSD1   = 5.0
          DSD2   = 5.0
          DRV0   = 1.0
          DRV1   = 1.0
        ELSE
          DSD0   =-.63
          DSD1   = 1.8
          DSD2   = 4.0
          DRV0   = 0.8
          DRV1   = (DSD2 - (DSD1-DSD0)*DRV0) / (DSD2-DSD1)
        CIF
      CPROC
C
      END
C   08/12/80 309201526  MEMBER NAME  ORFZFITV (FITSR)       SHELTRAN
      SUBROUTINE FZFITV(IPPATR,IPJHTL,NTRVTX,Z0,ERRFAC,MODEF)
C---
C---     GENERAL VERTEX FIT ROUTINE USING MARKED TRACKS ONLY
C---
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
#include "cworkpr.for"
#include "cworkeq.for"
C
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(HPZR0 ,HPWRK(20)),(HPZR9 ,HPWRK(21)),(HLDZR ,HPWRK(22))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
C
C2000 FORMAT('0CALL FZFITO(',2I6,1X,I3,8.2,F8.4,I4' )')
C2001 FORMAT(1H0,2I3,I8,2(I4,3F6.1,3F6.3),
C    ,     /,14X,I3,4E13.5,F6.2,I3,4E13.5,
C    ,     /,14X,I3,2F8.3,F6.1,I3,10X,6I3,8I6,2X,Z4)
C2003 FORMAT(' FRFITO RESULT:',F6.0,F6.3,F6.0,F6.3)
C
      NTRVTX = 0
      MODE   = MODEF
C
C
      IPTR1 = IDATA(IPPATR+1) + IPPATR
      LDTR  = IDATA(IPPATR+3)
      NTR   = IDATA(IPPATR+2)
C     PRINT 2000,IPPATR,IPJHTL,NTRVTX,Z0,ERRFAC,MODE
C     CALL PRPATR
      IF NTR.GT.0
      THEN
C
N       INITIALIZE ZRFITO
        IF(LAND(MODE,2).NE.0) CALL ZRFITO(-1,2001)
C
        FOR ITR=1,NTR
C
N         CHECK IF MARKED TRACK
          ITYPE =-IDATA(IPTR1+4)
          IF ITYPE .GT. 0
          THEN
C
N           REFIT TRACK WITH VERTEX CONSTRAINT
            CALL FRFITV(IPTR1,IPJHTL,ERRFAC)
C           I0 = HPTR0
C           I9 = I0 + 47
C           PRINT 2001, (WRK(I1),I1=I0,I9)
C
N           REPLACE BANK IF GOOD RESULT
            IP    = HPTR0 - 1
            SIG   =  WRK(IP+23)
            IF LAND(IWRK(IP+ 2),32).EQ.0
            THEN
              SIG   = 1000.
              SIG0  =    0.
            ELSE
              SIG   =  WRK(IP+23)
              SIG0  = ADATA(IPTR1+23)
              ANHT  = IWRK(IP+24)
              IF(LAND(IWRK(IP+ 4),16).NE.0) ANHT = ANHT * 1.5
              ANHT0 = IDATA(IPTR1+24)
C             PRINT 2003, ANHT,SIG,ANHT0,SIG0
              IF(ANHT/ANHT0.LT..8) SIG = SIG + .5
            CIF
            IF SIG0.GT.SIG .OR. SIG.LT..35
            THEN
C
              NTRVTX = NTRVTX + 1
              CALL MVC(IDATA(IPTR1+1),0,IWRK(HPTR0),0,112)
              IDATA(IPTR1+4) = LOR(IDATA(IPTR1+4),ITYPE)
N             SET ZRFIT INDEX
              INDZR = 0
              IF LAND(MODE,2) .NE. 0
              THEN
                 ADATA(IPTR1+17) = WRK(HPTR0+30)
N                ZFIT: STORE R-Z COORDINATES FOR LATER USE
                 CALL ZRFITO(INDZR,IPTR1)
              CIF
              IF LAND(MODE,2) .EQ. 0 .OR. INDZR.NE.0
              THEN
N                NO ZFIT: CALCULATE POINTS + DIRECTIONS
                 TGTH = ADATA(IPTR1+30)
                 Z0TR = ADATA(IPTR1+31)
                 CSTH = 1./SQRT(TGTH**2+1.)
                 SNTH = CSTH * TGTH
                 ADATA(IPTR1+ 8) = ADATA(IPTR1+ 8) * CSTH
                 ADATA(IPTR1+ 9) = ADATA(IPTR1+ 9) * CSTH
                 ADATA(IPTR1+15) = ADATA(IPTR1+15) * CSTH
                 ADATA(IPTR1+16) = ADATA(IPTR1+16) * CSTH
                 ADATA(IPTR1+17) = SNTH
                 ADATA(IPTR1+ 7) = ADATA(IPTR1+ 7) * TGTH + Z0TR
                 ADATA(IPTR1+14) = ADATA(IPTR1+14) * TGTH + Z0TR
              CIF
C
N           NO TRACK FROM ORIGIN, RESET TYPE(1. POINT)
            ELSE
              IDATA(IPTR1+4) = 0
C
            CIF
          CIF
        IPTR1 = IPTR1 + LDTR
        CFOR
C
N       CHECK IF TRACKS ACCEPTED
        IF NTRVTX.GT.0
        THEN
C
N         PERFORM SUPERFIT
          INDZR = 1
          IF(LAND(MODE,2).NE.0) CALL ZRFITO(INDZR,Z0)
C
        CIF
C
      CIF
      RETURN
C
      END
C   09/06/83 802181227  MEMBER NAME  JFETCH   (JADEGS)      SHELTRAN
      SUBROUTINE JFETCH(IPTR,IPJHTL,WRK,LHIT,IPRES,INDEX,/XO/,/YO/)
C
C        P. STEFFEN                         83/03/28
C
C        MODIFIED TO CALL NEW JFETCH WHEN APPLICABLE   J.SPITZER
C                                                       86/04/30
C    Z-CHAMBER COORDINATES ARE FETCHED IN CASE OF
C    ZS-FIT (INDEX=4)
C                                            15/7/87  J.S.
C        FETCH HITS FOR TRACK 'IPTR' IN PATR-BANK
C        CALCULATE COORDINATE INCLUDING ALL CORRECTIONS
C        USE SPECIAL LAYER DEPENDENT POS. + VD   ***************
C        STORE COORDINATES IN WRK(I1),I1=1,LHIT*NHIT
C
C        INDEX = 1 : COORDINATES IN REAL SPACE
C        INDEX = 2 : X-AXIS THROUGH 1. + LAST POINT
C        INDEX = 3 : X-AXIS THROUGH (XO,YO) + LAST POINT
C
C        INDEX = 4 : NEW FOR S-Z FITS    J. SPITZER 22/4/87
C                    COORDINATES IN REAL SPACE
C
C        LAST MOD: J. HAGEMANN 13/01/88  NEW FLAG VALUES FOR HITS OF
C                                        OVERLAPPING TRACKS (LBGOOD)
C                                        NOVL COUNTS THESE HITS
C                                        NOVL PASSED VIA COMMON/XYFVT1/
C
C  LBGOOD:  2=     OLD
C           2=  DIST. OF HIT-SEL. TRACK < DIST. OF HIT-SECOND TRACK
C          11=  DIST. OF HIT-SEL. TRACK = DIST. OF HIT-SECOND TRACK
C          12=  DIST. OF HIT-SEL. TRACK > DIST. OF HIT-SECOND TRACK
C
C
      IMPLICIT INTEGER*2 (H)
C
      DIMENSION WRK(200)
      EQUIVALENCE (ZWZ,IZW)
C---------------------------------
      COMMON/JSCALD/ JESCAL,JESKEY
C---------------------------------
#include "cdata.for"
C
#include "calibr.for"
C
C
#include "cjdrch.for"
#include "cdsmax.for"
C
      COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5), ZCHWW
C ONLY ZCHWW WHICH IS THE WEIGHT FOR Z-CHAMBER HITS IS USED HERE
C  ARRAYS FOR Z-CHAMBER INFORMATION
      DIMENSION IZCHMB(3,2),AZCHMB(3,2)
C
      COMMON/XYFVT1/MODXYV,NOVL
C
      DIMENSION IRESAR(13),RESAR(13),HRESAR(26)
      EQUIVALENCE (IRESAR(1),RESAR(1),HRESAR(1))
C
N     CONSTANTS FOR ANGULAR CORRECTION
      DATA NCOAR / 15/, DTGB / .15/
      REAL TGCOAR(15) /-99.,-.45, 12*0., 99./
      REAL T0COAR(60) / .000, .000, .000, .000, .000,
     ,     .000, .000,-.020,-.060,-.130,-.030, .100, .200, .200, .200,
     ,                  .000, .000, .010, .110, .100,
     ,     .075, .050, .025, .005, .015, .065, .060, .060, .060, .060,
     ,                  .190, .190, .180, .165, .140,
     ,     .120, .100, .075, .050, .010,-.050,-.075,-.035, .000, .000,
     ,                  .110, .110, .115, .140, .135,
     ,     .085, .045, .030, .040, .050, .055, .055, .055, .055, .055/
      REAL SLCOAR(60) / 60*0./
C
N     MASK FOR L/R BIT IN HIT LABEL
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
C
C-----------------------------------------------------------------------
C      --- NEW CALIBRATION WHEN AVAILABLE AND IF REQUESTED   J.S. ---
      IF JESCAL .GT. 0   .AND.   JESKEY .EQ. 54321
      THEN
         IP8=ICALIB(13)+6
         IP9=ICALIB(13)+2598
         CALL JFTNEW(IPTR,IPJHTL,WRK,LHIT,IPRES,INDEX,XO,YO,
     +   ACALIB(IP8),ACALIB(IP9))
         RETURN
      CIF
C
      INDX=INDEX
      IF(INDX.EQ.4) INDX=1
C-----------------------------------------------------------------------
C
C     IF(IDATA(IPTR+1).LT. 4) RETURN
C     I0 = IPTR + 1
C     I9 = IPTR + 48
C     PRINT 2001, (IDATA(I1),I1=I0,I9)
C     I0 = IPJHTL*2 + 1
C     I9 = I0 + IDATA(IPJHTL)*2 - 1
C     IPJETC = IDATA(IBLN('JETC'))
C     I0 = IPJETC*2 + 1
C     I9 = I0 + 109
C2001 FORMAT(1H0,2I3,I8,2(I4,3F6.1,3F6.3),
C    ,     /,14X,I3,4E13.5,F6.2,I3,4E13.5,
C    ,     /,14X,I3,2F8.3,F6.1,I3,10X,6I3,8I6,2X,Z4)
C2002 FORMAT('0FETCH:',2I3,2I5,12F9.5)
C2003 FORMAT('0ROTATION:',12F10.5)
C2004 FORMAT('0CIRC.CENTRE:',2I3, F10.5,2F10.0,F8.1,2F8.1)
C2005 FORMAT('0TRACK:',I6,/,(1X,3I6,4F9.3,I4,F9.3,2I4,F8.3,I6,3F8.2))
C2007 FORMAT(' FETCH:',I3,8F8.5,F10.7,F6.3)
C2009 FORMAT(' JHTL:',I8,1X,Z8,3I5,I8)
C2010 FORMAT(' HIT:',I6,12F8.2)
C2011 FORMAT('0ABERR:',10F10.6)
C2016 FORMAT('0ITRCLL =',6I8,/,(9X,6F8.3))
C
C2901 FORMAT('0JFETCH(PST) CALLED WITH WRONG INDEX:',I6)
C
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
        LBINIT = 1
        NCALL = 0
        PERFORM INIT
        PRINT 7777
 7777   FORMAT(' JFETCH : VERSION FROM 13/01/88 CALLED!',/,
     &         ' +++++++++++++++++++++++++++++++++++++++++++++++++++++')
      CIF
      NCALL = NCALL + 1
C
CCC   RESERVE SPACE IN CWORK
CCC   HPCO0  = 1
CCCCC LHIT   = MAX0(LHIT,14)
      LHBIT  = LHIT*4
CCC   HPFREE = LHIT*100 + HPCO0
CCC   HPCO9  = 0
      IPCO = 1
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      NEVT = HDATA(IPHEAD+11)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
N     CENTRE OF CIRCLE (USED FOR ANGULAR CORRECTION)
      IF IDATA(IPTR+18).EQ.1
      THEN
N       CIRCLE PARAMETERS
        ALFA  = ADATA(IPTR+21)
        CRV   = ADATA(IPTR+19)
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        RAD   =  1./ABS(CRV) + ADATA(IPTR+20)
        XCIRC = COS(ALFA) * RAD
        YCIRC = SIN(ALFA) * RAD
        CHARGE = SIGN(1.,ADATA(IPTR+25))
      ELSE
N       PARABOLA PARAMETERS
        CRV   = ADATA(IPTR+22)*2.
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        ALFA  = ADATA(IPTR+19)
        XCIRC =-SIN(ALFA)/CRV + ADATA(IPTR+20)
        YCIRC = COS(ALFA)/CRV + ADATA(IPTR+21)
        CHARGE =-SIGN(1.,ADATA(IPTR+22))
      CIF
C
N     ZVERT, THETA + DIR. COSINES
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTHI = SQRT(TGTH**2 + 1.)
      CSTH  = 1. / CSTHI
      SNTH  = CSTH * TGTH
C     PRINT 2004,ITRK,IDATA(IPTR+18),ALFA,XCIRC,YCIRC,ZVERT,TGTH,CSTHI
C
C     PRINT 2011,ABERR
C
N     ROTATION ANGLE (USING LAST POINT OF TRACK)
      SELECT INDX
      CASE 1
        XT = 0.
        YT = 0.
        CSROT = 1.
        SNROT = 0.
        XOT   = 0.
        YOT   = 0.
      CASE 2
        XT    = (ADATA(IPTR+12) + ADATA(IPTR+5)) * .5
        YT    = (ADATA(IPTR+13) + ADATA(IPTR+6)) * .5
        XX    =  ADATA(IPTR+12) - ADATA(IPTR+5)
        YY    =  ADATA(IPTR+13) - ADATA(IPTR+6)
        RR    = SQRT(XX**2+YY**2)
        IF RR.LT.10.
        THEN
           IPRES=IPCO
           RETURN
        CIF
        CSROT = XX / RR
        SNROT = YY / RR
        XX    = XO - XT
        YY    = YO - YT
        XOT   = 0.
        YOT   = 0.
      CASE 3
        XT    = (ADATA(IPTR+12) + XO) * .5
        YT    = (ADATA(IPTR+13) + YO) * .5
        XX    =  ADATA(IPTR+12) - XO
        YY    =  ADATA(IPTR+13) - YO
        RR    = SQRT(XX**2+YY**2)
        CSROT = XX / RR
        SNROT = YY / RR
        XX    = XO - XT
        YY    = YO - YT
        XOT   = XX*CSROT + YY*SNROT
        YOT   =-XX*SNROT + YY*CSROT
      OTHER
C
N       ILLEGAL INDEX
C       PRINT 2901,INDEX
        RETURN
C
      CSELECT
C
      NOVL = 0
C
N     SELECT CELLS CONTAINING TRACK
C
      IPC0 = IPTR + 34
      IPC9 = IPC0 +  5
      FOR IPC = IPC0,IPC9
         JCELL = IDATA(IPC)
         IF JCELL.GT. 0 .AND. JCELL.LE.96
         THEN
            JRING = 1
            IF(JCELL.GT.24) JRING = 2
            IF(JCELL.GT.48) JRING = 3
            PERFORM FETCH
         CIF
      CFOR
C
CCC   HPCO9 = IPCO - 1
CCC   PRINT 2005, LBCELL,(WRK(I),I=1,HPCO9)
C
C
C FETCH Z-CHAMBER HITS IN CASE Z-S FITS (INDEX=4)
      IF INDEX.EQ.4 .AND. ZCHWW.GT..1 .AND. ZCHWW.LT.2000.
      THEN
         CALL ZCFTNW(NRUN,NEVT,ITRK,TGTH,ZVERT,NZHIT,IZCHMB,AZCHMB)
         IF NZHIT.GT.0
         THEN
            FOR J=1,NZHIT
               HRESAR( 1) = 100+IZCHMB(1,J)
               HRESAR( 2) = IZCHMB(2,J)
               HRESAR( 3) = 0
               HRESAR( 4) = 0
               HRESAR( 5) = 1
C              HRESAR( 6) = IP-2*IPJETC
               HRESAR( 6) = 101
               XX=AZCHMB(1,J)
               YY=AZCHMB(2,J)
               ZZ=AZCHMB(3,J)
               RESAR ( 4) = XX
               RESAR ( 5) = YY
               RESAR ( 6) = ZZ
C CALCULATE TRACK LENGTH IN R-PHI FROM FIRST POINT ON TRACK
               UX=XX-ADATA(IPTR+5)
               UY=YY-ADATA(IPTR+6)
               UU=SQRT(UX**2+UY**2)
            IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
               IF(UX*ADATA(IPTR+8)+UY*ADATA(IPTR+9).LT.0.) UU=-UU
               RESAR ( 7) = UU
C              RESAR ( 8) = WW
               RESAR ( 8) = ZCHWW
               IF(NRUN.LT.24200) RESAR(8)=RESAR(8)*.6
               CALL MVC(WRK(IPCO),0,RESAR(1),0,LHBIT)
               IPCO = IPCO + LHIT
            CFOR
         CIF
      CIF
C
C
N     STORE RESULTS
      IPRES = IPCO
      IF INDEX.NE.4
      THEN
         WRK (IPRES   ) = XT
         WRK (IPRES+ 1) = YT
         WRK (IPRES+ 2) = CSROT
         WRK (IPRES+ 3) = SNROT
         WRK (IPRES+ 9) = XOT
         WRK (IPRES+10) = YOT
         WRK (IPRES+11) = CSTH
         WRK (IPRES+12) = SNTH
      CIF
C
C     PRINT 2003, CSROT,SNROT,XT,YT
C
C
      RETURN
C
C
N     *************************
N     *      F E T C H        *
N     *************************
C
C
N     FETCH HITS IN CELL
      PROC FETCH
C
N       DIR. OF SENSEW. + DRIFTSP.
        IF JRING.NE.3
        THEN
          IC1 = JCELL
          IF(IC1.GT.24) IC1 = IC1 - 24
          CSROT0 = DIRWR1(IC1,1)
          SNROT0 = DIRWR1(IC1,2)
        ELSE
          IC1 = JCELL - 48
          CSROT0 = DIRWR3(IC1,1)
          SNROT0 = DIRWR3(IC1,2)
        CIF
        DRICS  = TRMATC(JCELL,2)
        DRISN  = TRMATS(JCELL,2)
        DRITG  = DRISN/DRICS
        DRISNF = DRISN * .05
C
N       LOAD RADIUS AND WIRE SPACING
        R0 = FSENSW(JRING)
        DR = RINCR (JRING)
C
N       ANGLE OF TRACK IN RING
        R1   = DR*7.5 + R0
        DX   = R1 * CSROT0 - XCIRC
        DY   = R1 * SNROT0 - YCIRC
        RR   = SQRT(DX**2 + DY**2) * CHARGE
        CSB  = DX / RR
        SNB  = DY / RR
        TGB  = CSB/SNB
C
N       SET DRIFT SPACE BIN
        DSBIN1 = DRIVEL(JCELL,1)
C
N       ANGLE(TRACK,DRIFT DIRECT.)
        TANBET = (DRITG - TGB) / (TGB*DRITG + 1.)
C
N       DIFFERENT CORRECTION CONST. FOR MC + DATA
        IF NRUN.LE.100
        THEN
N         MC
          DS0 = DSBIN1*.5
          T0CORR = 0.
        ELSE
N         DATA
          DS0 = T0FIX(JRING)*DSBIN1*64.
          FOR I1=1,NCOAR
            IDX = I1
            IF(TANBET.LT.TGCOAR(IDX)) XFOR
          CFOR
          KRNG = JRING
          IF(KRNG.EQ.3 .AND. AND(JCELL,1).EQ.0) KRNG = 4
          IBIN = (KRNG-1)*NCOAR  + IDX
          T0CORR = (TANBET-TGCOAR(IDX)) * SLCOAR(IBIN) + T0COAR(IBIN)
        CIF
C     IF(NCALL.LE.2)
C    ,PRINT 2007, JCELL,CSROT0,SNROT0,DRICS,DRISN,CSB,SNB,CHARGE,TANBET,
C    ,            DSBIN1,DS0
C     IF(NCALL.LE.8)
C    ,PRINT 2093, JCELL,KRNG,IDX,IBIN,T0CORR,TGCOAR(IDX),T0COAR(IBIN),
C    ,            SLCOAR(IBIN),TANBET
C2093 FORMAT('0ANG.CORR.:',4I4,8F8.3)
N       CORRECTION CONSTANTS FOR JCELL
C
        IPJCOR = ICALIB(5) + JCELL
        CCST01 = ACALIB(IPJCOR     ) * ABS(TANBET)
        CCST02 = ACALIB(IPJCOR+  96) * ABS(TANBET)
        CCST11 = ACALIB(IPJCOR+ 192)
        CCST12 = ACALIB(IPJCOR+ 288)
        CCST21 = ACALIB(IPJCOR+ 384)
        CCST22 = ACALIB(IPJCOR+ 480)
        CCST51 = ACALIB(IPJCOR+ 576) * 10.
        CCST52 = ACALIB(IPJCOR+ 672) / 121.15
        CCST61 = ACALIB(IPJCOR+ 768) * 10.
        CCST62 = ACALIB(IPJCOR+ 864) / 121.15
        CCST81 = ACALIB(IPJCOR+1152)
C     IF(NCALL.LE.2)
C    ,PRINT 2002, JRING,JCELL,IP,IPCLLC,CCST01,CCST02,CCST11,CCST12,
C    ,  CCST21,CCST22,CCST51,CCST52,CCST61,CCST62,ACALIB(IPDY),CCST81
N       COUNTER FOR NUMBER OF HITS FOUND
        JHIT = 0
        NHIT   = 0
        NHGOOD = 0
N       PRESET LAST LAYER
        ILAYL =-99
N       LOOP OVER ALL HITS OF CELL
        IPCO = IPCO - LHIT
        IPJET4 = IDATA(IQJETC)
        IPJETC = IDATA(IQJETC)*2
        IP0    = IPJETC + 100
        IPCLL  = IPJETC + 2 + JCELL
        IP     = HDATA(IPCLL  ) + IP0
        IP9    = HDATA(IPCLL+1) + IP0
        IPHL   = IPJHTL + 2 + HDATA(IPCLL)/4
C     PRINT 2002, JRING,JCELL,IP,IP9,TGB,SNB,CSB,DRISN,DRICS
        WHILE IP.LT.IP9
C
N         CHECK TRACK # OF HIT LABEL
          LB   = IDATA(IPHL)
          ITR1 = LAND(SHFTR(LB,17),127)
          ITR2 = LAND(SHFTR(LB, 1),127)
C     PRINT 2009, IPHL,LB,ITR1,ITR2,ITRK,IP
          IF ITR1.EQ.ITRK .OR. ITR2.EQ.ITRK
          THEN
C
N           SET LBGOOD =  2,11,12 IF HIT ASSOCIATED WITH 2 TRACKS
            L0GOOD = 0
            IF ITR1.NE.0 .AND. ITR2.NE.0
            THEN
               NOVL   = NOVL + 1
               L0GOOD = 11
               ID1    = LAND(SHFTR(LB,27), 31)
               ID2    = LAND(SHFTR(LB,11), 31)
               IF ITR1 .EQ. ITRK
               THEN
                  IF( ID1 .LT. ID2 ) L0GOOD = 2
                  IF( ID1 .GT. ID2 ) L0GOOD = 12
               ELSE
                  IF( ID2 .LT. ID1 ) L0GOOD = 2
                  IF( ID2 .GT. ID1 ) L0GOOD = 12
               CIF
            CIF
C
N           L/R FROM HIT LABEL
            LBLR = 0
            IF(ITR1.EQ.ITRK) LBLR = LAND(LB,MKLRT1)
            IF(ITR2.EQ.ITRK) LBLR = LAND(LB,MKLRT2)
            LBSIDE =-1
            IF(LBLR.NE.0) LBSIDE = 1
            LBLR = LBSIDE
C
            IWIR = HDATA(IP)
            IWIR = SHFTR(IWIR,3)
N           LAYER NUMBER WITHIN RING 3
            ILAY = LAND(IWIR,15)
N           AMPLITUDES
            IAMPL = HDATA(IP+1)
            IAMPR = HDATA(IP+2)
N           DRIFT SPACE
            DS =(HDATA(IP+3)) * DSBIN1
            X1   = ILAY * DR + R0
            Z1   = X1*TGTH + ZVERT
N           CORRECTION FOR TOF + PROPAG. ALONG WIRE
            DDS = (1222.9-ABS(Z1))*ABERR(1) + ABERR(6)*R1*CSTHI
N           CORRECTION FOR GRAVITATION
            IF NRUN.LE.100
            THEN
N             MC
              DGR = 0.0
            ELSE
N             REAL
              DGR = ((Z1/1222.9)**2 - 1.) * .075
            CIF
            DSC =  DS - DDS + DS0
C     DATA NPRHT /0/
C     NPRHT = NPRHT + 1
C     IF(NPRHT.LE.50) PRINT 2019, IWIR,ILAY,JCELL,HDATA(IP+3),DS,DSBIN1,
C    ,                DSC,DDS,DS0,ACALIB(IPVD+ILAY)
C2019 FORMAT(' HIT ',4I6,F7.3,5E13.5)
            Y1   = SWDEPL
            IF(LAND(ILAY,1).NE.0) Y1 =-Y1
            Y1   = (7.5-ILAY)*(CCST52*Z1+CCST51) - CCST62*Z1-CCST61 + Y1
            X    = X1*CSROT0 - Y1*SNROT0
            Y    = X1*SNROT0 + Y1*CSROT0 - DGR
            IF DSC.LE.DRC
            THEN
              IF DSC.LT.DSD2
              THEN
                IF DSC.LT.DSD1
                THEN
                  DSC = DSC + DDS1 + (DSC-DSD1)*DRV1
                ELSE
                  DSC = DSC + DDS2 + (DSC-DSD2)*DRV2
                CIF
N               ANGULAR CORRECTION
C               DSC = DSC/DSD2 * T0CORR + DSC
                IF(DSC.LT.0.1) DSC = 0.1
              ELSE
C
N               ANGULAR CORRECTION
C               DSC = DSC + T0CORR
                DSC = (DSC-DSD2)/(DRC-DSD2) * T0CORR + DSC
              CIF
C             DSC = DSC + DSOFF
              DXR  = DSC * CSB
              DYR  = DSC * SNB
              DXL =-DXR
              DYL =-DYR
            ELSE
C
N             ANGULAR CORRECTION
              DSC = DSC + T0CORR
C             DSC = DSC + DSOFF
C
N             EDGE WIRE FIELD DISTORTION
              IF ILAY.LT. 3
              THEN
                DILAY =-(ILAY- 3)**2
                DSCL  = (DILAY*CCST11 + 1.) * DSC * (1. - CCST81)
                DSCR  = (DILAY*CCST12 + 1.) * DSC * (1. + CCST81)
              ELSE
              IF ILAY.GT.12
              THEN
                DILAY =-(ILAY-12)**2
                DSCL  = (DILAY*CCST21 + 1.) * DSC * (1. - CCST81)
                DSCR  = (DILAY*CCST22 + 1.) * DSC * (1. + CCST81)
              ELSE
                DSCL = DSC * (1. - CCST81)
                DSCR = DSC * (1. + CCST81)
              CIF
              CIF
C
N             FIELD DISTORTIONS AT LARGE DRIFT TIMES
              IF DSC.GT.ABERR(7)
              THEN
                DWIR  = ILAY - 7.5
                DWIRC = DSC*DRISNF
                DWIRL = DWIR + DWIRC
                DWIRR = DWIR - DWIRC
                DSCL  = (DSCL-ABERR(7))*DWIRL*CCST01 + DSCL
                DSCR  =-(DSCR-ABERR(7))*DWIRR*CCST02 + DSCR
              CIF
              DXR  = (DSCR-DRC)*DRISN + DRC*CSB
              DYR  = (DSCR-DRC)*DRICS + DRC*SNB
              DXL  =-(DSCL-DRC)*DRISN - DRC*CSB
              DYL  =-(DSCL-DRC)*DRICS - DRC*SNB
            CIF
C     PRINT 2010, ILAY,DS,DSC,DSCL,DSCR,XL,XR,X,Y,DXL,DXR,DYL,DYR
            XL   = DXL + X - XT
            YL   = DYL + Y - YT
            XXL  = XL*CSROT + YL*SNROT
            YYL  =-XL*SNROT + YL*CSROT
            XR   = DXR + X - XT
            YR   = DYR + Y - YT
            XXR  = XR*CSROT + YR*SNROT
            YYR  =-XR*SNROT + YR*CSROT
C
N           CALCULATE Z COORDINATE
C           IF IAMPR.LE.0.OR.IAMPL.LE.0
C           THEN
C             ZZ     = 0.
C             LZGOOD = 16
C           ELSE
C             ZZ = IAMPR + IAMPL
C             ZZ = FLOAT(IAMPR-IAMPL) * ZAL*.5 / ZZ
C             LZGOOD = 0
C             IF(ABS(ZZ).GT.1250.) LZGOOD = 16
C           CIF
            CALL AMPS2Z( IP,IPJET4,ZZ,WW,LZGOOD)
C
N           SET ARRAY
C     PRINT 2010, ILAY,DS,XXL,YYL,X1,Z1,XXR,YYR,Y1
C
N           CHECK IF LEFT + RIGHT SOLUTION POSSIBLE
            NLRSOL = 1
            IF(DSC.LT.2.0) NLRSOL = 2
C
N           LOOP OVER LEFT +/OR RIGHT SOLUTION
            ILRSOL = 0
            REPEAT
            ILRSOL = ILRSOL + 1
            LBGOOD = L0GOOD
C
N             SELECT SIDE
              IF NLRSOL.EQ.1 .AND. LBSIDE.LT.0  .OR.
     ?           NLRSOL.EQ.2 .AND. ILRSOL.EQ.1
              THEN
N               LEFT SIDE
                LBSIDE =-1
                XX  = XXL
                YY  = YYL
              ELSE
N               RIGHT SIDE
                LBSIDE = 1
                XX  = XXR
                YY  = YYR
              CIF
C
N             HIT QUALITY:
              IF(LBSIDE.NE.LBLR) LBGOOD = LBGOOD + 1
N             NEW LAYER?
              IF ILAY.NE.ILAYL .OR. LBGDL.LE.1.AND.LBGOOD.LE.2
              THEN
                LBREG = 1
N               INCREASE HIT COUNTER
                JHIT = JHIT + 1
                IPCO = IPCO + LHIT
              ELSE
N               2 HITS IN SAME LAYER, SELECT CLOSEST
                LBREG = 0
                ZWZ = WRK(IPCO+10)
                IF(LBGOOD.LT.IZW) LBREG = 1
              CIF
N             REGISTER NEW HIT?
              IF LBREG.NE.0
              THEN
                NHIT   = NHIT   + 1
                IF(LBGOOD.LE.2) NHGOOD = NHGOOD + 1
                IF INDEX.NE.4
                THEN
                   IRESAR( 1) = ILAY
                   IRESAR( 2) = IP
                   IRESAR( 3) = LBSIDE
                   RESAR ( 4) = XX
                   RESAR ( 5) = YY
                   RESAR ( 6) = ZZ
                   RESAR ( 7) = XX - XOT
                   IF(INDX.EQ.1) RESAR ( 7) = SQRT(XX**2 + YY**2)
                   IRESAR( 8) = LZGOOD
                   RESAR ( 9) = DSC
                   IRESAR(10) = JCELL
                   IRESAR(11) = LBGOOD
                   RESAR (12) = TANBET
                   IRESAR(13) = JRING
                   RESAR (14) = 0.
                ELSE
                   HRESAR( 1) = JCELL
                   HRESAR( 2) = ILAY
                   HRESAR( 3) = LZGOOD
                   HRESAR( 4) = LBGOOD
                   HRESAR( 5) = 1
                   HRESAR( 6) = IP-IPJETC
                   RESAR ( 4) = XX
                   RESAR ( 5) = YY
                   RESAR ( 6) = ZZ
C CALCULATE TRACK LENGTH IN R-PHI FROM FIRST POINT ON TRACK
                   UX=XX-ADATA(IPTR+5)
                   UY=YY-ADATA(IPTR+6)
                   UU=SQRT(UX**2+UY**2)
                   CURVXY=ADATA(IPTR+25)
                   IF(ABS(CURVXY).LT.1.E-8) CURVXY = SIGN(1.E-8,CURVXY)
            IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
                   IF(UX*ADATA(IPTR+8)+UY*ADATA(IPTR+9).LT.0.) UU=-UU
                   RESAR ( 7) = UU
                   RESAR ( 8) = WW
                CIF
C     PRINT 2005, LHIT,(RESAR(I1),I1=1,13)
                CALL MVC(WRK(IPCO),0,RESAR(1),0,LHBIT)
CCCCC           IF LHIT.GT.14
CCCCC           THEN
CCCCC             I0 = IPCO+14
CCCCC             I9 = IPCO+LHIT - 1
CCCCC             FOR I1=I0,I9
CCCCC               WRK(I1) = 0.
CCCCC             CFOR
CCCCC           CIF
                ILAYL = ILAY
                LBGDL = LBGOOD
              CIF
C
            UNTIL ILRSOL.GE.NLRSOL
C
          CIF
C
        IPHL = IPHL + 1
        IP   = IP   + 4
        CWHILE
N       SET IPCO TO 1. FREE LOCATION
        IPCO = IPCO + LHIT
C
C
      CPROC
C
C
N     *************************
N     *      I N I T          *
N     *************************
C
C
N     INITIALIZE CONSTANTS
      PROC INIT
C
        IQJETC = IBLN('JETC')
        IQHEAD = IBLN('HEAD')
C
N       RADIUS AROUND WIRE FOR CORR. OF DRIFTSPACE
        DRC = RINCR(1)*.5 * DRICOS
C       CONST. FOR VAR. OF DRIFT VEL.
N     GET RUN #
        IPHEAD = IDATA(IQHEAD)*2
        NRUN = HDATA(IPHEAD+10)
        IF NRUN.LE.100
        THEN
          DSD0   = .0
          DSD1   = .0
          DSD2   = .0
          DDS0   = .0
          DDS1   = .0
          DDS2   = .0
          DRV1   = .0
          DRV2   = .0
        ELSE
          DSD0   =-0.400
          DSD1   = 0.300
          DSD2   = 2.500
          DDS0   = 0.720
          DDS1   = 0.330
          DDS2   = 0.0
          DRV1   = (DDS0-DDS1) / (DSD0-DSD1)
          DRV2   = (DDS1-DDS2) / (DSD1-DSD2)
        CIF
C     PRINT 2091, DSD0,DDS0,DSD1,DDS1,DSD2,DDS2,DRV1,DRV2,DRC
C2091 FORMAT(' DSD,DDS=',3(F9.3,F7.3),F11.5,F9.5,F9.3,F8.3)
C
N       INITIALIZE ANGULAR CORRECTION CONSTANTS
        I9 = NCOAR - 1
        FOR I1=2,I9
          IF(I1.GT.2) TGCOAR(I1   ) = TGCOAR(I1- 1) + DTGB
          SLCOAR(I1   ) = (T0COAR(I1   )-T0COAR(I1- 1)) / DTGB
          SLCOAR(I1+15) = (T0COAR(I1+15)-T0COAR(I1+14)) / DTGB
          SLCOAR(I1+30) = (T0COAR(I1+30)-T0COAR(I1+29)) / DTGB
          SLCOAR(I1+45) = (T0COAR(I1+45)-T0COAR(I1+44)) / DTGB
        CFOR
C     PRINT 2092, TGCOAR,T0COAR,SLCOAR
C2092 FORMAT('0ANG.CORR.:',15F8.3,/,(11X,15F8.3))
      CPROC
C
      END
      SUBROUTINE JFETCH(IPTR,IPJHTL,WRK,LHIT,IPRES,INDEX,/XO/,/YO/)
C
C        P. STEFFEN                         83/03/28
C
C        MODIFIED TO CALL NEW JFETCH WHEN APPLICABLE   J.SPITZER
C                                                       86/04/30
C    Z-CHAMBER COORDINATES ARE FETCHED IN CASE OF
C    ZS-FIT (INDEX=4)
C                                            15/7/87  J.S.
C        FETCH HITS FOR TRACK 'IPTR' IN PATR-BANK
C        CALCULATE COORDINATE INCLUDING ALL CORRECTIONS
C        USE SPECIAL LAYER DEPENDENT POS. + VD   ***************
C        STORE COORDINATES IN WRK(I1),I1=1,LHIT*NHIT
C
C        INDEX = 1 : COORDINATES IN REAL SPACE
C        INDEX = 2 : X-AXIS THROUGH 1. + LAST POINT
C        INDEX = 3 : X-AXIS THROUGH (XO,YO) + LAST POINT
C
C        INDEX = 4 : NEW FOR S-Z FITS    J. SPITZER 22/4/87
C                    COORDINATES IN REAL SPACE
C
      IMPLICIT INTEGER*2 (H)
C
      DIMENSION WRK(200)
      EQUIVALENCE (ZWZ,IZW)
C---------------------------------
      COMMON/JSCALD/ JESCAL,JESKEY
C---------------------------------
#include "cdata.for"
C
#include "calibr.for"
C
C
#include "cjdrch.for"
#include "cdsmax.for"
C
      COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5), ZCHWW
C ONLY ZCHWW WHICH IS THE WEIGHT FOR Z-CHAMBER HITS IS USED HERE
C  ARRAYS FOR Z-CHAMBER INFORMATION
      DIMENSION IZCHMB(3,2),AZCHMB(3,2)
C
      DIMENSION IRESAR(13),RESAR(13),HRESAR(26)
      EQUIVALENCE (IRESAR(1),RESAR(1),HRESAR(1))
C
N     CONSTANTS FOR ANGULAR CORRECTION
      DATA NCOAR / 15/, DTGB / .15/
      REAL TGCOAR(15) /-99.,-.45, 12*0., 99./
      REAL T0COAR(60) / .000, .000, .000, .000, .000,
     ,     .000, .000,-.020,-.060,-.130,-.030, .100, .200, .200, .200,
     ,                  .000, .000, .010, .110, .100,
     ,     .075, .050, .025, .005, .015, .065, .060, .060, .060, .060,
     ,                  .190, .190, .180, .165, .140,
     ,     .120, .100, .075, .050, .010,-.050,-.075,-.035, .000, .000,
     ,                  .110, .110, .115, .140, .135,
     ,     .085, .045, .030, .040, .050, .055, .055, .055, .055, .055/
      REAL SLCOAR(60) / 60*0./
C
N     MASK FOR L/R BIT IN HIT LABEL
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
C
C-----------------------------------------------------------------------
C      --- NEW CALIBRATION WHEN AVAILABLE AND IF REQUESTED   J.S. ---
      IF JESCAL .GT. 0   .AND.   JESKEY .EQ. 54321
      THEN
         IP8=ICALIB(13)+6
         IP9=ICALIB(13)+2598
         CALL JFTNEW(IPTR,IPJHTL,WRK,LHIT,IPRES,INDEX,XO,YO,
     +   ACALIB(IP8),ACALIB(IP9))
         RETURN
      CIF
C
      INDX=INDEX
      IF(INDX.EQ.4) INDX=1
C-----------------------------------------------------------------------
C
C     IF(IDATA(IPTR+1).LT. 4) RETURN
C     I0 = IPTR + 1
C     I9 = IPTR + 48
C     PRINT 2001, (IDATA(I1),I1=I0,I9)
C     I0 = IPJHTL*2 + 1
C     I9 = I0 + IDATA(IPJHTL)*2 - 1
C     IPJETC = IDATA(IBLN('JETC'))
C     I0 = IPJETC*2 + 1
C     I9 = I0 + 109
C2001 FORMAT(1H0,2I3,I8,2(I4,3F6.1,3F6.3),
C    ,     /,14X,I3,4E13.5,F6.2,I3,4E13.5,
C    ,     /,14X,I3,2F8.3,F6.1,I3,10X,6I3,8I6,2X,Z4)
C2002 FORMAT('0FETCH:',2I3,2I5,12F9.5)
C2003 FORMAT('0ROTATION:',12F10.5)
C2004 FORMAT('0CIRC.CENTRE:',2I3, F10.5,2F10.0,F8.1,2F8.1)
C2005 FORMAT('0TRACK:',I6,/,(1X,3I6,4F9.3,I4,F9.3,2I4,F8.3,I6,3F8.2))
C2007 FORMAT(' FETCH:',I3,8F8.5,F10.7,F6.3)
C2009 FORMAT(' JHTL:',I8,1X,Z8,3I5,I8)
C2010 FORMAT(' HIT:',I6,12F8.2)
C2011 FORMAT('0ABERR:',10F10.6)
C2016 FORMAT('0ITRCLL =',6I8,/,(9X,6F8.3))
C
C2901 FORMAT('0JFETCH(PST) CALLED WITH WRONG INDEX:',I6)
C
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
        LBINIT = 1
        NCALL = 0
        PERFORM INIT
      CIF
      NCALL = NCALL + 1
C
CCC   RESERVE SPACE IN CWORK
CCC   HPCO0  = 1
CCCCC LHIT   = MAX0(LHIT,14)
      LHBIT  = LHIT*4
CCC   HPFREE = LHIT*100 + HPCO0
CCC   HPCO9  = 0
      IPCO = 1
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      NEVT = HDATA(IPHEAD+11)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
N     CENTRE OF CIRCLE (USED FOR ANGULAR CORRECTION)
      IF IDATA(IPTR+18).EQ.1
      THEN
N       CIRCLE PARAMETERS
        ALFA  = ADATA(IPTR+21)
        CRV   = ADATA(IPTR+19)
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        RAD   =  1./ABS(CRV) + ADATA(IPTR+20)
        XCIRC = COS(ALFA) * RAD
        YCIRC = SIN(ALFA) * RAD
        CHARGE = SIGN(1.,ADATA(IPTR+25))
      ELSE
N       PARABOLA PARAMETERS
        CRV   = ADATA(IPTR+22)*2.
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        ALFA  = ADATA(IPTR+19)
        XCIRC =-SIN(ALFA)/CRV + ADATA(IPTR+20)
        YCIRC = COS(ALFA)/CRV + ADATA(IPTR+21)
        CHARGE =-SIGN(1.,ADATA(IPTR+22))
      CIF
C
N     ZVERT, THETA + DIR. COSINES
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTHI = SQRT(TGTH**2 + 1.)
      CSTH  = 1. / CSTHI
      SNTH  = CSTH * TGTH
C     PRINT 2004,ITRK,IDATA(IPTR+18),ALFA,XCIRC,YCIRC,ZVERT,TGTH,CSTHI
C
C     PRINT 2011,ABERR
C
N     ROTATION ANGLE (USING LAST POINT OF TRACK)
      SELECT INDX
      CASE 1
        XT = 0.
        YT = 0.
        CSROT = 1.
        SNROT = 0.
        XOT   = 0.
        YOT   = 0.
      CASE 2
        XT    = (ADATA(IPTR+12) + ADATA(IPTR+5)) * .5
        YT    = (ADATA(IPTR+13) + ADATA(IPTR+6)) * .5
        XX    =  ADATA(IPTR+12) - ADATA(IPTR+5)
        YY    =  ADATA(IPTR+13) - ADATA(IPTR+6)
        RR    = SQRT(XX**2+YY**2)
        IF RR.LT.10.
        THEN
           IPRES=IPCO
           RETURN
        CIF
        CSROT = XX / RR
        SNROT = YY / RR
        XX    = XO - XT
        YY    = YO - YT
        XOT   = 0.
        YOT   = 0.
      CASE 3
        XT    = (ADATA(IPTR+12) + XO) * .5
        YT    = (ADATA(IPTR+13) + YO) * .5
        XX    =  ADATA(IPTR+12) - XO
        YY    =  ADATA(IPTR+13) - YO
        RR    = SQRT(XX**2+YY**2)
        CSROT = XX / RR
        SNROT = YY / RR
        XX    = XO - XT
        YY    = YO - YT
        XOT   = XX*CSROT + YY*SNROT
        YOT   =-XX*SNROT + YY*CSROT
      OTHER
C
N       ILLEGAL INDEX
C       PRINT 2901,INDEX
        RETURN
C
      CSELECT
C
C
N     SELECT CELLS CONTAINING TRACK
C
      IPC0 = IPTR + 34
      IPC9 = IPC0 +  5
      FOR IPC = IPC0,IPC9
         JCELL = IDATA(IPC)
         IF JCELL.GT. 0 .AND. JCELL.LE.96
         THEN
            JRING = 1
            IF(JCELL.GT.24) JRING = 2
            IF(JCELL.GT.48) JRING = 3
            PERFORM FETCH
         CIF
      CFOR
C
CCC   HPCO9 = IPCO - 1
CCC   PRINT 2005, LBCELL,(WRK(I),I=1,HPCO9)
C
C
C FETCH Z-CHAMBER HITS IN CASE Z-S FITS (INDEX=4)
      IF INDEX.EQ.4 .AND. ZCHWW.GT..1 .AND. ZCHWW.LT.2000.
      THEN
         CALL ZCFTNW(NRUN,NEVT,ITRK,TGTH,ZVERT,NZHIT,IZCHMB,AZCHMB)
         IF NZHIT.GT.0
         THEN
            FOR J=1,NZHIT
               HRESAR( 1) = 100+IZCHMB(1,J)
               HRESAR( 2) = IZCHMB(2,J)
               HRESAR( 3) = 0
               HRESAR( 4) = 0
               HRESAR( 5) = 1
C              HRESAR( 6) = IP-2*IPJETC
               HRESAR( 6) = 101
               XX=AZCHMB(1,J)
               YY=AZCHMB(2,J)
               ZZ=AZCHMB(3,J)
               RESAR ( 4) = XX
               RESAR ( 5) = YY
               RESAR ( 6) = ZZ
C CALCULATE TRACK LENGTH IN R-PHI FROM FIRST POINT ON TRACK
               UX=XX-ADATA(IPTR+5)
               UY=YY-ADATA(IPTR+6)
               UU=SQRT(UX**2+UY**2)
            IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
               IF(UX*ADATA(IPTR+8)+UY*ADATA(IPTR+9).LT.0.) UU=-UU
               RESAR ( 7) = UU
C              RESAR ( 8) = WW
               RESAR ( 8) = ZCHWW
               IF(NRUN.LT.24200) RESAR(8)=RESAR(8)*.6
               CALL MVC(WRK(IPCO),0,RESAR(1),0,LHBIT)
               IPCO = IPCO + LHIT
            CFOR
         CIF
      CIF
C
C
N     STORE RESULTS
      IPRES = IPCO
      IF INDEX.NE.4
      THEN
         WRK (IPRES   ) = XT
         WRK (IPRES+ 1) = YT
         WRK (IPRES+ 2) = CSROT
         WRK (IPRES+ 3) = SNROT
         WRK (IPRES+ 9) = XOT
         WRK (IPRES+10) = YOT
         WRK (IPRES+11) = CSTH
         WRK (IPRES+12) = SNTH
      CIF
C
C     PRINT 2003, CSROT,SNROT,XT,YT
C
C
      RETURN
C
C
N     *************************
N     *      F E T C H        *
N     *************************
C
C
N     FETCH HITS IN CELL
      PROC FETCH
C
N       DIR. OF SENSEW. + DRIFTSP.
        IF JRING.NE.3
        THEN
          IC1 = JCELL
          IF(IC1.GT.24) IC1 = IC1 - 24
          CSROT0 = DIRWR1(IC1,1)
          SNROT0 = DIRWR1(IC1,2)
        ELSE
          IC1 = JCELL - 48
          CSROT0 = DIRWR3(IC1,1)
          SNROT0 = DIRWR3(IC1,2)
        CIF
        DRICS  = TRMATC(JCELL,2)
        DRISN  = TRMATS(JCELL,2)
        DRITG  = DRISN/DRICS
        DRISNF = DRISN * .05
C
N       LOAD RADIUS AND WIRE SPACING
        R0 = FSENSW(JRING)
        DR = RINCR (JRING)
C
N       ANGLE OF TRACK IN RING
        R1   = DR*7.5 + R0
        DX   = R1 * CSROT0 - XCIRC
        DY   = R1 * SNROT0 - YCIRC
        RR   = SQRT(DX**2 + DY**2) * CHARGE
        CSB  = DX / RR
        SNB  = DY / RR
        TGB  = CSB/SNB
C
N       SET DRIFT SPACE BIN
        DSBIN1 = DRIVEL(JCELL,1)
C
N       ANGLE(TRACK,DRIFT DIRECT.)
        TANBET = (DRITG - TGB) / (TGB*DRITG + 1.)
C
N       DIFFERENT CORRECTION CONST. FOR MC + DATA
        IF NRUN.LE.100
        THEN
N         MC
          DS0 = DSBIN1*.5
          T0CORR = 0.
        ELSE
N         DATA
          DS0 = T0FIX(JRING)*DSBIN1*64.
          FOR I1=1,NCOAR
            IDX = I1
            IF(TANBET.LT.TGCOAR(IDX)) XFOR
          CFOR
          KRNG = JRING
          IF(KRNG.EQ.3 .AND. AND(JCELL,1).EQ.0) KRNG = 4
          IBIN = (KRNG-1)*NCOAR  + IDX
          T0CORR = (TANBET-TGCOAR(IDX)) * SLCOAR(IBIN) + T0COAR(IBIN)
        CIF
C     IF(NCALL.LE.2)
C    ,PRINT 2007, JCELL,CSROT0,SNROT0,DRICS,DRISN,CSB,SNB,CHARGE,TANBET,
C    ,            DSBIN1,DS0
C     IF(NCALL.LE.8)
C    ,PRINT 2093, JCELL,KRNG,IDX,IBIN,T0CORR,TGCOAR(IDX),T0COAR(IBIN),
C    ,            SLCOAR(IBIN),TANBET
C2093 FORMAT('0ANG.CORR.:',4I4,8F8.3)
N       CORRECTION CONSTANTS FOR JCELL
C
        IPJCOR = ICALIB(5) + JCELL
        CCST01 = ACALIB(IPJCOR     ) * ABS(TANBET)
        CCST02 = ACALIB(IPJCOR+  96) * ABS(TANBET)
        CCST11 = ACALIB(IPJCOR+ 192)
        CCST12 = ACALIB(IPJCOR+ 288)
        CCST21 = ACALIB(IPJCOR+ 384)
        CCST22 = ACALIB(IPJCOR+ 480)
        CCST51 = ACALIB(IPJCOR+ 576) * 10.
        CCST52 = ACALIB(IPJCOR+ 672) / 121.15
        CCST61 = ACALIB(IPJCOR+ 768) * 10.
        CCST62 = ACALIB(IPJCOR+ 864) / 121.15
        CCST81 = ACALIB(IPJCOR+1152)
C     IF(NCALL.LE.2)
C    ,PRINT 2002, JRING,JCELL,IP,IPCLLC,CCST01,CCST02,CCST11,CCST12,
C    ,  CCST21,CCST22,CCST51,CCST52,CCST61,CCST62,ACALIB(IPDY),CCST81
N       COUNTER FOR NUMBER OF HITS FOUND
        JHIT = 0
        NHIT   = 0
        NHGOOD = 0
N       PRESET LAST LAYER
        ILAYL =-99
N       LOOP OVER ALL HITS OF CELL
        IPCO = IPCO - LHIT
        IPJET4 = IDATA(IQJETC)
        IPJETC = IDATA(IQJETC)*2
        IP0    = IPJETC + 100
        IPCLL  = IPJETC + 2 + JCELL
        IP     = HDATA(IPCLL  ) + IP0
        IP9    = HDATA(IPCLL+1) + IP0
        IPHL   = IPJHTL + 2 + HDATA(IPCLL)/4
C     PRINT 2002, JRING,JCELL,IP,IP9,TGB,SNB,CSB,DRISN,DRICS
        WHILE IP.LT.IP9
C
N         CHECK TRACK # OF HIT LABEL
          LB   = IDATA(IPHL)
          ITR1 = LAND(SHFTR(LB,17),127)
          ITR2 = LAND(SHFTR(LB, 1),127)
C     PRINT 2009, IPHL,LB,ITR1,ITR2,ITRK,IP
          IF ITR1.EQ.ITRK .OR. ITR2.EQ.ITRK
          THEN
C
N           SET LBGOOD = 2 IF HIT ASSOCIATED WITH 2 TRACKS
            L0GOOD = 0
            IF(ITR1.NE.0 .AND. ITR2.NE.0) L0GOOD = 2
C
N           L/R FROM HIT LABEL
            LBLR = 0
            IF(ITR1.EQ.ITRK) LBLR = LAND(LB,MKLRT1)
            IF(ITR2.EQ.ITRK) LBLR = LAND(LB,MKLRT2)
            LBSIDE =-1
            IF(LBLR.NE.0) LBSIDE = 1
            LBLR = LBSIDE
C
            IWIR = HDATA(IP)
            IWIR = SHFTR(IWIR,3)
N           LAYER NUMBER WITHIN RING 3
            ILAY = LAND(IWIR,15)
N           AMPLITUDES
            IAMPL = HDATA(IP+1)
            IAMPR = HDATA(IP+2)
N           DRIFT SPACE
            DS =(HDATA(IP+3)) * DSBIN1
            X1   = ILAY * DR + R0
            Z1   = X1*TGTH + ZVERT
N           CORRECTION FOR TOF + PROPAG. ALONG WIRE
            DDS = (1222.9-ABS(Z1))*ABERR(1) + ABERR(6)*R1*CSTHI
N           CORRECTION FOR GRAVITATION
            DGR = ((Z1/1222.9)**2 - 1.) * .075
            DSC =  DS - DDS + DS0
C     DATA NPRHT /0/
C     NPRHT = NPRHT + 1
C     IF(NPRHT.LE.50) PRINT 2019, IWIR,ILAY,JCELL,HDATA(IP+3),DS,DSBIN1,
C    ,                DSC,DDS,DS0,ACALIB(IPVD+ILAY)
C2019 FORMAT(' HIT ',4I6,F7.3,5E13.5)
            Y1   = SWDEPL
            IF(LAND(ILAY,1).NE.0) Y1 =-Y1
            Y1   = (7.5-ILAY)*(CCST52*Z1+CCST51) - CCST62*Z1-CCST61 + Y1
            X    = X1*CSROT0 - Y1*SNROT0
            Y    = X1*SNROT0 + Y1*CSROT0 - DGR
            IF DSC.LE.DRC
            THEN
              IF DSC.LT.DSD2
              THEN
                IF DSC.LT.DSD1
                THEN
                  DSC = DSC + DDS1 + (DSC-DSD1)*DRV1
                ELSE
                  DSC = DSC + DDS2 + (DSC-DSD2)*DRV2
                CIF
N               ANGULAR CORRECTION
C               DSC = DSC/DSD2 * T0CORR + DSC
                IF(DSC.LT.0.1) DSC = 0.1
              ELSE
C
N               ANGULAR CORRECTION
C               DSC = DSC + T0CORR
                DSC = (DSC-DSD2)/(DRC-DSD2) * T0CORR + DSC
              CIF
C             DSC = DSC + DSOFF
              DXR  = DSC * CSB
              DYR  = DSC * SNB
              DXL =-DXR
              DYL =-DYR
            ELSE
C
N             ANGULAR CORRECTION
              DSC = DSC + T0CORR
C             DSC = DSC + DSOFF
C
N             EDGE WIRE FIELD DISTORTION
              IF ILAY.LT. 3
              THEN
                DILAY =-(ILAY- 3)**2
                DSCL  = (DILAY*CCST11 + 1.) * DSC * (1. - CCST81)
                DSCR  = (DILAY*CCST12 + 1.) * DSC * (1. + CCST81)
              ELSE
              IF ILAY.GT.12
              THEN
                DILAY =-(ILAY-12)**2
                DSCL  = (DILAY*CCST21 + 1.) * DSC * (1. - CCST81)
                DSCR  = (DILAY*CCST22 + 1.) * DSC * (1. + CCST81)
              ELSE
                DSCL = DSC * (1. - CCST81)
                DSCR = DSC * (1. + CCST81)
              CIF
              CIF
C
N             FIELD DISTORTIONS AT LARGE DRIFT TIMES
              IF DSC.GT.ABERR(7)
              THEN
                DWIR  = ILAY - 7.5
                DWIRC = DSC*DRISNF
                DWIRL = DWIR + DWIRC
                DWIRR = DWIR - DWIRC
                DSCL  = (DSCL-ABERR(7))*DWIRL*CCST01 + DSCL
                DSCR  =-(DSCR-ABERR(7))*DWIRR*CCST02 + DSCR
              CIF
              DXR  = (DSCR-DRC)*DRISN + DRC*CSB
              DYR  = (DSCR-DRC)*DRICS + DRC*SNB
              DXL  =-(DSCL-DRC)*DRISN - DRC*CSB
              DYL  =-(DSCL-DRC)*DRICS - DRC*SNB
            CIF
C     PRINT 2010, ILAY,DS,DSC,DSCL,DSCR,XL,XR,X,Y,DXL,DXR,DYL,DYR
            XL   = DXL + X - XT
            YL   = DYL + Y - YT
            XXL  = XL*CSROT + YL*SNROT
            YYL  =-XL*SNROT + YL*CSROT
            XR   = DXR + X - XT
            YR   = DYR + Y - YT
            XXR  = XR*CSROT + YR*SNROT
            YYR  =-XR*SNROT + YR*CSROT
C
N           CALCULATE Z COORDINATE
C           IF IAMPR.LE.0.OR.IAMPL.LE.0
C           THEN
C             ZZ     = 0.
C             LZGOOD = 16
C           ELSE
C             ZZ = IAMPR + IAMPL
C             ZZ = FLOAT(IAMPR-IAMPL) * ZAL*.5 / ZZ
C             LZGOOD = 0
C             IF(ABS(ZZ).GT.1250.) LZGOOD = 16
C           CIF
            CALL AMPS2Z( IP,IPJET4,ZZ,WW,LZGOOD)
C
N           SET ARRAY
C     PRINT 2010, ILAY,DS,XXL,YYL,X1,Z1,XXR,YYR,Y1
C
N           CHECK IF LEFT + RIGHT SOLUTION POSSIBLE
            NLRSOL = 1
            IF(DSC.LT.2.0) NLRSOL = 2
C
N           LOOP OVER LEFT +/OR RIGHT SOLUTION
            ILRSOL = 0
            REPEAT
            ILRSOL = ILRSOL + 1
            LBGOOD = L0GOOD
C
N             SELECT SIDE
              IF NLRSOL.EQ.1 .AND. LBSIDE.LT.0  .OR.
     ?           NLRSOL.EQ.2 .AND. ILRSOL.EQ.1
              THEN
N               LEFT SIDE
                LBSIDE =-1
                XX  = XXL
                YY  = YYL
              ELSE
N               RIGHT SIDE
                LBSIDE = 1
                XX  = XXR
                YY  = YYR
              CIF
C
N             HIT QUALITY:
              IF(LBSIDE.NE.LBLR) LBGOOD = LBGOOD + 1
N             NEW LAYER?
              IF ILAY.NE.ILAYL .OR. LBGDL.LE.1.AND.LBGOOD.LE.2
              THEN
                LBREG = 1
N               INCREASE HIT COUNTER
                JHIT = JHIT + 1
                IPCO = IPCO + LHIT
              ELSE
N               2 HITS IN SAME LAYER, SELECT CLOSEST
                LBREG = 0
                ZWZ = WRK(IPCO+10)
                IF(LBGOOD.LT.IZW) LBREG = 1
              CIF
N             REGISTER NEW HIT?
              IF LBREG.NE.0
              THEN
                NHIT   = NHIT   + 1
                IF(LBGOOD.LE.2) NHGOOD = NHGOOD + 1
                IF INDEX.NE.4
                THEN
                   IRESAR( 1) = ILAY
                   IRESAR( 2) = IP
                   IRESAR( 3) = LBSIDE
                   RESAR ( 4) = XX
                   RESAR ( 5) = YY
                   RESAR ( 6) = ZZ
                   RESAR ( 7) = XX - XOT
                   IF(INDX.EQ.1) RESAR ( 7) = SQRT(XX**2 + YY**2)
                   IRESAR( 8) = LZGOOD
                   RESAR ( 9) = DSC
                   IRESAR(10) = JCELL
                   IRESAR(11) = LBGOOD
                   RESAR (12) = TANBET
                   IRESAR(13) = JRING
                   RESAR (14) = 0.
                ELSE
                   HRESAR( 1) = JCELL
                   HRESAR( 2) = ILAY
                   HRESAR( 3) = LZGOOD
                   HRESAR( 4) = LBGOOD
                   HRESAR( 5) = 1
                   HRESAR( 6) = IP-IPJETC
                   RESAR ( 4) = XX
                   RESAR ( 5) = YY
                   RESAR ( 6) = ZZ
C CALCULATE TRACK LENGTH IN R-PHI FROM FIRST POINT ON TRACK
                   UX=XX-ADATA(IPTR+5)
                   UY=YY-ADATA(IPTR+6)
                   UU=SQRT(UX**2+UY**2)
                   CURVXY=ADATA(IPTR+25)
                   IF(ABS(CURVXY).LT.1.E-8) CURVXY = SIGN(1.E-8,CURVXY)
            IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
                   IF(UX*ADATA(IPTR+8)+UY*ADATA(IPTR+9).LT.0.) UU=-UU
                   RESAR ( 7) = UU
                   RESAR ( 8) = WW
                CIF
C     PRINT 2005, LHIT,(RESAR(I1),I1=1,13)
                CALL MVC(WRK(IPCO),0,RESAR(1),0,LHBIT)
CCCCC           IF LHIT.GT.14
CCCCC           THEN
CCCCC             I0 = IPCO+14
CCCCC             I9 = IPCO+LHIT - 1
CCCCC             FOR I1=I0,I9
CCCCC               WRK(I1) = 0.
CCCCC             CFOR
CCCCC           CIF
                ILAYL = ILAY
                LBGDL = LBGOOD
              CIF
C
            UNTIL ILRSOL.GE.NLRSOL
C
          CIF
C
        IPHL = IPHL + 1
        IP   = IP   + 4
        CWHILE
N       SET IPCO TO 1. FREE LOCATION
        IPCO = IPCO + LHIT
C
C
      CPROC
C
C
N     *************************
N     *      I N I T          *
N     *************************
C
C
N     INITIALIZE CONSTANTS
      PROC INIT
C
        IQJETC = IBLN('JETC')
        IQHEAD = IBLN('HEAD')
C
N       RADIUS AROUND WIRE FOR CORR. OF DRIFTSPACE
        DRC = RINCR(1)*.5 * DRICOS
C       CONST. FOR VAR. OF DRIFT VEL.
N     GET RUN #
        IPHEAD = IDATA(IQHEAD)*2
        NRUN = HDATA(IPHEAD+10)
        IF NRUN.LE.100
        THEN
          DSD0   = .0
          DSD1   = .0
          DSD2   = .0
          DDS0   = .0
          DDS1   = .0
          DDS2   = .0
          DRV1   = .0
          DRV2   = .0
        ELSE
          DSD0   =-0.400
          DSD1   = 0.300
          DSD2   = 2.500
          DDS0   = 0.720
          DDS1   = 0.330
          DDS2   = 0.0
          DRV1   = (DDS0-DDS1) / (DSD0-DSD1)
          DRV2   = (DDS1-DDS2) / (DSD1-DSD2)
        CIF
C     PRINT 2091, DSD0,DDS0,DSD1,DDS1,DSD2,DDS2,DRV1,DRV2,DRC
C2091 FORMAT(' DSD,DDS=',3(F9.3,F7.3),F11.5,F9.5,F9.3,F8.3)
C
N       INITIALIZE ANGULAR CORRECTION CONSTANTS
        I9 = NCOAR - 1
        FOR I1=2,I9
          IF(I1.GT.2) TGCOAR(I1   ) = TGCOAR(I1- 1) + DTGB
          SLCOAR(I1   ) = (T0COAR(I1   )-T0COAR(I1- 1)) / DTGB
          SLCOAR(I1+15) = (T0COAR(I1+15)-T0COAR(I1+14)) / DTGB
          SLCOAR(I1+30) = (T0COAR(I1+30)-T0COAR(I1+29)) / DTGB
          SLCOAR(I1+45) = (T0COAR(I1+45)-T0COAR(I1+44)) / DTGB
        CFOR
C     PRINT 2092, TGCOAR,T0COAR,SLCOAR
C2092 FORMAT('0ANG.CORR.:',15F8.3,/,(11X,15F8.3))
      CPROC
C
      END
C   09/06/83 802181229  MEMBER NAME  JFTNEW   (JADEGS)      SHELTRAN
      SUBROUTINE JFTNEW(IPTR,IPJHTL,WRK,LHIT,IPRES,INDEX,/XO/,/YO/,
     +CALCST,DSTORW)
C
C        J. SPITZER                           /10/86
C    UPDATED 4.4.87, FROM F11SPI.JADECAL.S
C    INCLUDE POSSIBILITY OF SUPPLYING PARTICLE MASS FOR
C    FLIGHT TIME CORRECTION                   1/6/87  J.S.
C
C    Z-CHAMBER COORDINATES ARE FETCHED IN CASE OF
C    ZS-FIT (INDEX=4)
C                                            15/7/87  J.S.
C
C        FETCH HITS FOR TRACK 'IPTR' IN PATR-BANK
C        CALCULATE COORDINATE INCLUDING ALL CORRECTIONS
C        STORE COORDINATES IN WRK(I1),I1=1,LHIT*NHIT
C
C        INDEX = 1 : COORDINATES IN REAL SPACE
C        INDEX = 2 : X-AXIS THROUGH 1. + LAST POINT
C        INDEX = 3 : X-AXIS THROUGH (XO,YO) + LAST POINT
C
C        INDEX = 4 : NEW FOR S-Z FITS    J. SPITZER 22/4/87
C                    COORDINATES IN REAL SPACE
C
C        LAST MOD: J. HAGEMANN 13/01/88  NEW FLAG VALUES FOR HITS OF
C                                        OVERLAPPING TRACKS (LBGOOD)
C                                        NOVL COUNTS THESE HITS
C                                        NOVL PASSED VIA COMMON/XYFVT1/
C
C  LBGOOD:  2=     OLD
C           2=  DIST. OF HIT-SEL. TRACK < DIST. OF HIT-SECOND TRACK
C          11=  DIST. OF HIT-SEL. TRACK = DIST. OF HIT-SECOND TRACK
C          12=  DIST. OF HIT-SEL. TRACK > DIST. OF HIT-SECOND TRACK
C
C
      IMPLICIT INTEGER*2 (H)
C
      DIMENSION WRK(200)
      EQUIVALENCE (ZWZ,IZW)
C
#include "cdata.for"
#include "cjdrch.for"
C
      COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5), ZCHWW
C ONLY ZCHWW WHICH IS THE WEIGHT FOR Z-CHAMBER HITS IS USED HERE
C  ARRAYS FOR Z-CHAMBER INFORMATION
      DIMENSION IZCHMB(3,2),AZCHMB(3,2)
C
N     * CALIBRATION CONSTANTS
      COMMON/JSCALD/ JESCAL,JESKEY,JESDRW
C
      COMMON/XYFVT1/MODXYV,NOVL
C
      COMMON /CFLMAS/ AFLMAS
      DIMENSION CALCST(96,27),DSTORW(5,64,24)
      REAL GG(4,8) / 1.16872E-1,-2.58070E-1, 1.32006E-1, 286.,
     +              -1.04290E-1, 8.84550E-2,-1.96380E-2, 286.,
     +               3.24418E-2,-7.46600E-2, 2.36765E-2, 496.,
     +              -5.75000E-2, 4.36000E-2,-1.10000E-2, 496.,
     +               3.12761E-2,-1.12856E-1, 6.26170E-2, 707.,
     +               5.44166E-1,-8.74361E-1, 3.05246E-1, 707.,
     +               3.54954E-1,-6.67922E-1, 2.74681E-1, 707.,
     +              -2.26062E-1, 3.02971E-1,-1.15400E-1, 707./
      REAL GGF(4,8)/         0.,         0.,         0., 286.,
     +                       0.,         0.,         0., 286.,
     +                       0.,         0.,         0., 496.,
     +                       0.,         0.,         0., 496.,
     +                       0.,         0.,         0., 707.,
     +               7.52764E-1,-1.10923E00, 3.47826E-1, 707.,
     +               2.52785E-1,-4.63690E-1, 1.75837E-1, 707.,
     +                       0.,         0.,         0., 707./
      REAL THL(4)/-52.,-52.,-4.,960./,
     +     THU(4)/-4.,-4.,960.,1570./,
     +     A2(4)/-52.,-1.97924E-5,-20.5739,0./,
     +     A3(4)/-9.224,700.,6.E-5,2.E-10/,
     +     A4(4)/1.497,1320.,-8.31E-6,-2.73E-10/
C
      REAL B1(4,4)/
     + 0.62000E+03,   0.87300E+03,  -0.41000E+03,   0.15800E+04,
     + 0.62000E+03,   0.87300E+03,  -0.41000E+03,   0.15800E+04,
     + 0.57030E+03,   0.89000E+03,  -0.38000E+03,   0.15800E+04,
     + 0.64300E+03,   0.90200E+03,  -0.41200E+03,   0.15800E+04/
C
      DIMENSION Q(5,4)
      DATA Q/14.5142,3.2743E-2,-6.E-6,0.,0.,
     +       4.46445E1,-8.87962E-2,1.29605E-4,9.02461E-9,-5.85976E-11,
     +       4.52471E1,-8.94577E-2,1.39668E-4,1.05065E-8,-7.46739E-11,
     +       18.256,3.46596E-2,-1.26438E-5,0.,0./
      DIMENSION P(5,4)
      DATA P/-.955408,1.62185E-3,-8.22074E-7,0.,0.,
     +       -.1736,1.41338E-3,-1.14314E-5,1.96957E-8,-7.93752E-12,
     +       -.173,2.2942E-4,-2.4298E-6,0.,0.,
     +       -1.0475,1.92375E-3,-1.2E-6,0.,0./
C
      REAL OMERIN(3)/2*.130900,.0654498/,ALORIN(3)/3*.34/,
     +     RR1(3)/211.,421.,632./,WIRDIS/10./,SMAXW(2,64),SM01(2,64),
     +     ANG375/.0654498/,
     +     FLTIM1/.028/,FLTIM2/.0363/,FLTIM3/1222.9/,ELFRCZ/.231/,
     +     AVFRMX/.78742/,EPS/1.E-4/,TANLOR/.365028/,SINLOR/.342898/,
     +     COSLOR/.939373/,PIVALU/3.141593/,
     +     PARVD(3)/.59562E-2,.59482E-2,.59421E-2/
C
      DATA JESOLD/-1/,LIMPRT/0/,KIMPRT/0/,LIMPR1/3/,KIMPR1/0/
C
      DIMENSION IRESAR(13),RESAR(13),HRESAR(13)
      EQUIVALENCE (IRESAR(1),RESAR(1),HRESAR(1))
C
C
N     MASK FOR L/R BIT IN HIT LABEL
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
C
      INDX=INDEX
      IF(INDX.EQ.4) INDX=1
      IF JESCAL.NE.JESOLD
      THEN
         JESOLD=JESCAL
         IF KIMPR1.LT.LIMPR1
         THEN
            KIMPR1=KIMPR1+1
            PRINT 720, JESCAL
720      FORMAT(' **** NEW ID CALIBRATION IN EFFECT IN JFETCH AFTER ',
     +   'RUN', I7,/,6X,'BIT 256 IS SET IN THE PROGRAM IDENTIFIER ',
     +   'WORD OF THE PATR BANK',////)
         CIF
      CIF
C
N     INITIALIZATION
      DATA LBINIT /0/,IQJETC/0/,IQHEAD/0/
      IF LBINIT .EQ. 0
      THEN
         LBINIT = 1
         IQJETC = IBLN('JETC')
         IQHEAD = IBLN('HEAD')
C
         A5=1./.6726
         FOR J=1,8
            GG(1,J)=GG(1,J)*A5
            GG(2,J)=GG(2,J)*A5*.5/GG(4,J)
            GG(3,J)=GG(3,J)*A5*.333333/GG(4,J)**2
            IF J.EQ.6.OR.J.EQ.7
            THEN
               GGF(1,J)=GGF(1,J)*A5
               GGF(2,J)=GGF(2,J)*A5*.5/GGF(4,J)
               GGF(3,J)=GGF(3,J)*A5*.333333/GGF(4,J)**2
            CIF
         CFOR
C
         FOR I=1,64
            IF I.LE.16
            THEN
               IRIN=1
               IW=I
            ELSE
               IF I.LE.32
               THEN
                  IRIN=2
                  IW=I-16
               ELSE
                  IRIN=3
                  IW=I-32
                  IF(IW.GT.16) IW=IW-16
               CIF
            CIF
            R=RR1(IRIN)+(IW-1)*WIRDIS
            SMAXW(1,I)=1.05*R*SIN(OMERIN(IRIN))/COS(ALORIN(IRIN)
     +      -OMERIN(IRIN))
            SMAXW(2,I)=1.05*R*SIN(OMERIN(IRIN))/COS(ALORIN(IRIN)
     +      +OMERIN(IRIN))
            SM01(1,I)=.7*SMAXW(1,I)
            SM01(2,I)=.7*SMAXW(2,I)
            IF IW.LT.3.OR.IW.GT.14
            THEN
               SM01(1,I)=.45*SMAXW(1,I)
               SM01(2,I)=.45*SMAXW(2,I)
            CIF
         CFOR
        PRINT 7777
 7777   FORMAT(' JFTNEW : VERSION FROM 13/01/88 CALLED!',/,
     &         ' +++++++++++++++++++++++++++++++++++++++++++++++++++++')
      CIF
C
      LHBIT  = LHIT*4
      IPCO = 1
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      IF NRUN.LT.24200
      THEN
         FREQR=1.0127
         FLTIM2=.0363
      ELSE
         FREQR=1.
         FLTIM2=0.
      CIF
      NEVT = HDATA(IPHEAD+11)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
N     SET FLAG FOR NEW CALIBRATION
      IDATA(IPTR+2) = LOR(IDATA(IPTR+2),256)
C
N     POINTER TO CALIBRATED JETC BANK
      IPJETC = IDATA(IQJETC)
      IP0    = 2*IPJETC + 100
C++++++++
N     LOCATE RAW JETC BANK
         IPRAW=IPJETC
         WHILE IDATA(IPRAW -1).GT.0
            IPRAW =IDATA(IPRAW -1)
         CWHILE
         IPRAW2=2*IPRAW -2*IPJETC
C========
C
N     ZVERT, THETA + DIR. COSINES
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTH = 1./SQRT(TGTH**2 + 1.)
      SNTH  = CSTH * TGTH
C
C
C++++++++
         XHCS = (ADATA(IPTR+12) + ADATA(IPTR+5)) * .5
         YHCS = (ADATA(IPTR+13) + ADATA(IPTR+6)) * .5
         XX    =  ADATA(IPTR+12) - ADATA(IPTR+5)
         YY    =  ADATA(IPTR+13) - ADATA(IPTR+6)
         RR    = SQRT(XX**2+YY**2)
         IF RR.LT.10.
         THEN
            IPRES=IPCO
            RETURN
         CIF
         CSROT = XX / RR
         SNROT = YY / RR
         ALCS=.5*RR
         SINFIC=SNROT
         COSFIC=CSROT
         IF(COSFIC.GT.1.) COSFIC=1.
         IF(COSFIC.LT.-1.) COSFIC=-1.
         FIC=ACOS(COSFIC)
         IF(SINFIC.LT.0.) FIC=2.*PIVALU-FIC
         BCS1X=-XHCS*COSFIC-YHCS*SINFIC
         BCS1Y= XHCS*SINFIC-YHCS*COSFIC
         CURVXY=ADATA(IPTR+25)
         IF(ABS(CURVXY).LT.1.E-8) CURVXY = SIGN(1.E-8,CURVXY)
C   FOLLOWING PARAMETERS ARE USED FOR CORRECTIONS ONLY
         IF(ABS(CURVXY*ALCS).GT..966) CURVXY=SIGN(.966/ALCS,CURVXY)
         CTGTH=TGTH
         IF(ABS(CTGTH).GT.2.) CTGTH=SIGN(2.,CTGTH)
C
         CPR0=CURVXY/SQRT(1.-(CURVXY*ALCS)**2)
         VCRS=1./CPR0
         CURN1=CURVXY*PARVD(2)
C========
C
N     ROTATION ANGLE (USING LAST POINT OF TRACK)
      SELECT INDX
      CASE 1
        XT = 0.
        YT = 0.
        CSROT = 1.
        SNROT = 0.
        XOT   = 0.
      CASE 2
        XT    = XHCS
        YT    = YHCS
        XOT   = 0.
      CASE 3
        XT    = (ADATA(IPTR+12) + XO) * .5
        YT    = (ADATA(IPTR+13) + YO) * .5
        XX    =  ADATA(IPTR+12) - XO
        YY    =  ADATA(IPTR+13) - YO
        RR    = SQRT(XX**2+YY**2)
        CSROT = XX / RR
        SNROT = YY / RR
C++
        XOT   = -.5*RR
      OTHER
N       ILLEGAL INDEX
        RETURN
      CSELECT
C
      NOVL = 0
C
N     SELECT CELLS CONTAINING TRACK
C
      IPC0 = IPTR + 34
      IPC9 = IPC0 +  5
      FOR IPC = IPC0,IPC9
         JCELL = IDATA(IPC)
         IF JCELL.GT. 0 .AND. JCELL.LE.96
         THEN
            JRING = 1
            IF(JCELL.GT.24) JRING = 2
            IF(JCELL.GT.48) JRING = 3
            PERFORM FETCH
         CIF
      CFOR
C
C FETCH Z-CHAMBER HITS IN CASE Z-S FITS (INDEX=4)
      IF INDEX.EQ.4 .AND. ZCHWW.GT..1 .AND. ZCHWW.LT.2000.
      THEN
         CALL ZCFTNW(NRUN,NEVT,ITRK,TGTH,ZVERT,NZHIT,IZCHMB,AZCHMB)
         IF NZHIT.GT.0
         THEN
            FOR J=1,NZHIT
               HRESAR( 1) = 100+IZCHMB(1,J)
               HRESAR( 2) = IZCHMB(2,J)
               HRESAR( 3) = 0
               HRESAR( 4) = 0
               HRESAR( 5) = 1
C              HRESAR( 6) = IP-2*IPJETC
               HRESAR( 6) = 101
               XX=AZCHMB(1,J)
               YY=AZCHMB(2,J)
               ZZ=AZCHMB(3,J)
               RESAR ( 4) = XX
               RESAR ( 5) = YY
               RESAR ( 6) = ZZ
C CALCULATE TRACK LENGTH IN R-PHI FROM FIRST POINT ON TRACK
               UX=XX-ADATA(IPTR+5)
               UY=YY-ADATA(IPTR+6)
               UU=SQRT(UX**2+UY**2)
            IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
               IF(UX*ADATA(IPTR+8)+UY*ADATA(IPTR+9).LT.0.) UU=-UU
               RESAR ( 7) = UU
C              RESAR ( 8) = WW
               RESAR ( 8) = ZCHWW
               IF(NRUN.LT.24200) RESAR(8)=RESAR(8)*.6
               CALL MVC(WRK(IPCO),0,RESAR(1),0,LHBIT)
               IPCO = IPCO + LHIT
            CFOR
         CIF
      CIF
C
N     STORE RESULTS
      IPRES = IPCO
      IF INDEX.LT.4
      THEN
         WRK (IPRES   ) = XT
         WRK (IPRES+ 1) = YT
         WRK (IPRES+ 2) = CSROT
         WRK (IPRES+ 3) = SNROT
         WRK (IPRES+ 9) = XOT
         WRK (IPRES+10) = 0.
         WRK (IPRES+11) = CSTH
         WRK (IPRES+12) = SNTH
      CIF
C
      RETURN
C
N     *************************
N     *      F E T C H        *
N     *************************
C
C
N     FETCH HITS IN CELL
      PROC FETCH
C
C+++++++
         KRING=JRING
         IF(JRING.EQ.3 .AND. JCELL-(JCELL/2)*2.NE.1) KRING=4
         IF JCELL.LE.24
         THEN
            ISEG=JCELL
         ELSE
            IF JCELL.LE.48
            THEN
               ISEG=JCELL-24
            ELSE
               ISEG=(JCELL-47)/2
            CIF
         CIF
         FISEGM=((ISEG-1)*4+2)*ANG375
N    MIDDLE OF CELL WITHIN SEGMENT
         ACEL1=CALCST(JCELL,18)
         ZETCEL=2.*SIN(.5*ACEL1*CURVXY)/CURVXY*TGTH+ZVERT
         IF(ABS(ZETCEL).GT.1000.) ZETCEL=SIGN(1000.,ZETCEL)
         ACEL1=   ACEL1        +ZETCEL*CALCST(JCELL,19)
         BCEL1=CALCST(JCELL,20)+ZETCEL*CALCST(JCELL,21)
         OCEL1=CALCST(JCELL,22)+ZETCEL*CALCST(JCELL,23)
         FIIC=FISEGM+OCEL1-FIC
         ACEL2=CALCST(JCELL,19)*CTGTH*WIRDIS
         BCEL2=CALCST(JCELL,21)*CTGTH*WIRDIS
         OCEL2=CALCST(JCELL,23)*CTGTH*WIRDIS
         ROT1X=COS(FIIC)
         ROT1Y=SIN(FIIC)
         ROT2X=SIN(FIC-FISEGM)
         ROT2Y=COS(FIC-FISEGM)
         BCS1XC=BCS1X+ACEL1*ROT2Y+BCEL1*ROT2X
         BCS1YC=BCS1Y-ACEL1*ROT2X+BCEL1*ROT2Y
C=========
C
N       COUNTER FOR NUMBER OF HITS FOUND
        JHIT = 0
        NHIT   = 0
        NHGOOD = 0
N       PRESET LAST LAYER
        ILAYL =-99
N       LOOP OVER ALL HITS OF CELL
        IPCO = IPCO - LHIT
        IPCLL  = 2*IPJETC + 2 + JCELL
        IP     = HDATA(IPCLL  ) + IP0
        IP9    = HDATA(IPCLL+1) + IP0
        IPHL   = IPJHTL + 2 + HDATA(IPCLL)/4
        WHILE IP.LT.IP9
C
N         CHECK TRACK # OF HIT LABEL
          LB   = IDATA(IPHL)
          ITR1 = LAND(SHFTR(LB,17),127)
          ITR2 = LAND(SHFTR(LB, 1),127)
          IF ITR1.EQ.ITRK .OR. ITR2.EQ.ITRK
          THEN
C
N           SET LBGOOD =  2,11,12 IF HIT ASSOCIATED WITH 2 TRACKS
            L0GOOD = 0
            IF ITR1.NE.0 .AND. ITR2.NE.0
            THEN
               NOVL   = NOVL + 1
               L0GOOD = 11
               ID1    = LAND(SHFTR(LB,27), 31)
               ID2    = LAND(SHFTR(LB,11), 31)
               IF ITR1 .EQ. ITRK
               THEN
                  IF( ID1 .LT. ID2 ) L0GOOD = 2
                  IF( ID1 .GT. ID2 ) L0GOOD = 12
               ELSE
                  IF( ID2 .LT. ID1 ) L0GOOD = 2
                  IF( ID2 .GT. ID1 ) L0GOOD = 12
               CIF
            CIF
C
N           L/R FROM HIT LABEL
            LBLR = 0
            IF(ITR1.EQ.ITRK) LBLR = LAND(LB,MKLRT1)
            IF(ITR2.EQ.ITRK) LBLR = LAND(LB,MKLRT2)
            LBSIDE =-1
            IF(LBLR.NE.0) LBSIDE = 1
            LBLR = LBSIDE
C
            IWIR = HDATA(IP)
            IWIR = SHFTR(IWIR,3)
N           LAYER NUMBER WITHIN RING 3
            ILAY = LAND(IWIR,15)
N           AMPLITUDES
            IAMPL = HDATA(IP+1)
            IAMPR = HDATA(IP+2)
N           CALCULATE Z COORDINATE
C           IF IAMPR.LE.0.OR.IAMPL.LE.0
C           THEN
C             ZZ     = 0.
C             LZGOOD = 16
C           ELSE
C             ZZ = IAMPR + IAMPL
C             ZZ = FLOAT(IAMPR-IAMPL) * ZAL*.5 / ZZ
C             LZGOOD = 0
C             IF(ABS(ZZ).GT.1250.) LZGOOD = 16
C           CIF
            CALL AMPS2Z( IP,IPJETC,ZZ,WW,LZGOOD)
C
C+++++++
N     WIRE NUMBER WITHIN CELL 1..16
            IW=ILAY+1
            IODD=1
            IF(IW-(IW/2)*2.EQ.0) IODD=-1
            RHIT=ACEL1+(IW-8.5)*WIRDIS
            FLPATH=2.*SIN(.5*RHIT*CURVXY)/CURVXY
            ZHIT=FLPATH*TGTH+ZVERT
            IF(ABS(ZHIT).GT.1200.) ZHIT=SIGN(1200.,ZHIT)
            FLPATH=SQRT(FLPATH**2+ZHIT**2)
C
N     DRIFT TIME (FROM RAW BANK) + CORRECTIONS
            TDRIFT=HDATA(IP+3+IPRAW2)
            IF NRUN.LT.24200
            THEN
               TDRIFT=TDRIFT*64.+32.
               IF(NRUN.GE.19050.AND.NRUN.LE.20274) TDRIFT=TDRIFT+20.
               IF(NRUN.GE. 3300.AND.NRUN.LE. 3550) TDRIFT=TDRIFT-90.
            ELSE
               IF NRUN.LE.24698
               THEN
                  TDRIFT=TDRIFT-5.
                  IF(NRUN.LT.24405) TDRIFT=TDRIFT+153.
                  IF(NRUN.GE.24227.AND.NRUN.LE.24232) TDRIFT=TDRIFT+147.
                  IF(NRUN.GE.24233.AND.NRUN.LE.24245) TDRIFT=TDRIFT+297.
               CIF
            CIF
            AMRAWL=HDATA(IP+1+IPRAW2)*8.
            AMRAWR=HDATA(IP+2+IPRAW2)*8.
N     SLEWING CORRECTION
            PERFORM SLWCOR
N     CLOCK FREQUENCY
            IF(JRING.EQ.3) TDRIFT=TDRIFT*FREQR
N     FLIGHT AND PROPAGATION TIME
            BKGS=ABS(HDATA(IPHEAD+30)*.001)
            IF(BKGS.LT.3.) BKGS=4.8
            AMOMGV=.02998E-3*BKGS*SQRT(1.+CTGTH**2)/CURVXY
            CPERV=SQRT(1.+(AFLMAS/AMOMGV)**2)
            TDRIFT=TDRIFT-FLTIM1*FLPATH*CPERV-FLTIM2*(FLTIM3-ABS(ZHIT))
N     T0
            TDRIFT=TDRIFT-CALCST(JCELL,IW)
N     * CALCULATE "WIRE NUMBER CORRECTION"
            TSTG=CALCST(JCELL,17)*(1.-ELFRCZ*(ZHIT/1200.)**2)
     +      *AVFRMX*SINLOR*PARVD(JRING)/WIRDIS
            XK=IW-IODD*TSTG-8.5
            ROT3X=XK*WIRDIS*(ROT1X-ROT1Y*XK*OCEL2)
            ROT3Y=XK*WIRDIS*(ROT1Y+ROT1X*XK*OCEL2)
N     * WIRE IN THE "CIRCLE" SYSTEM
            XWPR=ROT3X+BCS1XC+XK*( ACEL2*ROT2Y+BCEL2*ROT2X)
            YWPR=ROT3Y+BCS1YC+XK*(-ACEL2*ROT2X+BCEL2*ROT2Y)
C========
N           CHECK IF LEFT + RIGHT SOLUTION POSSIBLE
            NLRSOL = 1
C++
            IF(TDRIFT.LT.300.) NLRSOL = 2
C
N           LOOP OVER LEFT +/OR RIGHT SOLUTION
            ILRSOL = 0
            REPEAT
            ILRSOL = ILRSOL + 1
            LBGOOD = L0GOOD
C
N             SELECT SIDE
              IF NLRSOL.EQ.1 .AND. LBSIDE.LT.0  .OR.
     ?           NLRSOL.EQ.2 .AND. ILRSOL.EQ.1
              THEN
N               LEFT SIDE
                LBSIDE =-1
C++
                PERFORM GETCOR
              ELSE
N               RIGHT SIDE
                LBSIDE = 1
C++
                PERFORM GETCOR
              CIF
C
N             HIT QUALITY:
              IF(LBSIDE.NE.LBLR) LBGOOD = LBGOOD + 1
N             NEW LAYER?
              IF ILAY.NE.ILAYL .OR. LBGDL.LE.1.AND.LBGOOD.LE.2
              THEN
                LBREG = 1
N               INCREASE HIT COUNTER
                JHIT = JHIT + 1
                IPCO = IPCO + LHIT
              ELSE
N               2 HITS IN SAME LAYER, SELECT CLOSEST
                LBREG = 0
                ZWZ = WRK(IPCO+10)
                IF(LBGOOD.LT.IZW) LBREG = 1
              CIF
N             REGISTER NEW HIT?
              IF LBREG.NE.0
              THEN
                NHIT   = NHIT   + 1
                IF(LBGOOD.LE.2) NHGOOD = NHGOOD + 1
                IF INDEX.NE.4
                THEN
                   IRESAR( 1) = ILAY
                   IRESAR( 2) = IP
                   IRESAR( 3) = LBSIDE
                   RESAR ( 4) = XX
                   RESAR ( 5) = YY
                   RESAR ( 6) = ZZ
                   RESAR ( 7) = XX - XOT
                   IF(INDX.EQ.1) RESAR ( 7) = SQRT(XX**2 + YY**2)
                   IRESAR( 8) = LZGOOD
                   RESAR ( 9) = DSC
                   IRESAR(10) = JCELL
                   IRESAR(11) = LBGOOD
                   RESAR (12) = TANBET
                   IRESAR(13) = JRING
                   RESAR (14) = 0.
                ELSE
                   HRESAR( 1) = JCELL
                   HRESAR( 2) = ILAY
                   HRESAR( 3) = LZGOOD
                   HRESAR( 4) = LBGOOD
                   HRESAR( 5) = 1
                   HRESAR( 6) = IP-2*IPJETC
                   RESAR ( 4) = XX
                   RESAR ( 5) = YY
                   RESAR ( 6) = ZZ
C CALCULATE TRACK LENGTH IN R-PHI FROM FIRST POINT ON TRACK
                   UX=XX-ADATA(IPTR+5)
                   UY=YY-ADATA(IPTR+6)
                   UU=SQRT(UX**2+UY**2)
            IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
                   IF(UX*ADATA(IPTR+8)+UY*ADATA(IPTR+9).LT.0.) UU=-UU
                   RESAR ( 7) = UU
                   RESAR ( 8) = WW
                CIF
                CALL MVC(WRK(IPCO),0,RESAR(1),0,LHBIT)
                ILAYL = ILAY
                LBGDL = LBGOOD
              CIF
C
            UNTIL ILRSOL.GE.NLRSOL
C
          CIF
C
        IPHL = IPHL + 1
        IP   = IP   + 4
        CWHILE
N       SET IPCO TO 1. FREE LOCATION
        IPCO = IPCO + LHIT
C
      CPROC
C
N     *************************
N     *      G E T C O R      *
N     *************************
C
      PROC GETCOR
         IF LBSIDE.LT.0
         THEN
            AG2= CALCST(JCELL,26)
            VDP=-CALCST(JCELL,24)
         ELSE
            AG2= CALCST(JCELL,27)
            VDP= CALCST(JCELL,25)
         CIF
         AG2=AG2-FIIC-XK*OCEL2
         SINAG2=SIN(AG2)
         COSAG2=COS(AG2)
C CALCULATE DELTA=SIGNED CHANGE OF DRIFT TIME PRO WIRE SPACING
         F=XWPR*SINAG2+(YWPR+VCRS)*COSAG2
         G=(ALCS-XWPR)*(ALCS+XWPR)-YWPR*(YWPR+2.*VCRS)
         IF G.GT.-.98*F**2
         THEN
N     * WIRE CIRCLE DISTANCE ALONG DRIFT DIR.
            PERFORM CALSQT
            DISTWC=SQTVAL
            XPR=XWPR+DISTWC*SINAG2
            DYPDXP=1.-(CURVXY*XPR)**2
            IF DYPDXP.LT..02
            THEN
               IF KIMPRT.LT.LIMPRT
               THEN
                  KIMPRT=KIMPRT+1
                  PRINT 675,CURVXY,ALCS,XPR,NRUN,NEVT,ITRK
675      FORMAT(' *** ERROR IN JFTNEW *** CURVATURE, HALF TRACK LENGTH',
     +   ' X IN TR C.S.',/,8X,3E15.5,'   TRACK',I9,I6,I4)
C
                  PRINT 676, CPR0,VCRS,XHCS,YHCS,FIC,TGTH,
     +            XWPR,YWPR,AG2,F,G,DISTWC,DYPDXP,
     +            KRING,JCELL,ISEG,FISEGM,IW,IODD,ILRSOL,LBSIDE
676               FORMAT(/,' CPR0,VCRS,XHCS,YHCS,FIC,TGTH,',/,
     +            ' XWPR,YWPR,AG2,F,G,DISTWC,DYPDXP,'/,
     +            ' KRING,JCELL,ISEG,FISEGM,IW,IODD,ILRSOL,LBSIDE',/,
     +            1X,6E15.5,/,1X,7E15.5,/,1X,3I4,E15.5,4I6,////)
               CIF
               DYPDXP=.02
            CIF
            DYPDXP=-XPR*CURVXY/SQRT(DYPDXP)
            C=1.-DYPDXP*SINAG2/COSAG2
            IF(ABS(C).LT..001) C=SIGN(.001,C)
            TANBET=(DYPDXP+SINAG2/COSAG2)/C
            AMU=WIRDIS/PARVD(JRING)*COSLOR
            DELTA=AMU*(TANBET-TANLOR)
            IF(ABS(DELTA).GT.1800.) DELTA=SIGN(1800.,DELTA)
         ELSE
C           HIT CAN NOT BE ON THE TRACK. FOR ANGLE DEPENDENT
C           CORRECTIONS A TRACK PARALELL WITH THE WIRE PLANE
C           WILL BE ASSUMED
            IF KIMPRT.LT.LIMPRT
            THEN
               KIMPRT=KIMPRT+1
               PRINT 674,CURVXY,ALCS,NRUN,NEVT,ITRK
674      FORMAT(' *** ERROR IN JFTNEW *** CURVATURE, HALF TRACK LENGTH',
     +   /,8X,2E15.5,'   TRACK',I9,I6,I4)
               PRINT 677, CPR0,VCRS,XHCS,YHCS,FIC,TGTH,
     +         XWPR,YWPR,AG2,F,G,
     +         KRING,JCELL,ISEG,FISEGM,IW,IODD,ILRSOL,LBSIDE
677            FORMAT(/,' CPR0,VCRS,XHCS,YHCS,FIC,TGTH,',/,
     +         ' XWPR,YWPR,AG2,F,G,'/,
     +         ' KRING,JCELL,ISEG,FISEGM,IW,IODD,ILRSOL,LBSIDE',/,
     +         1X,6E15.5,/,1X,5E15.5,/,1X,3I4,E15.5,4I6,////)
            CIF
            TANBET=TANLOR
            DELTA=0.
         CIF
C
N     *  Z AND THETA DEPENDENT SLEWING
         PERFORM SLWZTH
N     * CLOSE WIRE CORRECTION
         PERFORM CLWCOR
N     * CORRECT FOR STAGGERING AND TRACK ANGLE FI
         PERFORM STGANG
C
N     * DISTANCE FROM WIRE
         Y1=VDP*TCORR
N     * DISTORTIONS
         IF(JESDRW.GT.0) PERFORM DSTRTN
         DSC=ABS(Y1)
         XX=XWPR+Y1*SINAG2
         YY=YWPR+Y1*COSAG2
         IF INDX.NE.2
         THEN
            A= XX*COSFIC-YY*SINFIC+XHCS
            YY=XX*SINFIC+YY*COSFIC+YHCS
            XX=A
            IF INDX.EQ.3
            THEN
               A = (XX-XT)*CSROT+(YY-YT)*SNROT
               YY=-(XX-XT)*SNROT+(YY-YT)*CSROT
               XX=A
            CIF
         CIF
      CPROC
C
N     *************************
N     *      S L W C O R      *
N     *************************
C
      PROC SLWCOR
      A=AMRAWL
      IF(AMRAWR.GT.A) A=AMRAWR
      IF(A.LT.10.) A=10.
N    * SLEWING FOR RAW AMPLITUDES
      IF NRUN.GE. 24200
      THEN
         IF A.GT.1800.
         THEN
            TSLEW=-1.449+1.19097E-3*(A-2000.)
         ELSE
            TSLEW=-2.100+2.11521E-3*(A-1600.)-8.50349E-12*(1600.-A)**4
         CIF
      ELSE
         IF A.GT.5000.
         THEN
            TSLEW=-50.+5.80000E-3*(A-5000.)
         ELSE
            IF A.LT.300.
            THEN
               TSLEW=-200.
            ELSE
         TSLEW=-4472.05*A**(-5.23557E-1-6.42692E-3*(ALOG(A)-7.77529)**2)
               IF(NRUN.GE.20275.AND.A.LT.1500.)
     +         TSLEW=TSLEW-(A-1500.)**2*2.26664E-5
            CIF
            IF(A.LT.650. .AND.(NRUN.GE.20275 .OR.
     +      NRUN.GE.13000 .AND. NRUN.LE. 14599) )
     +      TSLEW=TSLEW+116.6-1.79687E-1*A
            IF(A.LT.800. .AND. NRUN.GE.11473 .AND. NRUN.LE.12554)
     +      TSLEW=TSLEW+139.4-1.74800E-1*A
         CIF
      CIF
      TDRIFT=TDRIFT+TSLEW
      CPROC
C
N     *************************
N     *      S L W Z T H      *
N     *************************
C
      PROC SLWZTH
      ACTG=ABS(CTGTH)
      IF NRUN.GE. 24200
      THEN
         ZTSLW=0.
         IF KRING.EQ.4
         THEN
            IF LBSIDE.LT.0
            THEN
               ZTSLW=-19.43-14.5942*ACTG+19.8951*ACTG**2
               IF ACTG.LT..42
               THEN
                  ZTSLW=ZTSLW+5.1921+3.216*ACTG-82.49*ACTG**2
               ELSE
                  ZTSLW=ZTSLW-24.66+48.9578*ACTG-22.7265*ACTG**2
               CIF
            ELSE
               ZTSLW=5.918-5.45559*ACTG-2.12*ACTG**2
            CIF
         CIF
         IF KRING.EQ.3
         THEN
            IF LBSIDE.LT.0
            THEN
               ZTSLW=-.937-8.66313*ACTG+9.8988*ACTG**2
            ELSE
               ZTSLW= 2.46- 3.8375*ACTG-14.5671*ACTG**2
            CIF
         CIF
         J=2*KRING
         IF(LBSIDE.LT.0) J=J-1
         IF J.EQ.6.OR.J.EQ.7
         THEN
            AZ=ABS(ZHIT)
            BZ=ACTG*GGF(4,J)
            ZTSLW=ZTSLW+(AZ-BZ)*(GGF(1,J)+GGF(2,J)*(AZ+BZ)+GGF(3,J)
     +      *(AZ*(AZ+BZ)+BZ**2))
         CIF
      ELSE
         IF KRING.EQ.1
         THEN
            ZTSLW=13.46-14.03*ACTG
         ELSE
            IF KRING.EQ.2
            THEN
               ZTSLW=15.23-31.278*ACTG+7.54731*ACTG**2
            ELSE
               ZTSLW=20.86-48.672*ACTG+13.663*ACTG**2
            CIF
         CIF
C
         IF KRING.EQ.1
         THEN
            IF LBSIDE.LT.0
            THEN
               IF ACTG.LT..37
               THEN
                  T1=10.30-26.1*ACTG
               ELSE
                  T1=3.88-18.5345*ABS(ACTG-.5427)
               CIF
               T1=T1-0.30+ 0.77*ACTG-0.7648*ACTG**2
            ELSE
               IF ACTG.LT..37
               THEN
                  T1=7.50-26.3*ACTG
               ELSE
                  T1=-1.95+6.84118*ABS(ACTG-.4)
               CIF
               T1=T1+3.21-14.10*ACTG+10.73*ACTG**2
            CIF
            ZTSLW=ZTSLW+T1
         CIF
         IF KRING.EQ.2
         THEN
            IF LBSIDE.LT.0
            THEN
               IF ACTG.LT..40
               THEN
                  T1=8.787-19.675*ACTG
               ELSE
                  T1=4.091-18.133*ABS(ACTG-.56)
               CIF
            ELSE
               IF ACTG.LT..48
               THEN
                  T1=1.983-12.667*ACTG
               ELSE
                  T1=-4.1125+13.574*ABS(ACTG-.5)
               CIF
            CIF
            ZTSLW=ZTSLW+T1
         CIF
         IF KRING.EQ.3
         THEN
            T1=8.336-12.3519*ACTG
            IF LBSIDE.LT.0
            THEN
               T1=T1-2.68+18.09*ACTG-15.95*ACTG**2
            ELSE
               T1=T1+1.20-14.489*ACTG+14.623*ACTG**2
               IF(NRUN.GE.8712.AND.NRUN.LE.9999)
     +         T1=T1+14.20-8.5415*ACTG-10.6000*ACTG**2
               IF(NRUN.GE.7592.AND.NRUN.LE.8711)
     +         T1=T1+4.16+17.97*ACTG-24.33*ACTG**2
            CIF
            ZTSLW=ZTSLW+T1
         CIF
         IF KRING.EQ.4
         THEN
            IF ACTG.LT..56
            THEN
               T1=5.58-15.183*ACTG
            ELSE
               T1=-3.30+8.1613*(ACTG-.5)
            CIF
            IF LBSIDE.LT.0
            THEN
               T1=T1+3.12-21.16*ACTG+18.90*ACTG**2
               IF(NRUN.GE.11038.AND.NRUN.LE.12554)
     +         T1=T1+.39+29.1785*ACTG-30.4402*ACTG**2
               IF(NRUN.GE.8712.AND.NRUN.LE.9999)
     +         T1=T1+7.30+13.8*ACTG-23.20*ACTG**2
               IF(NRUN.GE.7592.AND.NRUN.LE.8711)
     +         T1=T1+0.16+ 9.08*ACTG- 9.60*ACTG**2
               IF(NRUN.GE.6185.AND.NRUN.LE.7591)
     +         T1=T1-16.6+41.18*ACTG-20.60*ACTG**2
            ELSE
               T1=T1-0.62+12.25*ACTG-10.52*ACTG**2
            CIF
            ZTSLW=ZTSLW+T1
         CIF
C
         J=2*KRING
         IF(LBSIDE.LT.0) J=J-1
         AZ=ABS(ZHIT)
         BZ=ACTG*GG(4,J)
         ZTSLW=ZTSLW+(AZ-BZ)*(GG(1,J)+GG(2,J)*(AZ+BZ)+GG(3,J)
     +   *(AZ*(AZ+BZ)+BZ**2))
         IF KRING.GE.3
         THEN
            ZTSLW=ZTSLW+1.70E-2/.6727*(ZHIT-CTGTH*GG(4,J))
         CIF
C
         IF(KRING.GE.3) ZTSLW=ZTSLW*FREQR
      CIF
      TCORR=TDRIFT+ZTSLW
      CPROC
C
N     *************************
N     *      C L W C O R      *
N     *************************
C
      PROC CLWCOR
C     APPLY CLOSE WIRE CORRECTION
      IF TCORR.LT.THU(4)
      THEN
         TCOR=TCORR
         IF TCOR.GT.THL(4)
         THEN
            TCOR=TCOR+A4(1)+A4(3)*(TCOR-A4(2))**2+A4(4)*(TCOR-A4(2))**4
         ELSE
            IF TCOR.GT.THL(3)
            THEN
               TCOR=TCOR+A3(1)+A3(3)*(TCOR-A3(2))**2
     +         +A3(4)*(TCOR-A3(2))**4
            ELSE
               IF TCOR.GT.THL(2)
               THEN
                  TCOR=TCOR-A2(1)+A2(2)*((TCOR-A2(3))**4-
     +            (A2(1)-A2(3))**4)
               ELSE
                  TCOR=TCOR-A2(1)
               CIF
            CIF
         CIF
         IF(TCOR.GT.0..AND.TCOR.LT.120.) TCOR=TCOR-8.E-2*(TCOR-120.)
         TCORR=TCOR
      CIF
      CPROC
C
N     *************************
N     *      S T G A N G      *
N     *************************
C
      PROC STGANG
         PERFORM STGFIZ
         STGTC=CALCST(JCELL,17)*IODD*LBSIDE*STGCOR
C
         IF DELTA.GT.B1(3,KRING)
         THEN
            A12=B1(2,KRING)
         ELSE
            A12=B1(1,KRING)
         CIF
         CSGINV=SQRT(1.+((DELTA-B1(3,KRING))/B1(4,KRING))**2)
         IF TCORR.GT.A12
         THEN
C           Y=A12*CURN1*LBSIDE
C           IF ABS(Y).GT.1.E-5
C           THEN
C!!            TANGCC=A12*(-SQTVAL(1./(Y*CSGINV),1.-2./Y,1.,1.E-4)-1.)
C           ELSE
               TANGCC=A12*(CSGINV-1.)
C           CIF
         ELSE
C           Y=TCORR*CURN1*LBSIDE
C           IF ABS(Y).GT.1.E-5
C           THEN
C !!           TANGCC=TCORR*(SQTVAL(1./(Y*CSGINV),1.-2./Y,1.,1.E-4)+1.)
C           ELSE
               TANGCC=TCORR*(CSGINV-1.)
C           CIF
         CIF
         TCORR=TCORR+TANGCC+STGTC
      CPROC
C
N     *************************
N     *      S T G F I Z      *
N     *************************
C
      PROC STGFIZ
C
      D=DELTA
      Z=ZHIT
      PERFORM STGZ0
      PERFORM FRACT
      U=(Z/1200.)**2
      STGCOR=STGDZ0*(1.+U*FRACZD)
      IF(STGCOR.LT..2) STGCOR=.2
      CPROC
C-----------------------------------------------------------------------
      PROC STGZ0
N     * CALCULATE D DEPENDENT STAG FRAC AT Z=0
         IF D.LT.-600.
         THEN
            I=1
         ELSE
            IF D.LT.0.
            THEN
               I=2
            ELSE
               IF D.LT.600.
               THEN
                  I=3
               ELSE
                  I=4
               CIF
            CIF
         CIF
         STGDZ0=Q(1,I)+ABS(D)*(Q(2,I)+Q(4,I)*D**2)+Q(3,I)*D**2
     +   +Q(5,I)*D**4
         STGDZ0=STGDZ0/44.9444
      CPROC
C-----------------------------------------------------------------------
      PROC FRACT
N     * CALCULATE D DEPENDENT EL.STAT.FRACTION
         IF D.LT.-500.
         THEN
            I=1
         ELSE
            IF D.LT.0.
            THEN
               I=2
            ELSE
               IF D.LT.400.
               THEN
                  I=3
               ELSE
                  I=4
               CIF
            CIF
         CIF
         FRACZD=P(1,I)+ABS(D)*(P(2,I)+P(4,I)*D**2)+P(3,I)*D**2
     +   +P(5,I)*D**4
      CPROC
C
N     *************************
N     *      D S T R T N      *
N     *************************
C
      PROC DSTRTN
C
      IWR=(KRING-1)*16+IW
      IF Y1.GT.0.
      THEN
         IND=2
      ELSE
         IND=1
      CIF
      SMAX=SMAXW(IND,IWR)
      SM0=SM01(IND,IWR)
      Y1COR=DSTORW(2*IND,IWR,ISEG)*Y1**2    +   DSTORW(5,IWR,ISEG)
      S0=ABS(Y1)-SM0
      IF(S0.GT.0.) Y1COR=Y1COR+DSTORW(2*IND-1,IWR,ISEG)*S0**2
      IF IW.EQ.1 .OR. IW.EQ.16
      THEN
         X=ABS(Y1/SMAX)
         IF IWR.EQ.1
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..52
               THEN
                  T=-.07*(1.-((2.*X-.52)/.52)**2)
               ELSE
                  IF X.LT.1.
                  THEN
                     T=.05*(1.-((2.*X-1.37)/.33)**2)
                  ELSE
                     T=-.13
                  CIF
               CIF
            ELSE
               IF X.LT..52
               THEN
                  T=-.05*(1.-((2.*X-.52)/.52)**2)
               ELSE
                  IF X.LT.1.
                  THEN
                     T=.035*(1.-((2.*X-1.34)/.30)**2)
                  ELSE
                     T=-.13
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.16
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..42
               THEN
                  T=.075*(1.-((2.*X-.42)/.42)**2)
               ELSE
                  IF X.LT..67
                  THEN
                     T=-.06*(1.-((2.*X-1.09)/.25)**2)
                  ELSE
                     IF X.LT..9
                     THEN
                        T= .06*(1.-((2.*X-1.53)/.19)**2)
                     ELSE
                        T=-.080
                     CIF
                  CIF
               CIF
            ELSE
               IF X.LT..50
               THEN
                  T= .05*(1.-((2.*X-.50)/.50)**2)
               ELSE
                  IF X.LT..75
                  THEN
                     T=-.02*(1.-((2.*X-1.25)/.25)**2)
                  ELSE
                     IF X.LT.1.
                     THEN
                        T=.025*(1.-((2.*X-1.70)/.20)**2)
                     ELSE
                        T=-.03
                     CIF
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.17
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..40
               THEN
                  T=-.085*(1.-((2.*X-.40)/.40)**2)
               ELSE
                  IF X.LT..62
                  THEN
                     T= .05*(1.-((2.*X-1.02)/.22)**2)
                  ELSE
                     IF X.LT.1.
                     THEN
                        T=-.04*(1.-((2.*X-1.47)/.23)**2)
                     ELSE
                        T= .170
                     CIF
                  CIF
               CIF
            ELSE
               IF X.LT..37
               THEN
                  T=-.10*(1.-((2.*X-.37)/.37)**2)
               ELSE
                  IF X.LT..60
                  THEN
                     T= .06*(1.-((2.*X- .97)/.23)**2)
                  ELSE
                     IF X.LT..72
                     THEN
                        T=-.03*(1.-((2.*X-1.32)/.12)**2)
                     ELSE
                        IF X.LT..9
                        THEN
                           T= .03*(1.-((2.*X-1.58)/.14)**2)
                        ELSE
                           T=-.07
                        CIF
                     CIF
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.32
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..27
               THEN
                  T=.120*(1.-((2.*X-.27)/.27)**2)
               ELSE
                  IF X.LT..46
                  THEN
                     T=-.08*(1.-((2.*X- .73)/.19)**2)
                  ELSE
                     IF X.LT..64
                     THEN
                        T= .055*(1.-((2.*X-1.10)/.18)**2)
                     ELSE
                        T=0.
                     CIF
                  CIF
               CIF
            ELSE
               IF X.LT..43
               THEN
                  T= .05*(1.-((2.*X-.43)/.43)**2)
               ELSE
                  IF X.LT..67
                  THEN
                     T=-.025*(1.-((2.*X-1.10)/.24)**2)
                  ELSE
                     IF X.LT.1.
                     THEN
                        T=.020*(1.-((2.*X-1.55)/.21)**2)
                     ELSE
                        T=-.070
                     CIF
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.33
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..42
               THEN
                  T=-.09*(1.-((2.*X-.42)/.42)**2)
               ELSE
                  IF X.LT..68
                  THEN
                     T= .06*(1.-((2.*X-1.10)/.26)**2)
                  ELSE
                     IF X.LT..95
                     THEN
                        T=-.055*(1.-((2.*X-1.54)/.18)**2)
                     ELSE
                        T=.170
                     CIF
                  CIF
               CIF
            ELSE
               IF X.LT..44
               THEN
                  T=-.11*(1.-((2.*X-.44)/.44)**2)
               ELSE
                  IF X.LT..68
                  THEN
                     T= .075*(1.-((2.*X-1.12)/.24)**2)
                  ELSE
                     IF X.LT..9
                     THEN
                        T=-.05*(1.-((2.*X-1.53)/.17)**2)
                     ELSE
                        T= .080
                     CIF
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.48
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..34
               THEN
                  T= .08*(1.-((2.*X-.34)/.34)**2)
               ELSE
                  IF X.LT..85
                  THEN
                     T=-.035*(1.-((2.*X- .99)/.31)**2)
                  ELSE
                     T=.150
                  CIF
               CIF
            ELSE
               IF X.LT..30
               THEN
                  T=.035*(1.-((2.*X-.30)/.30)**2)
               ELSE
                  T=-.035*(1.-((2.*X-1.10)/.50)**2)
               CIF
            CIF
         CIF
         IF IWR.EQ.49
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..42
               THEN
                  T=-.08*(1.-((2.*X-.42)/.42)**2)
               ELSE
                  IF X.LT..70
                  THEN
                     T=.018*(1.-((2.*X-1.07)/.23)**2)
                  ELSE
                     T=.035
                  CIF
               CIF
            ELSE
               IF X.LT..50
               THEN
                  T=-.09*(1.-((2.*X-.50)/.50)**2)
               ELSE
                  IF X.LT..85
                  THEN
                     T= .080*(1.-((2.*X-1.30)/.30)**2)
                  ELSE
                     T=-.060
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.64
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..35
               THEN
                  T= .09*(1.-((2.*X-.35)/.35)**2)
               ELSE
                  IF X.LT..64
                  THEN
                     T=-.07*(1.-((2.*X- .99)/.29)**2)
                  ELSE
                     IF X.LT..85
                     THEN
                        T= .05*(1.-((2.*X-1.44)/.16)**2)
                     ELSE
                        T=-.09
                     CIF
                  CIF
               CIF
            ELSE
               IF X.LT..40
               THEN
                  T= .09*(1.-((2.*X-.40)/.40)**2)
               ELSE
                  T=0.
               CIF
            CIF
         CIF
         Y1COR=Y1COR+T
      CIF
      Y1=Y1+Y1COR
      CPROC
C
CCCCCCCCCCCCCCCC
C     FUNCTION SQTVAL(F,G,AL,EPS)
C
C     CALCULATE SQTVAL=F*(SQRT(1+G*AL**2/F**2)-1).
C     TO ACHIEVE GOOD PRECISION, FOR LARGE F THE TAYLOR EXPANSION
C     IS USED UPTO AT MOST 15 TERMS
C     EPS IS THE REQUIRED ABSOLUTE PRECISION
C
      PROC CALSQT
      S=G/F
      U=-S/F
      S=-.5*S
      IF ABS(U).GT..3
      THEN
         IF U.LT..98
         THEN
            SQTVAL=F*(SQRT(1.-U)-1.)
         ELSE
            SQTVAL=0.
C           PRINT 100,F,G,AL
C100        FORMAT(1X,' SQTVAL',3E16.7)
         CIF
      ELSE
         VAL=-S*(1.+.25*U+.125*U**2)
         QQ=S*U**3/12.8
         N=5
         WHILE ABS(QQ).GT.EPS .AND.N.LT.15
           VAL=VAL-QQ
           QQ=QQ*U*(1.-1.5/N)
           N=N+1
         CWHILE
         SQTVAL=VAL
      CIF
      CPROC
      END
      SUBROUTINE JFTNEW(IPTR,IPJHTL,WRK,LHIT,IPRES,INDEX,/XO/,/YO/,
     +CALCST,DSTORW)
C
C        J. SPITZER                           /10/86
C    UPDATED 4.4.87, FROM F11SPI.JADECAL.S
C    INCLUDE POSSIBILITY OF SUPPLYING PARTICLE MASS FOR
C    FLIGHT TIME CORRECTION                   1/6/87  J.S.
C
C    Z-CHAMBER COORDINATES ARE FETCHED IN CASE OF
C    ZS-FIT (INDEX=4)
C                                            15/7/87  J.S.
C
C        FETCH HITS FOR TRACK 'IPTR' IN PATR-BANK
C        CALCULATE COORDINATE INCLUDING ALL CORRECTIONS
C        STORE COORDINATES IN WRK(I1),I1=1,LHIT*NHIT
C
C        INDEX = 1 : COORDINATES IN REAL SPACE
C        INDEX = 2 : X-AXIS THROUGH 1. + LAST POINT
C        INDEX = 3 : X-AXIS THROUGH (XO,YO) + LAST POINT
C
C        INDEX = 4 : NEW FOR S-Z FITS    J. SPITZER 22/4/87
C                    COORDINATES IN REAL SPACE
C
      IMPLICIT INTEGER*2 (H)
C
      DIMENSION WRK(200)
      EQUIVALENCE (ZWZ,IZW)
C
#include "cdata.for"
#include "cjdrch.for"
C
      COMMON/CCMZCT/ DIMPCT, ZCUTV, ZCUTVV, IZVCST(5), ZCHWW
C ONLY ZCHWW WHICH IS THE WEIGHT FOR Z-CHAMBER HITS IS USED HERE
C  ARRAYS FOR Z-CHAMBER INFORMATION
      DIMENSION IZCHMB(3,2),AZCHMB(3,2)
C
N     * CALIBRATION CONSTANTS
      COMMON/JSCALD/ JESCAL,JESKEY,JESDRW
C
      COMMON /CFLMAS/ AFLMAS
      DIMENSION CALCST(96,27),DSTORW(5,64,24)
      REAL GG(4,8) / 1.16872E-1,-2.58070E-1, 1.32006E-1, 286.,
     +              -1.04290E-1, 8.84550E-2,-1.96380E-2, 286.,
     +               3.24418E-2,-7.46600E-2, 2.36765E-2, 496.,
     +              -5.75000E-2, 4.36000E-2,-1.10000E-2, 496.,
     +               3.12761E-2,-1.12856E-1, 6.26170E-2, 707.,
     +               5.44166E-1,-8.74361E-1, 3.05246E-1, 707.,
     +               3.54954E-1,-6.67922E-1, 2.74681E-1, 707.,
     +              -2.26062E-1, 3.02971E-1,-1.15400E-1, 707./
      REAL GGF(4,8)/         0.,         0.,         0., 286.,
     +                       0.,         0.,         0., 286.,
     +                       0.,         0.,         0., 496.,
     +                       0.,         0.,         0., 496.,
     +                       0.,         0.,         0., 707.,
     +               7.52764E-1,-1.10923E00, 3.47826E-1, 707.,
     +               2.52785E-1,-4.63690E-1, 1.75837E-1, 707.,
     +                       0.,         0.,         0., 707./
      REAL THL(4)/-52.,-52.,-4.,960./,
     +     THU(4)/-4.,-4.,960.,1570./,
     +     A2(4)/-52.,-1.97924E-5,-20.5739,0./,
     +     A3(4)/-9.224,700.,6.E-5,2.E-10/,
     +     A4(4)/1.497,1320.,-8.31E-6,-2.73E-10/
C
      REAL B1(4,4)/
     + 0.62000E+03,   0.87300E+03,  -0.41000E+03,   0.15800E+04,
     + 0.62000E+03,   0.87300E+03,  -0.41000E+03,   0.15800E+04,
     + 0.57030E+03,   0.89000E+03,  -0.38000E+03,   0.15800E+04,
     + 0.64300E+03,   0.90200E+03,  -0.41200E+03,   0.15800E+04/
C
      DIMENSION Q(5,4)
      DATA Q/14.5142,3.2743E-2,-6.E-6,0.,0.,
     +       4.46445E1,-8.87962E-2,1.29605E-4,9.02461E-9,-5.85976E-11,
     +       4.52471E1,-8.94577E-2,1.39668E-4,1.05065E-8,-7.46739E-11,
     +       18.256,3.46596E-2,-1.26438E-5,0.,0./
      DIMENSION P(5,4)
      DATA P/-.955408,1.62185E-3,-8.22074E-7,0.,0.,
     +       -.1736,1.41338E-3,-1.14314E-5,1.96957E-8,-7.93752E-12,
     +       -.173,2.2942E-4,-2.4298E-6,0.,0.,
     +       -1.0475,1.92375E-3,-1.2E-6,0.,0./
C
      REAL OMERIN(3)/2*.130900,.0654498/,ALORIN(3)/3*.34/,
     +     RR1(3)/211.,421.,632./,WIRDIS/10./,SMAXW(2,64),SM01(2,64),
     +     ANG375/.0654498/,
     +     FLTIM1/.028/,FLTIM2/.0363/,FLTIM3/1222.9/,ELFRCZ/.231/,
     +     AVFRMX/.78742/,EPS/1.E-4/,TANLOR/.365028/,SINLOR/.342898/,
     +     COSLOR/.939373/,PIVALU/3.141593/,
     +     PARVD(3)/.59562E-2,.59482E-2,.59421E-2/
C
      DATA JESOLD/-1/,LIMPRT/0/,KIMPRT/0/,LIMPR1/3/,KIMPR1/0/
C
      DIMENSION IRESAR(13),RESAR(13),HRESAR(13)
      EQUIVALENCE (IRESAR(1),RESAR(1),HRESAR(1))
C
C
N     MASK FOR L/R BIT IN HIT LABEL
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
C
      INDX=INDEX
      IF(INDX.EQ.4) INDX=1
      IF JESCAL.NE.JESOLD
      THEN
         JESOLD=JESCAL
         IF KIMPR1.LT.LIMPR1
         THEN
            KIMPR1=KIMPR1+1
            PRINT 720, JESCAL
720      FORMAT(' **** NEW ID CALIBRATION IN EFFECT IN JFETCH AFTER ',
     +   'RUN', I7,/,6X,'BIT 256 IS SET IN THE PROGRAM IDENTIFIER ',
     +   'WORD OF THE PATR BANK',////)
         CIF
      CIF
C
N     INITIALIZATION
      DATA LBINIT /0/,IQJETC/0/,IQHEAD/0/
      IF LBINIT .EQ. 0
      THEN
         LBINIT = 1
         IQJETC = IBLN('JETC')
         IQHEAD = IBLN('HEAD')
C
         A5=1./.6726
         FOR J=1,8
            GG(1,J)=GG(1,J)*A5
            GG(2,J)=GG(2,J)*A5*.5/GG(4,J)
            GG(3,J)=GG(3,J)*A5*.333333/GG(4,J)**2
            IF J.EQ.6.OR.J.EQ.7
            THEN
               GGF(1,J)=GGF(1,J)*A5
               GGF(2,J)=GGF(2,J)*A5*.5/GGF(4,J)
               GGF(3,J)=GGF(3,J)*A5*.333333/GGF(4,J)**2
            CIF
         CFOR
C
         FOR I=1,64
            IF I.LE.16
            THEN
               IRIN=1
               IW=I
            ELSE
               IF I.LE.32
               THEN
                  IRIN=2
                  IW=I-16
               ELSE
                  IRIN=3
                  IW=I-32
                  IF(IW.GT.16) IW=IW-16
               CIF
            CIF
            R=RR1(IRIN)+(IW-1)*WIRDIS
            SMAXW(1,I)=1.05*R*SIN(OMERIN(IRIN))/COS(ALORIN(IRIN)
     +      -OMERIN(IRIN))
            SMAXW(2,I)=1.05*R*SIN(OMERIN(IRIN))/COS(ALORIN(IRIN)
     +      +OMERIN(IRIN))
            SM01(1,I)=.7*SMAXW(1,I)
            SM01(2,I)=.7*SMAXW(2,I)
            IF IW.LT.3.OR.IW.GT.14
            THEN
               SM01(1,I)=.45*SMAXW(1,I)
               SM01(2,I)=.45*SMAXW(2,I)
            CIF
         CFOR
      CIF
C
      LHBIT  = LHIT*4
      IPCO = 1
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      IF NRUN.LT.24200
      THEN
         FREQR=1.0127
         FLTIM2=.0363
      ELSE
         FREQR=1.
         FLTIM2=0.
      CIF
      NEVT = HDATA(IPHEAD+11)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
N     SET FLAG FOR NEW CALIBRATION
      IDATA(IPTR+2) = LOR(IDATA(IPTR+2),256)
C
N     POINTER TO CALIBRATED JETC BANK
      IPJETC = IDATA(IQJETC)
      IP0    = 2*IPJETC + 100
C++++++++
N     LOCATE RAW JETC BANK
         IPRAW=IPJETC
         WHILE IDATA(IPRAW -1).GT.0
            IPRAW =IDATA(IPRAW -1)
         CWHILE
         IPRAW2=2*IPRAW -2*IPJETC
C========
C
N     ZVERT, THETA + DIR. COSINES
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTH = 1./SQRT(TGTH**2 + 1.)
      SNTH  = CSTH * TGTH
C
C
C++++++++
         XHCS = (ADATA(IPTR+12) + ADATA(IPTR+5)) * .5
         YHCS = (ADATA(IPTR+13) + ADATA(IPTR+6)) * .5
         XX    =  ADATA(IPTR+12) - ADATA(IPTR+5)
         YY    =  ADATA(IPTR+13) - ADATA(IPTR+6)
         RR    = SQRT(XX**2+YY**2)
         IF RR.LT.10.
         THEN
            IPRES=IPCO
            RETURN
         CIF
         CSROT = XX / RR
         SNROT = YY / RR
         ALCS=.5*RR
         SINFIC=SNROT
         COSFIC=CSROT
         IF(COSFIC.GT.1.) COSFIC=1.
         IF(COSFIC.LT.-1.) COSFIC=-1.
         FIC=ACOS(COSFIC)
         IF(SINFIC.LT.0.) FIC=2.*PIVALU-FIC
         BCS1X=-XHCS*COSFIC-YHCS*SINFIC
         BCS1Y= XHCS*SINFIC-YHCS*COSFIC
         CURVXY=ADATA(IPTR+25)
         IF(ABS(CURVXY).LT.1.E-8) CURVXY = SIGN(1.E-8,CURVXY)
C   FOLLOWING PARAMETERS ARE USED FOR CORRECTIONS ONLY
         IF(ABS(CURVXY*ALCS).GT..966) CURVXY=SIGN(.966/ALCS,CURVXY)
         CTGTH=TGTH
         IF(ABS(CTGTH).GT.2.) CTGTH=SIGN(2.,CTGTH)
C
         CPR0=CURVXY/SQRT(1.-(CURVXY*ALCS)**2)
         VCRS=1./CPR0
         CURN1=CURVXY*PARVD(2)
C========
C
N     ROTATION ANGLE (USING LAST POINT OF TRACK)
      SELECT INDX
      CASE 1
        XT = 0.
        YT = 0.
        CSROT = 1.
        SNROT = 0.
        XOT   = 0.
      CASE 2
        XT    = XHCS
        YT    = YHCS
        XOT   = 0.
      CASE 3
        XT    = (ADATA(IPTR+12) + XO) * .5
        YT    = (ADATA(IPTR+13) + YO) * .5
        XX    =  ADATA(IPTR+12) - XO
        YY    =  ADATA(IPTR+13) - YO
        RR    = SQRT(XX**2+YY**2)
        CSROT = XX / RR
        SNROT = YY / RR
C++
        XOT   = -.5*RR
      OTHER
N       ILLEGAL INDEX
        RETURN
      CSELECT
C
C
C
N     SELECT CELLS CONTAINING TRACK
C
      IPC0 = IPTR + 34
      IPC9 = IPC0 +  5
      FOR IPC = IPC0,IPC9
         JCELL = IDATA(IPC)
         IF JCELL.GT. 0 .AND. JCELL.LE.96
         THEN
            JRING = 1
            IF(JCELL.GT.24) JRING = 2
            IF(JCELL.GT.48) JRING = 3
            PERFORM FETCH
         CIF
      CFOR
C
C FETCH Z-CHAMBER HITS IN CASE Z-S FITS (INDEX=4)
      IF INDEX.EQ.4 .AND. ZCHWW.GT..1 .AND. ZCHWW.LT.2000.
      THEN
         CALL ZCFTNW(NRUN,NEVT,ITRK,TGTH,ZVERT,NZHIT,IZCHMB,AZCHMB)
         IF NZHIT.GT.0
         THEN
            FOR J=1,NZHIT
               HRESAR( 1) = 100+IZCHMB(1,J)
               HRESAR( 2) = IZCHMB(2,J)
               HRESAR( 3) = 0
               HRESAR( 4) = 0
               HRESAR( 5) = 1
C              HRESAR( 6) = IP-2*IPJETC
               HRESAR( 6) = 101
               XX=AZCHMB(1,J)
               YY=AZCHMB(2,J)
               ZZ=AZCHMB(3,J)
               RESAR ( 4) = XX
               RESAR ( 5) = YY
               RESAR ( 6) = ZZ
C CALCULATE TRACK LENGTH IN R-PHI FROM FIRST POINT ON TRACK
               UX=XX-ADATA(IPTR+5)
               UY=YY-ADATA(IPTR+6)
               UU=SQRT(UX**2+UY**2)
            IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
               IF(UX*ADATA(IPTR+8)+UY*ADATA(IPTR+9).LT.0.) UU=-UU
               RESAR ( 7) = UU
C              RESAR ( 8) = WW
               RESAR ( 8) = ZCHWW
               IF(NRUN.LT.24200) RESAR(8)=RESAR(8)*.6
               CALL MVC(WRK(IPCO),0,RESAR(1),0,LHBIT)
               IPCO = IPCO + LHIT
            CFOR
         CIF
      CIF
C
N     STORE RESULTS
      IPRES = IPCO
      IF INDEX.LT.4
      THEN
         WRK (IPRES   ) = XT
         WRK (IPRES+ 1) = YT
         WRK (IPRES+ 2) = CSROT
         WRK (IPRES+ 3) = SNROT
         WRK (IPRES+ 9) = XOT
         WRK (IPRES+10) = 0.
         WRK (IPRES+11) = CSTH
         WRK (IPRES+12) = SNTH
      CIF
C
      RETURN
C
N     *************************
N     *      F E T C H        *
N     *************************
C
C
N     FETCH HITS IN CELL
      PROC FETCH
C
C+++++++
         KRING=JRING
         IF(JRING.EQ.3 .AND. JCELL-(JCELL/2)*2.NE.1) KRING=4
         IF JCELL.LE.24
         THEN
            ISEG=JCELL
         ELSE
            IF JCELL.LE.48
            THEN
               ISEG=JCELL-24
            ELSE
               ISEG=(JCELL-47)/2
            CIF
         CIF
         FISEGM=((ISEG-1)*4+2)*ANG375
N    MIDDLE OF CELL WITHIN SEGMENT
         ACEL1=CALCST(JCELL,18)
         ZETCEL=2.*SIN(.5*ACEL1*CURVXY)/CURVXY*TGTH+ZVERT
         IF(ABS(ZETCEL).GT.1000.) ZETCEL=SIGN(1000.,ZETCEL)
         ACEL1=   ACEL1        +ZETCEL*CALCST(JCELL,19)
         BCEL1=CALCST(JCELL,20)+ZETCEL*CALCST(JCELL,21)
         OCEL1=CALCST(JCELL,22)+ZETCEL*CALCST(JCELL,23)
         FIIC=FISEGM+OCEL1-FIC
         ACEL2=CALCST(JCELL,19)*CTGTH*WIRDIS
         BCEL2=CALCST(JCELL,21)*CTGTH*WIRDIS
         OCEL2=CALCST(JCELL,23)*CTGTH*WIRDIS
         ROT1X=COS(FIIC)
         ROT1Y=SIN(FIIC)
         ROT2X=SIN(FIC-FISEGM)
         ROT2Y=COS(FIC-FISEGM)
         BCS1XC=BCS1X+ACEL1*ROT2Y+BCEL1*ROT2X
         BCS1YC=BCS1Y-ACEL1*ROT2X+BCEL1*ROT2Y
C=========
C
N       COUNTER FOR NUMBER OF HITS FOUND
        JHIT = 0
        NHIT   = 0
        NHGOOD = 0
N       PRESET LAST LAYER
        ILAYL =-99
N       LOOP OVER ALL HITS OF CELL
        IPCO = IPCO - LHIT
        IPCLL  = 2*IPJETC + 2 + JCELL
        IP     = HDATA(IPCLL  ) + IP0
        IP9    = HDATA(IPCLL+1) + IP0
        IPHL   = IPJHTL + 2 + HDATA(IPCLL)/4
        WHILE IP.LT.IP9
C
N         CHECK TRACK # OF HIT LABEL
          LB   = IDATA(IPHL)
          ITR1 = LAND(SHFTR(LB,17),127)
          ITR2 = LAND(SHFTR(LB, 1),127)
          IF ITR1.EQ.ITRK .OR. ITR2.EQ.ITRK
          THEN
C
N           SET LBGOOD = 2 IF HIT ASSOCIATED WITH 2 TRACKS
            L0GOOD = 0
            IF(ITR1.NE.0 .AND. ITR2.NE.0) L0GOOD = 2
C
N           L/R FROM HIT LABEL
            LBLR = 0
            IF(ITR1.EQ.ITRK) LBLR = LAND(LB,MKLRT1)
            IF(ITR2.EQ.ITRK) LBLR = LAND(LB,MKLRT2)
            LBSIDE =-1
            IF(LBLR.NE.0) LBSIDE = 1
            LBLR = LBSIDE
C
            IWIR = HDATA(IP)
            IWIR = SHFTR(IWIR,3)
N           LAYER NUMBER WITHIN RING 3
            ILAY = LAND(IWIR,15)
N           AMPLITUDES
            IAMPL = HDATA(IP+1)
            IAMPR = HDATA(IP+2)
N           CALCULATE Z COORDINATE
C           IF IAMPR.LE.0.OR.IAMPL.LE.0
C           THEN
C             ZZ     = 0.
C             LZGOOD = 16
C           ELSE
C             ZZ = IAMPR + IAMPL
C             ZZ = FLOAT(IAMPR-IAMPL) * ZAL*.5 / ZZ
C             LZGOOD = 0
C             IF(ABS(ZZ).GT.1250.) LZGOOD = 16
C           CIF
            CALL AMPS2Z( IP,IPJETC,ZZ,WW,LZGOOD)
C
C+++++++
N     WIRE NUMBER WITHIN CELL 1..16
            IW=ILAY+1
            IODD=1
            IF(IW-(IW/2)*2.EQ.0) IODD=-1
            RHIT=ACEL1+(IW-8.5)*WIRDIS
            FLPATH=2.*SIN(.5*RHIT*CURVXY)/CURVXY
            ZHIT=FLPATH*TGTH+ZVERT
            IF(ABS(ZHIT).GT.1200.) ZHIT=SIGN(1200.,ZHIT)
            FLPATH=SQRT(FLPATH**2+ZHIT**2)
C
N     DRIFT TIME (FROM RAW BANK) + CORRECTIONS
            TDRIFT=HDATA(IP+3+IPRAW2)
            IF NRUN.LT.24200
            THEN
               TDRIFT=TDRIFT*64.+32.
               IF(NRUN.GE.19050.AND.NRUN.LE.20274) TDRIFT=TDRIFT+20.
               IF(NRUN.GE. 3300.AND.NRUN.LE. 3550) TDRIFT=TDRIFT-90.
            ELSE
               IF NRUN.LE.24698
               THEN
                  TDRIFT=TDRIFT-5.
                  IF(NRUN.LT.24405) TDRIFT=TDRIFT+153.
                  IF(NRUN.GE.24227.AND.NRUN.LE.24232) TDRIFT=TDRIFT+147.
                  IF(NRUN.GE.24233.AND.NRUN.LE.24245) TDRIFT=TDRIFT+297.
               CIF
            CIF
            AMRAWL=HDATA(IP+1+IPRAW2)*8.
            AMRAWR=HDATA(IP+2+IPRAW2)*8.
N     SLEWING CORRECTION
            PERFORM SLWCOR
N     CLOCK FREQUENCY
            IF(JRING.EQ.3) TDRIFT=TDRIFT*FREQR
N     FLIGHT AND PROPAGATION TIME
            BKGS=ABS(HDATA(IPHEAD+30)*.001)
            IF(BKGS.LT.3.) BKGS=4.8
            AMOMGV=.02998E-3*BKGS*SQRT(1.+CTGTH**2)/CURVXY
            CPERV=SQRT(1.+(AFLMAS/AMOMGV)**2)
            TDRIFT=TDRIFT-FLTIM1*FLPATH*CPERV-FLTIM2*(FLTIM3-ABS(ZHIT))
N     T0
            TDRIFT=TDRIFT-CALCST(JCELL,IW)
N     * CALCULATE "WIRE NUMBER CORRECTION"
            TSTG=CALCST(JCELL,17)*(1.-ELFRCZ*(ZHIT/1200.)**2)
     +      *AVFRMX*SINLOR*PARVD(JRING)/WIRDIS
            XK=IW-IODD*TSTG-8.5
            ROT3X=XK*WIRDIS*(ROT1X-ROT1Y*XK*OCEL2)
            ROT3Y=XK*WIRDIS*(ROT1Y+ROT1X*XK*OCEL2)
N     * WIRE IN THE "CIRCLE" SYSTEM
            XWPR=ROT3X+BCS1XC+XK*( ACEL2*ROT2Y+BCEL2*ROT2X)
            YWPR=ROT3Y+BCS1YC+XK*(-ACEL2*ROT2X+BCEL2*ROT2Y)
C========
N           CHECK IF LEFT + RIGHT SOLUTION POSSIBLE
            NLRSOL = 1
C++
            IF(TDRIFT.LT.300.) NLRSOL = 2
C
N           LOOP OVER LEFT +/OR RIGHT SOLUTION
            ILRSOL = 0
            REPEAT
            ILRSOL = ILRSOL + 1
            LBGOOD = L0GOOD
C
N             SELECT SIDE
              IF NLRSOL.EQ.1 .AND. LBSIDE.LT.0  .OR.
     ?           NLRSOL.EQ.2 .AND. ILRSOL.EQ.1
              THEN
N               LEFT SIDE
                LBSIDE =-1
C++
                PERFORM GETCOR
              ELSE
N               RIGHT SIDE
                LBSIDE = 1
C++
                PERFORM GETCOR
              CIF
C
N             HIT QUALITY:
              IF(LBSIDE.NE.LBLR) LBGOOD = LBGOOD + 1
N             NEW LAYER?
              IF ILAY.NE.ILAYL .OR. LBGDL.LE.1.AND.LBGOOD.LE.2
              THEN
                LBREG = 1
N               INCREASE HIT COUNTER
                JHIT = JHIT + 1
                IPCO = IPCO + LHIT
              ELSE
N               2 HITS IN SAME LAYER, SELECT CLOSEST
                LBREG = 0
                ZWZ = WRK(IPCO+10)
                IF(LBGOOD.LT.IZW) LBREG = 1
              CIF
N             REGISTER NEW HIT?
              IF LBREG.NE.0
              THEN
                NHIT   = NHIT   + 1
                IF(LBGOOD.LE.2) NHGOOD = NHGOOD + 1
                IF INDEX.NE.4
                THEN
                   IRESAR( 1) = ILAY
                   IRESAR( 2) = IP
                   IRESAR( 3) = LBSIDE
                   RESAR ( 4) = XX
                   RESAR ( 5) = YY
                   RESAR ( 6) = ZZ
                   RESAR ( 7) = XX - XOT
                   IF(INDX.EQ.1) RESAR ( 7) = SQRT(XX**2 + YY**2)
                   IRESAR( 8) = LZGOOD
                   RESAR ( 9) = DSC
                   IRESAR(10) = JCELL
                   IRESAR(11) = LBGOOD
                   RESAR (12) = TANBET
                   IRESAR(13) = JRING
                   RESAR (14) = 0.
                ELSE
                   HRESAR( 1) = JCELL
                   HRESAR( 2) = ILAY
                   HRESAR( 3) = LZGOOD
                   HRESAR( 4) = LBGOOD
                   HRESAR( 5) = 1
                   HRESAR( 6) = IP-2*IPJETC
                   RESAR ( 4) = XX
                   RESAR ( 5) = YY
                   RESAR ( 6) = ZZ
C CALCULATE TRACK LENGTH IN R-PHI FROM FIRST POINT ON TRACK
                   UX=XX-ADATA(IPTR+5)
                   UY=YY-ADATA(IPTR+6)
                   UU=SQRT(UX**2+UY**2)
            IF(ABS(CURVXY*UU).GT.1.E-5) UU=2.*SIN(.5*CURVXY*UU)/CURVXY
                   IF(UX*ADATA(IPTR+8)+UY*ADATA(IPTR+9).LT.0.) UU=-UU
                   RESAR ( 7) = UU
                   RESAR ( 8) = WW
                CIF
                CALL MVC(WRK(IPCO),0,RESAR(1),0,LHBIT)
                ILAYL = ILAY
                LBGDL = LBGOOD
              CIF
C
            UNTIL ILRSOL.GE.NLRSOL
C
          CIF
C
        IPHL = IPHL + 1
        IP   = IP   + 4
        CWHILE
N       SET IPCO TO 1. FREE LOCATION
        IPCO = IPCO + LHIT
C
      CPROC
C
N     *************************
N     *      G E T C O R      *
N     *************************
C
      PROC GETCOR
         IF LBSIDE.LT.0
         THEN
            AG2= CALCST(JCELL,26)
            VDP=-CALCST(JCELL,24)
         ELSE
            AG2= CALCST(JCELL,27)
            VDP= CALCST(JCELL,25)
         CIF
         AG2=AG2-FIIC-XK*OCEL2
         SINAG2=SIN(AG2)
         COSAG2=COS(AG2)
C CALCULATE DELTA=SIGNED CHANGE OF DRIFT TIME PRO WIRE SPACING
         F=XWPR*SINAG2+(YWPR+VCRS)*COSAG2
         G=(ALCS-XWPR)*(ALCS+XWPR)-YWPR*(YWPR+2.*VCRS)
         IF G.GT.-.98*F**2
         THEN
N     * WIRE CIRCLE DISTANCE ALONG DRIFT DIR.
            PERFORM CALSQT
            DISTWC=SQTVAL
            XPR=XWPR+DISTWC*SINAG2
            DYPDXP=1.-(CURVXY*XPR)**2
            IF DYPDXP.LT..02
            THEN
               IF KIMPRT.LT.LIMPRT
               THEN
                  KIMPRT=KIMPRT+1
                  PRINT 675,CURVXY,ALCS,XPR,NRUN,NEVT,ITRK
675      FORMAT(' *** ERROR IN JFTNEW *** CURVATURE, HALF TRACK LENGTH',
     +   ' X IN TR C.S.',/,8X,3E15.5,'   TRACK',I9,I6,I4)
C
                  PRINT 676, CPR0,VCRS,XHCS,YHCS,FIC,TGTH,
     +            XWPR,YWPR,AG2,F,G,DISTWC,DYPDXP,
     +            KRING,JCELL,ISEG,FISEGM,IW,IODD,ILRSOL,LBSIDE
676               FORMAT(/,' CPR0,VCRS,XHCS,YHCS,FIC,TGTH,',/,
     +            ' XWPR,YWPR,AG2,F,G,DISTWC,DYPDXP,'/,
     +            ' KRING,JCELL,ISEG,FISEGM,IW,IODD,ILRSOL,LBSIDE',/,
     +            1X,6E15.5,/,1X,7E15.5,/,1X,3I4,E15.5,4I6,////)
               CIF
               DYPDXP=.02
            CIF
            DYPDXP=-XPR*CURVXY/SQRT(DYPDXP)
            C=1.-DYPDXP*SINAG2/COSAG2
            IF(ABS(C).LT..001) C=SIGN(.001,C)
            TANBET=(DYPDXP+SINAG2/COSAG2)/C
            AMU=WIRDIS/PARVD(JRING)*COSLOR
            DELTA=AMU*(TANBET-TANLOR)
            IF(ABS(DELTA).GT.1800.) DELTA=SIGN(1800.,DELTA)
         ELSE
C           HIT CAN NOT BE ON THE TRACK. FOR ANGLE DEPENDENT
C           CORRECTIONS A TRACK PARALELL WITH THE WIRE PLANE
C           WILL BE ASSUMED
            IF KIMPRT.LT.LIMPRT
            THEN
               KIMPRT=KIMPRT+1
               PRINT 674,CURVXY,ALCS,NRUN,NEVT,ITRK
674      FORMAT(' *** ERROR IN JFTNEW *** CURVATURE, HALF TRACK LENGTH',
     +   /,8X,2E15.5,'   TRACK',I9,I6,I4)
               PRINT 677, CPR0,VCRS,XHCS,YHCS,FIC,TGTH,
     +         XWPR,YWPR,AG2,F,G,
     +         KRING,JCELL,ISEG,FISEGM,IW,IODD,ILRSOL,LBSIDE
677            FORMAT(/,' CPR0,VCRS,XHCS,YHCS,FIC,TGTH,',/,
     +         ' XWPR,YWPR,AG2,F,G,'/,
     +         ' KRING,JCELL,ISEG,FISEGM,IW,IODD,ILRSOL,LBSIDE',/,
     +         1X,6E15.5,/,1X,5E15.5,/,1X,3I4,E15.5,4I6,////)
            CIF
            TANBET=TANLOR
            DELTA=0.
         CIF
C
N     *  Z AND THETA DEPENDENT SLEWING
         PERFORM SLWZTH
N     * CLOSE WIRE CORRECTION
         PERFORM CLWCOR
N     * CORRECT FOR STAGGERING AND TRACK ANGLE FI
         PERFORM STGANG
C
N     * DISTANCE FROM WIRE
         Y1=VDP*TCORR
N     * DISTORTIONS
         IF(JESDRW.GT.0) PERFORM DSTRTN
         DSC=ABS(Y1)
         XX=XWPR+Y1*SINAG2
         YY=YWPR+Y1*COSAG2
         IF INDX.NE.2
         THEN
            A= XX*COSFIC-YY*SINFIC+XHCS
            YY=XX*SINFIC+YY*COSFIC+YHCS
            XX=A
            IF INDX.EQ.3
            THEN
               A = (XX-XT)*CSROT+(YY-YT)*SNROT
               YY=-(XX-XT)*SNROT+(YY-YT)*CSROT
               XX=A
            CIF
         CIF
      CPROC
C
N     *************************
N     *      S L W C O R      *
N     *************************
C
      PROC SLWCOR
      A=AMRAWL
      IF(AMRAWR.GT.A) A=AMRAWR
      IF(A.LT.10.) A=10.
N    * SLEWING FOR RAW AMPLITUDES
      IF NRUN.GE. 24200
      THEN
         IF A.GT.1800.
         THEN
            TSLEW=-1.449+1.19097E-3*(A-2000.)
         ELSE
            TSLEW=-2.100+2.11521E-3*(A-1600.)-8.50349E-12*(1600.-A)**4
         CIF
      ELSE
         IF A.GT.5000.
         THEN
            TSLEW=-50.+5.80000E-3*(A-5000.)
         ELSE
            IF A.LT.300.
            THEN
               TSLEW=-200.
            ELSE
         TSLEW=-4472.05*A**(-5.23557E-1-6.42692E-3*(ALOG(A)-7.77529)**2)
               IF(NRUN.GE.20275.AND.A.LT.1500.)
     +         TSLEW=TSLEW-(A-1500.)**2*2.26664E-5
            CIF
            IF(A.LT.650. .AND.(NRUN.GE.20275 .OR.
     +      NRUN.GE.13000 .AND. NRUN.LE. 14599) )
     +      TSLEW=TSLEW+116.6-1.79687E-1*A
            IF(A.LT.800. .AND. NRUN.GE.11473 .AND. NRUN.LE.12554)
     +      TSLEW=TSLEW+139.4-1.74800E-1*A
         CIF
      CIF
      TDRIFT=TDRIFT+TSLEW
      CPROC
C
N     *************************
N     *      S L W Z T H      *
N     *************************
C
      PROC SLWZTH
      ACTG=ABS(CTGTH)
      IF NRUN.GE. 24200
      THEN
         ZTSLW=0.
         IF KRING.EQ.4
         THEN
            IF LBSIDE.LT.0
            THEN
               ZTSLW=-19.43-14.5942*ACTG+19.8951*ACTG**2
               IF ACTG.LT..42
               THEN
                  ZTSLW=ZTSLW+5.1921+3.216*ACTG-82.49*ACTG**2
               ELSE
                  ZTSLW=ZTSLW-24.66+48.9578*ACTG-22.7265*ACTG**2
               CIF
            ELSE
               ZTSLW=5.918-5.45559*ACTG-2.12*ACTG**2
            CIF
         CIF
         IF KRING.EQ.3
         THEN
            IF LBSIDE.LT.0
            THEN
               ZTSLW=-.937-8.66313*ACTG+9.8988*ACTG**2
            ELSE
               ZTSLW= 2.46- 3.8375*ACTG-14.5671*ACTG**2
            CIF
         CIF
         J=2*KRING
         IF(LBSIDE.LT.0) J=J-1
         IF J.EQ.6.OR.J.EQ.7
         THEN
            AZ=ABS(ZHIT)
            BZ=ACTG*GGF(4,J)
            ZTSLW=ZTSLW+(AZ-BZ)*(GGF(1,J)+GGF(2,J)*(AZ+BZ)+GGF(3,J)
     +      *(AZ*(AZ+BZ)+BZ**2))
         CIF
      ELSE
         IF KRING.EQ.1
         THEN
            ZTSLW=13.46-14.03*ACTG
         ELSE
            IF KRING.EQ.2
            THEN
               ZTSLW=15.23-31.278*ACTG+7.54731*ACTG**2
            ELSE
               ZTSLW=20.86-48.672*ACTG+13.663*ACTG**2
            CIF
         CIF
C
         IF KRING.EQ.1
         THEN
            IF LBSIDE.LT.0
            THEN
               IF ACTG.LT..37
               THEN
                  T1=10.30-26.1*ACTG
               ELSE
                  T1=3.88-18.5345*ABS(ACTG-.5427)
               CIF
               T1=T1-0.30+ 0.77*ACTG-0.7648*ACTG**2
            ELSE
               IF ACTG.LT..37
               THEN
                  T1=7.50-26.3*ACTG
               ELSE
                  T1=-1.95+6.84118*ABS(ACTG-.4)
               CIF
               T1=T1+3.21-14.10*ACTG+10.73*ACTG**2
            CIF
            ZTSLW=ZTSLW+T1
         CIF
         IF KRING.EQ.2
         THEN
            IF LBSIDE.LT.0
            THEN
               IF ACTG.LT..40
               THEN
                  T1=8.787-19.675*ACTG
               ELSE
                  T1=4.091-18.133*ABS(ACTG-.56)
               CIF
            ELSE
               IF ACTG.LT..48
               THEN
                  T1=1.983-12.667*ACTG
               ELSE
                  T1=-4.1125+13.574*ABS(ACTG-.5)
               CIF
            CIF
            ZTSLW=ZTSLW+T1
         CIF
         IF KRING.EQ.3
         THEN
            T1=8.336-12.3519*ACTG
            IF LBSIDE.LT.0
            THEN
               T1=T1-2.68+18.09*ACTG-15.95*ACTG**2
            ELSE
               T1=T1+1.20-14.489*ACTG+14.623*ACTG**2
               IF(NRUN.GE.8712.AND.NRUN.LE.9999)
     +         T1=T1+14.20-8.5415*ACTG-10.6000*ACTG**2
               IF(NRUN.GE.7592.AND.NRUN.LE.8711)
     +         T1=T1+4.16+17.97*ACTG-24.33*ACTG**2
            CIF
            ZTSLW=ZTSLW+T1
         CIF
         IF KRING.EQ.4
         THEN
            IF ACTG.LT..56
            THEN
               T1=5.58-15.183*ACTG
            ELSE
               T1=-3.30+8.1613*(ACTG-.5)
            CIF
            IF LBSIDE.LT.0
            THEN
               T1=T1+3.12-21.16*ACTG+18.90*ACTG**2
               IF(NRUN.GE.11038.AND.NRUN.LE.12554)
     +         T1=T1+.39+29.1785*ACTG-30.4402*ACTG**2
               IF(NRUN.GE.8712.AND.NRUN.LE.9999)
     +         T1=T1+7.30+13.8*ACTG-23.20*ACTG**2
               IF(NRUN.GE.7592.AND.NRUN.LE.8711)
     +         T1=T1+0.16+ 9.08*ACTG- 9.60*ACTG**2
               IF(NRUN.GE.6185.AND.NRUN.LE.7591)
     +         T1=T1-16.6+41.18*ACTG-20.60*ACTG**2
            ELSE
               T1=T1-0.62+12.25*ACTG-10.52*ACTG**2
            CIF
            ZTSLW=ZTSLW+T1
         CIF
C
         J=2*KRING
         IF(LBSIDE.LT.0) J=J-1
         AZ=ABS(ZHIT)
         BZ=ACTG*GG(4,J)
         ZTSLW=ZTSLW+(AZ-BZ)*(GG(1,J)+GG(2,J)*(AZ+BZ)+GG(3,J)
     +   *(AZ*(AZ+BZ)+BZ**2))
         IF KRING.GE.3
         THEN
            ZTSLW=ZTSLW+1.70E-2/.6727*(ZHIT-CTGTH*GG(4,J))
         CIF
C
         IF(KRING.GE.3) ZTSLW=ZTSLW*FREQR
      CIF
      TCORR=TDRIFT+ZTSLW
      CPROC
C
N     *************************
N     *      C L W C O R      *
N     *************************
C
      PROC CLWCOR
C     APPLY CLOSE WIRE CORRECTION
      IF TCORR.LT.THU(4)
      THEN
         TCOR=TCORR
         IF TCOR.GT.THL(4)
         THEN
            TCOR=TCOR+A4(1)+A4(3)*(TCOR-A4(2))**2+A4(4)*(TCOR-A4(2))**4
         ELSE
            IF TCOR.GT.THL(3)
            THEN
               TCOR=TCOR+A3(1)+A3(3)*(TCOR-A3(2))**2
     +         +A3(4)*(TCOR-A3(2))**4
            ELSE
               IF TCOR.GT.THL(2)
               THEN
                  TCOR=TCOR-A2(1)+A2(2)*((TCOR-A2(3))**4-
     +            (A2(1)-A2(3))**4)
               ELSE
                  TCOR=TCOR-A2(1)
               CIF
            CIF
         CIF
         IF(TCOR.GT.0..AND.TCOR.LT.120.) TCOR=TCOR-8.E-2*(TCOR-120.)
         TCORR=TCOR
      CIF
      CPROC
C
N     *************************
N     *      S T G A N G      *
N     *************************
C
      PROC STGANG
         PERFORM STGFIZ
         STGTC=CALCST(JCELL,17)*IODD*LBSIDE*STGCOR
C
         IF DELTA.GT.B1(3,KRING)
         THEN
            A12=B1(2,KRING)
         ELSE
            A12=B1(1,KRING)
         CIF
         CSGINV=SQRT(1.+((DELTA-B1(3,KRING))/B1(4,KRING))**2)
         IF TCORR.GT.A12
         THEN
C           Y=A12*CURN1*LBSIDE
C           IF ABS(Y).GT.1.E-5
C           THEN
C!!            TANGCC=A12*(-SQTVAL(1./(Y*CSGINV),1.-2./Y,1.,1.E-4)-1.)
C           ELSE
               TANGCC=A12*(CSGINV-1.)
C           CIF
         ELSE
C           Y=TCORR*CURN1*LBSIDE
C           IF ABS(Y).GT.1.E-5
C           THEN
C !!           TANGCC=TCORR*(SQTVAL(1./(Y*CSGINV),1.-2./Y,1.,1.E-4)+1.)
C           ELSE
               TANGCC=TCORR*(CSGINV-1.)
C           CIF
         CIF
         TCORR=TCORR+TANGCC+STGTC
      CPROC
C
N     *************************
N     *      S T G F I Z      *
N     *************************
C
      PROC STGFIZ
C
      D=DELTA
      Z=ZHIT
      PERFORM STGZ0
      PERFORM FRACT
      U=(Z/1200.)**2
      STGCOR=STGDZ0*(1.+U*FRACZD)
      IF(STGCOR.LT..2) STGCOR=.2
      CPROC
C-----------------------------------------------------------------------
      PROC STGZ0
N     * CALCULATE D DEPENDENT STAG FRAC AT Z=0
         IF D.LT.-600.
         THEN
            I=1
         ELSE
            IF D.LT.0.
            THEN
               I=2
            ELSE
               IF D.LT.600.
               THEN
                  I=3
               ELSE
                  I=4
               CIF
            CIF
         CIF
         STGDZ0=Q(1,I)+ABS(D)*(Q(2,I)+Q(4,I)*D**2)+Q(3,I)*D**2
     +   +Q(5,I)*D**4
         STGDZ0=STGDZ0/44.9444
      CPROC
C-----------------------------------------------------------------------
      PROC FRACT
N     * CALCULATE D DEPENDENT EL.STAT.FRACTION
         IF D.LT.-500.
         THEN
            I=1
         ELSE
            IF D.LT.0.
            THEN
               I=2
            ELSE
               IF D.LT.400.
               THEN
                  I=3
               ELSE
                  I=4
               CIF
            CIF
         CIF
         FRACZD=P(1,I)+ABS(D)*(P(2,I)+P(4,I)*D**2)+P(3,I)*D**2
     +   +P(5,I)*D**4
      CPROC
C
N     *************************
N     *      D S T R T N      *
N     *************************
C
      PROC DSTRTN
C
      IWR=(KRING-1)*16+IW
      IF Y1.GT.0.
      THEN
         IND=2
      ELSE
         IND=1
      CIF
      SMAX=SMAXW(IND,IWR)
      SM0=SM01(IND,IWR)
      Y1COR=DSTORW(2*IND,IWR,ISEG)*Y1**2    +   DSTORW(5,IWR,ISEG)
      S0=ABS(Y1)-SM0
      IF(S0.GT.0.) Y1COR=Y1COR+DSTORW(2*IND-1,IWR,ISEG)*S0**2
      IF IW.EQ.1 .OR. IW.EQ.16
      THEN
         X=ABS(Y1/SMAX)
         IF IWR.EQ.1
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..52
               THEN
                  T=-.07*(1.-((2.*X-.52)/.52)**2)
               ELSE
                  IF X.LT.1.
                  THEN
                     T=.05*(1.-((2.*X-1.37)/.33)**2)
                  ELSE
                     T=-.13
                  CIF
               CIF
            ELSE
               IF X.LT..52
               THEN
                  T=-.05*(1.-((2.*X-.52)/.52)**2)
               ELSE
                  IF X.LT.1.
                  THEN
                     T=.035*(1.-((2.*X-1.34)/.30)**2)
                  ELSE
                     T=-.13
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.16
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..42
               THEN
                  T=.075*(1.-((2.*X-.42)/.42)**2)
               ELSE
                  IF X.LT..67
                  THEN
                     T=-.06*(1.-((2.*X-1.09)/.25)**2)
                  ELSE
                     IF X.LT..9
                     THEN
                        T= .06*(1.-((2.*X-1.53)/.19)**2)
                     ELSE
                        T=-.080
                     CIF
                  CIF
               CIF
            ELSE
               IF X.LT..50
               THEN
                  T= .05*(1.-((2.*X-.50)/.50)**2)
               ELSE
                  IF X.LT..75
                  THEN
                     T=-.02*(1.-((2.*X-1.25)/.25)**2)
                  ELSE
                     IF X.LT.1.
                     THEN
                        T=.025*(1.-((2.*X-1.70)/.20)**2)
                     ELSE
                        T=-.03
                     CIF
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.17
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..40
               THEN
                  T=-.085*(1.-((2.*X-.40)/.40)**2)
               ELSE
                  IF X.LT..62
                  THEN
                     T= .05*(1.-((2.*X-1.02)/.22)**2)
                  ELSE
                     IF X.LT.1.
                     THEN
                        T=-.04*(1.-((2.*X-1.47)/.23)**2)
                     ELSE
                        T= .170
                     CIF
                  CIF
               CIF
            ELSE
               IF X.LT..37
               THEN
                  T=-.10*(1.-((2.*X-.37)/.37)**2)
               ELSE
                  IF X.LT..60
                  THEN
                     T= .06*(1.-((2.*X- .97)/.23)**2)
                  ELSE
                     IF X.LT..72
                     THEN
                        T=-.03*(1.-((2.*X-1.32)/.12)**2)
                     ELSE
                        IF X.LT..9
                        THEN
                           T= .03*(1.-((2.*X-1.58)/.14)**2)
                        ELSE
                           T=-.07
                        CIF
                     CIF
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.32
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..27
               THEN
                  T=.120*(1.-((2.*X-.27)/.27)**2)
               ELSE
                  IF X.LT..46
                  THEN
                     T=-.08*(1.-((2.*X- .73)/.19)**2)
                  ELSE
                     IF X.LT..64
                     THEN
                        T= .055*(1.-((2.*X-1.10)/.18)**2)
                     ELSE
                        T=0.
                     CIF
                  CIF
               CIF
            ELSE
               IF X.LT..43
               THEN
                  T= .05*(1.-((2.*X-.43)/.43)**2)
               ELSE
                  IF X.LT..67
                  THEN
                     T=-.025*(1.-((2.*X-1.10)/.24)**2)
                  ELSE
                     IF X.LT.1.
                     THEN
                        T=.020*(1.-((2.*X-1.55)/.21)**2)
                     ELSE
                        T=-.070
                     CIF
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.33
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..42
               THEN
                  T=-.09*(1.-((2.*X-.42)/.42)**2)
               ELSE
                  IF X.LT..68
                  THEN
                     T= .06*(1.-((2.*X-1.10)/.26)**2)
                  ELSE
                     IF X.LT..95
                     THEN
                        T=-.055*(1.-((2.*X-1.54)/.18)**2)
                     ELSE
                        T=.170
                     CIF
                  CIF
               CIF
            ELSE
               IF X.LT..44
               THEN
                  T=-.11*(1.-((2.*X-.44)/.44)**2)
               ELSE
                  IF X.LT..68
                  THEN
                     T= .075*(1.-((2.*X-1.12)/.24)**2)
                  ELSE
                     IF X.LT..9
                     THEN
                        T=-.05*(1.-((2.*X-1.53)/.17)**2)
                     ELSE
                        T= .080
                     CIF
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.48
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..34
               THEN
                  T= .08*(1.-((2.*X-.34)/.34)**2)
               ELSE
                  IF X.LT..85
                  THEN
                     T=-.035*(1.-((2.*X- .99)/.31)**2)
                  ELSE
                     T=.150
                  CIF
               CIF
            ELSE
               IF X.LT..30
               THEN
                  T=.035*(1.-((2.*X-.30)/.30)**2)
               ELSE
                  T=-.035*(1.-((2.*X-1.10)/.50)**2)
               CIF
            CIF
         CIF
         IF IWR.EQ.49
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..42
               THEN
                  T=-.08*(1.-((2.*X-.42)/.42)**2)
               ELSE
                  IF X.LT..70
                  THEN
                     T=.018*(1.-((2.*X-1.07)/.23)**2)
                  ELSE
                     T=.035
                  CIF
               CIF
            ELSE
               IF X.LT..50
               THEN
                  T=-.09*(1.-((2.*X-.50)/.50)**2)
               ELSE
                  IF X.LT..85
                  THEN
                     T= .080*(1.-((2.*X-1.30)/.30)**2)
                  ELSE
                     T=-.060
                  CIF
               CIF
            CIF
         CIF
         IF IWR.EQ.64
         THEN
            IF IND.EQ.2
            THEN
               IF X.LT..35
               THEN
                  T= .09*(1.-((2.*X-.35)/.35)**2)
               ELSE
                  IF X.LT..64
                  THEN
                     T=-.07*(1.-((2.*X- .99)/.29)**2)
                  ELSE
                     IF X.LT..85
                     THEN
                        T= .05*(1.-((2.*X-1.44)/.16)**2)
                     ELSE
                        T=-.09
                     CIF
                  CIF
               CIF
            ELSE
               IF X.LT..40
               THEN
                  T= .09*(1.-((2.*X-.40)/.40)**2)
               ELSE
                  T=0.
               CIF
            CIF
         CIF
         Y1COR=Y1COR+T
      CIF
      Y1=Y1+Y1COR
      CPROC
C
CCCCCCCCCCCCCCCC
C     FUNCTION SQTVAL(F,G,AL,EPS)
C
C     CALCULATE SQTVAL=F*(SQRT(1+G*AL**2/F**2)-1).
C     TO ACHIEVE GOOD PRECISION, FOR LARGE F THE TAYLOR EXPANSION
C     IS USED UPTO AT MOST 15 TERMS
C     EPS IS THE REQUIRED ABSOLUTE PRECISION
C
      PROC CALSQT
      S=G/F
      U=-S/F
      S=-.5*S
      IF ABS(U).GT..3
      THEN
         IF U.LT..98
         THEN
            SQTVAL=F*(SQRT(1.-U)-1.)
         ELSE
            SQTVAL=0.
C           PRINT 100,F,G,AL
C100        FORMAT(1X,' SQTVAL',3E16.7)
         CIF
      ELSE
         VAL=-S*(1.+.25*U+.125*U**2)
         QQ=S*U**3/12.8
         N=5
         WHILE ABS(QQ).GT.EPS .AND.N.LT.15
           VAL=VAL-QQ
           QQ=QQ*U*(1.-1.5/N)
           N=N+1
         CWHILE
         SQTVAL=VAL
      CIF
      CPROC
      END
C   19/03/80 312171949  MEMBER NAME  REDONE   (JADEGS)      SHELTRAN
      SUBROUTINE REDONE (INDREJ,LBWRT,IWRT)
C---
C---     SHORT VERSION OF USREDUC1 ON JADEPR.JADESR
C---     RETURNS REJECT CODE INDREJ, WRITE CODES LBWRT AND IWRT
C---                                  LAST CHANGE 25.08.80
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
      COMMON /CREDON/LIMHIT,LIMHT1,CRVTAG,CRVNTG
      COMMON /CIPRI/ IPRI
      COMMON /CHEADR/ HEAD(108)
      EQUIVALENCE (HRUN,HEAD(18)),(HEVENT,HEAD(19))
C
      DATA ETAGLM /300./
      DATA MKTAGE /Z400/, MKLGLE /Z200/, MKLUMI /Z100/, MKFWMU/Z800/
      DATA MKMUCN /Z00F/
C
        IQHEAD = IBLN('HEAD')
        IQTRIG = IBLN('TRIG')
        IQALGN = IBLN('ALGN')
        IQZVTX = IBLN('ZVTX')
        IQJETC = IBLN('JETC')
        IQPATR = IBLN('PATR')
        IQJHTL = IBLN('JHTL')
      IPHEAD=IDATA(IQHEAD)
      CALL MVCL(HEAD(1),0,IDATA(IPHEAD-3),0,216)
C
      INDREJ = 0
      LBWRT = 0
        IWRT = 0
C
N       SET OVERFLOW MARKER
        IFLW= 0
        IF(HEAD(23).NE.0) IFLW = 1
C
N       CHECK TRIGGER WITH T2-ACCEPT
        LBTRBT = 0
N       SET TRIGGER BITS IF 'TRIG' BANK # 1 EXISTS
        IPTRIG = IDATA(IQTRIG)
        IF(IPTRIG.GT.0 .AND. IDATA(IPTRIG-2).EQ.1)
     ?  LBTRBT = HDATA(IPTRIG*2+10)
N       SET LUMI-FLAG
        ILUMI = 0
        IF(LAND(LBTRBT,MKLUMI).NE.0) ILUMI = 1
C
N       CHECK FOR MUON HITS IF FWD MU TRIG
        IMUACC=0
        IF(LAND(LBTRBT,MKFWMU).NE.0) CALL MEWT3(IMUACC)
        IF IMUACC.LE.0
        THEN
        CALL TRGCHK(LBTRCK,LBTRBT)
N       TRIGGER CHECK -VE
C                                                     >>>>> REJECT <<<<<
        INDREJ =  1
        IF(IPRI.GT.0.AND.LBTRCK.EQ.0) WRITE(6,4810)
4810  FORMAT(' REJECTED BY TRIGGER CHECK  ')
        IF(LBTRCK.EQ.0) RETURN
C
N       LG-ENERGY IN CYLINDER AND E-CAPS; SET FLAG IACC
        IACC = 0
        CALL LGCUTX(IACC,ECYL,ECAMI,ECAPL,ETOT)
N       RESET IACC IF NO HIGH ENERGY TRIGGER
        IF(LAND(LBTRBT,MKLGLE).EQ.0) IACC = 0
C
C  SET   FLAG FOR ENERGY IN FORWARD TAGGING BLOCKS
C              IFTG = 0      NO ENERGY
C              IFTG = 11     ENERGY ABOVE LIMIT IN NEG. FW ARM
C              IFTG = 12     ENERGY ABOVE LIMIT IN POS. FW ARM
C              IFTG = 113/13 ENERGY ABOVE LIMIT IN BOTH FW ARMS(LUMI)
        IFTG = 0
        IF(LAND(LBTRBT,2).NE.0) CALL TAGFLG(IFTG)
        ETAG = 0.
        IF(IFTG.NE.0. .AND. LAND(LBTRBT,MKTAGE).NE.0) ETAG = ETOT
C
N       SET WRITE FLAG FOR HIGH ENERGY, OVERFLOW, TAGGING, MU-CAND.
        IF(IACC.NE.     0) IWRT = 1
        IF(IFLW.NE.     0) IWRT = IWRT + 2
        IF ETAG.GT.ETAGLM .AND. IWRT.EQ.0
        THEN
          ECENTR = 0.
          IF(IFTG.EQ.11) ECENTR = ETOT - ECAMI
          IF(IFTG.EQ.12) ECENTR = ETOT - ECAPL
          IF(IFTG.GT.12) ECENTR = ETOT
          IF(ECENTR.GT.100.) IWRT = IWRT + 4
C         IF(IFTG.GT.0.AND.IFTG.LT.11.AND.ETOT.GT.100.) IWRT=IWRT+4
        CIF
C
N       IWRT = 0 .AND. GOOD TRIGGER CHECK -VE
C                                                     >>>>> REJECT <<<<<
        INDREJ =  2
        IF(IPRI.GT.0.AND.LBTRCK.EQ.16.AND.IWRT.EQ.0) WRITE(6,4811)
4811  FORMAT(' REJECTED BY TRIGGER CHECK, WITH IWRT = 0 ,NO TRACKS ')
        IF(IWRT.EQ.0 .AND. LBTRCK.EQ.16) RETURN
N       ACCEPT FWD MUON TRIG WITH MUON HITS
        ELSE
        IWRT=1
        LBWRT=16
        INDREJ = 0
        IF(IPRI.GT.0) WRITE(6,4521)
4521  FORMAT(' ACCEPTED AS FW MU TRIGGER WITH MU TRACKS ')
        RETURN
        CIF
C
C       CHECK IF HITS IN ID
        IPJCA  = IDATA(IQJETC)
        IF IPJCA.LE.0
        THEN
N         NO HITS IN 'JETC'
C                                                     >>>>> REJECT <<<<<
          INDREJ =  3
        IF(IPRI.GT.0.AND.IWRT.EQ.0) WRITE(6,4812)
4812  FORMAT(' NO HITS IN INNER DET.,WITH IWRT = 0  REJECTED ')
          IF(IWRT.EQ.0) RETURN
N         WRITE  IF IWRT = 1
N         IWRT=1, NO HITS IN 'JETC'
C                                                     ***** ACCEPT *****
          INDREJ = 0
          LBWRT =  1
        IF(IPRI.GT.0) WRITE(6,4813)
4813  FORMAT(' NO HITS IN INNER DET.,WITH IWRT = 1  ACCEPTED ')
          RETURN
        CIF
C
C
N       STOP ANALYSIS FOR IWRT=1,4
        INDREJ = 0
        LBWRT = 2
        IF(IPRI.GT.0.AND.(IWRT.EQ.1.OR.IWRT.EQ.4)) WRITE(6,4814)
4814  FORMAT('  IWRT = 1,4 ACCEPTED ')
        IF(LAND(IWRT,5).NE.0) RETURN
        LBWRT = 0
C
        IPJCA  = IDATA(IQJETC)
        IPJCA2 = IPJCA*2
        IPZV   = IDATA(IQZVTX)
        IF IPZV.GT.0
        THEN
          IFLAG  = IDATA(IPZV+6)
          ZVTX   = ADATA(IPZV+1)
          PEAK   = ADATA(IPZV+4)
C
N         ONLY CLEAN VTX FOR 1T + 1T(R1) * CLEAN R1
          IF(IFLAG.LT.3 .AND. LBTRCK.EQ.8) IFLAG = 0
          IF IFLAG.LE.0
          THEN
N           EVENTS WITHOUT ZVTX
C                                                     >>>>> REJECT <<<<<
             IF IWRT.EQ.0
             THEN
                INDREJ=4
                IF(IPRI.GT.0) WRITE(6,4347)
4347  FORMAT('  NO ZVTX FOUND,IWRT=0   REJECTED ')
             ELSE
                LBWRT = 14
                IF(IPRI.GT.0) WRITE(6,4387)
4387  FORMAT('  NO ZVTX FOUND,IWRT>0   ACCEPTED ')
             CIF
             RETURN
          ELSE
C
            IF ABS(ZVTX).GT.350.
            THEN
             IF IWRT.EQ.0
             THEN
                INDREJ=5
                IF(IPRI.GT.0) WRITE(6,9347)
9347  FORMAT('  ZVTX > 350,IWRT=0   REJECTED ')
             ELSE
                LBWRT = 15
                IF(IPRI.GT.0) WRITE(6,9387)
9387  FORMAT('  ZVTX > 350,IWRT>0   ACCEPTED ')
             CIF
             RETURN
            CIF
          CIF
        ELSE
N         NO 'ZVTX'-BANK
C                                                     >>>>> REJECT <<<<<
          IF IWRT.EQ.0
          THEN
             INDREJ=6
        IF(IPRI.GT.0) WRITE(6,4817)
4817  FORMAT('  NO ZVTX BANK,IWRT=0   REJECTED ')
          ELSE
             LBWRT = 6
        IF(IPRI.GT.0) WRITE(6,4887)
4887  FORMAT('  NO ZVTX BANK,IWRT>0   ACCEPTED ')
          CIF
          RETURN
        CIF
C
        IPPATR = IDATA(IQPATR)
        IF IPPATR.LE.0
        THEN
           IF IWRT.EQ.0
           THEN
              INDREJ = 7
        IF(IPRI.GT.0) WRITE(6,4818)
4818  FORMAT('  NO PATR BANK,IWRT=0   REJECTED ')
           ELSE
              LBWRT = 7
        IF(IPRI.GT.0) WRITE(6,4888)
4888  FORMAT('  NO PATR BANK, IWRT>0  ACCEPTED ')
           CIF
           RETURN
        CIF
C
        NTR    = IDATA(IPPATR+2)
        LDTR   = IDATA(IPPATR+3)
        IPTR0  = IPPATR + IDATA(IPPATR+1)
        IPTR9  = IPTR0 + (NTR-1)*LDTR
        IF NTR.LE.0
        THEN
N         0 TRACKS
C                                                     >>>>> REJECT <<<<<
          IF IWRT.EQ.0
          THEN
             INDREJ = 8
             IF(IPRI.GT.0) WRITE(6,4819)
4819  FORMAT('  NO TRACKS, IWRT=0   REJECTED ')
          ELSE
             LBWRT = 8
             IF(IPRI.GT.0) WRITE(6,4889)
4889  FORMAT('  NO TRACKS, IWRT>0   ACCEPTED ')
          CIF
          RETURN
        CIF
C
        IF IFTG.NE.0
        THEN
C
N         ***** TAGGING EVENTS WITH IWRT=0
          ACRV=1000.
          ZMIN = 1000.
N         FIND MIN(Z) OF LONG TRACKS, >200MEV
          FOR IPTR=IPTR0,IPTR9,LDTR
            IF IDATA(IPTR+24).GT.LIMHIT
            THEN
              CRV = ADATA(IPTR+25)
              IF(ABS(CRV).LT.ACRV) ACRV=ABS(CRV)
N             CURVATURE CUT CORRESPONDING TO 200 MEV
              IF ABS(CRV)      .LE.CRVTAG
              THEN
                AZV = ADATA(IPTR+31)
N               ZR FIT INTERCEPT WITH Z-AXIS
                IF(ABS(AZV).LT.ABS(ZMIN)) ZMIN = AZV
              CIF
            CIF
          CFOR
          IF ABS(ZMIN).GT.300.
          THEN
N           MIN(Z) > 300.
C                                                     >>>>> REJECT <<<<<
            IF IWRT.EQ.0
            THEN
               INDREJ = 9
               IF(IPRI.GT.0) WRITE(6,4829)
4829  FORMAT('  TAGGED EVENT, ZMIN > 300 MM IWRT=0   REJECTED ')
            ELSE
               LBWRT = 9
               IF(IPRI.GT.0) WRITE(6,4869)
4869  FORMAT('  TAGGED EVENT, ZMIN > 300 MM IWRT>0   ACCEPTED ')
            CIF
            RETURN
          CIF
          IWRT = IWRT + 16
N         MIN(Z) < 300.
C                                                     ***** ACCEPT *****
          INDREJ = 0
          LBWRT = 3
        IF(IPRI.GT.0) WRITE(6,4830)
4830  FORMAT('  TAGGED EVENT, ZMIN < 300 MM  ACCEPTED ')
          RETURN
        CIF
C
N       ***** OTHER EVENTS WITH IWRT=0
C
N       LABEL FOR LONG TRACKS
        LBLONG = 0
N       LABEL FOR LONG TRACKS, >600MEV
        NE100 = 0
        ZSUM = 0.
        LBEHIG = 0
        ACRV=1000.
        ZAV=0.
        ZMIN = 1000.
        IKNT=0
N       DET. MIN(Z) FOR LONG TRACKS, >600MEV
        FOR IPTR=IPTR0,IPTR9,LDTR
N         CHECK IF LIMHIT HITS USED IN ZR-FIT
          IF IDATA(IPTR+33).GT.LIMHIT
          THEN
            LBLONG = 1
            CRV = ADATA(IPTR+25)
            AZV = ADATA(IPTR+31)
            IF(ABS(CRV).LT.ACRV) ACRV=ABS(CRV)
N           CURVATURE CUT CORRESPONDING TO C:A 600 MEV
            IF ABS(CRV)      .LE.CRVNTG
            THEN
              LBEHIG = 1
              ZAV=ZAV+AZV
              IKNT=IKNT+1
              IF(ABS(AZV).LT.ABS(ZMIN)) ZMIN = AZV
            ELSE
              IF ABS(CRV).LT.CRVTAG.AND.ABS(AZV).LE.300.
              THEN
                RSQ = ADATA(IPTR+5)**2+ADATA(IPTR+6)**2
                IF RSQ.LT.250000.
                THEN
                   NE100 = NE100 + 1
                   ZSUM = AZV + ZSUM
                CIF
              CIF
            CIF
          CIF
        CFOR
        ISTAR = LBEHIG*2 + LBLONG
        IF(NE100.GE.2) ISTAR = ISTAR + 4
        IF(IKNT.GT.0) ZAV=ZAV/FLOAT(IKNT)
        IF(NE100.GE.2.AND.LBEHIG.EQ.0) ZMIN = ZSUM/FLOAT(NE100)
C
N       ***** EVENTS WITH SHORT TRACKS ONLY
        IF LBLONG.EQ.0
        THEN
          ETOTX = ECAMI + ECAPL
N         CHECK IF LONG TRACK IN R-FI
          ACRV=1000.
          IKNT=0
          FOR IPTR=IPTR0,IPTR9,LDTR
N           CHECK IF LIMHT1 HITS USED IN RFI-FIT
            IF IDATA(IPTR+24).GT.LIMHT1
            THEN
              LBLONG = 1
              CRV = ADATA(IPTR+25)
              IF(ABS(CRV).LT.ACRV) ACRV=ABS(CRV)
              IKNT=IKNT+1
            CIF
          CFOR
          IF LBLONG.EQ.0
          THEN
N           ONLY SHORT TRACKS
C                                                     >>>>> REJECT <<<<<
            IF IWRT.EQ.0
            THEN
               INDREJ = 10
               IF(IPRI.GT.0) WRITE(6,4831)
4831  FORMAT('  NOTAG EVENT, ONLY SHORT TRACKS  IWRT=0    REJECTED ')
            ELSE
               LBWRT = 10
               IF(IPRI.GT.0) WRITE(6,4851)
4851  FORMAT('  NOTAG EVENT, ONLY SHORT TRACKS  IWRT>0  ACCEPTED ')
            CIF
            RETURN
          CIF
C
N         LONG TRACKS IN R-FI, SHORT TRACKS IN R-Z
C                                                     ***** ACCEPT *****
          INDREJ = 0
          LBWRT = 4
        IF(IPRI.GT.0) WRITE(6,4832)
4832  FORMAT('  NOTAG EVENT, LONG TRACKS RFI, SHORT RZ, ACCEPTED ')
          IWRT = IWRT + 32
          RETURN
        CIF
C
N       ***** LONG TRACKS
        IF LBEHIG.EQ.0.AND.NE100.LT.2
        THEN
N         ***** LONG TRACKS, <600MEV
          ETOTX = ECAMI + ECAPL
N         LONG TRACKS, <600MEV
C                                                     >>>>> REJECT <<<<<
          IF IWRT.EQ.0
          THEN
             INDREJ = 11
             IF(IPRI.GT.0) WRITE(6,4833)
4833  FORMAT(' LONG TRKS < 600 , < 2 WEAK TRACKS, IWRT=0  REJECTED')
          ELSE
             LBWRT = 11
             IF(IPRI.GT.0) WRITE(6,4873)
4873  FORMAT(' LONG TRKS < 600 , < 2 WEAK TRACKS, IWRT>0  ACCEPTED')
          CIF
          RETURN
        CIF
C
N       ***** LONG TRACKS, >600MEV
        IF ABS(ZMIN).GT.300.
        THEN
N         MIN(Z) >300.
C                                                     >>>>> REJECT <<<<<
          IF IWRT.EQ.0
          THEN
             INDREJ = 12
             IF(IPRI.GT.0) WRITE(6,4834)
4834  FORMAT(' LONG TRKS > 600, OR<2 WEAK TR.ZMIN>300 IWRT=0 REJECTED')
          ELSE
             LBWRT = 12
             IF(IPRI.GT.0) WRITE(6,4884)
4884  FORMAT(' LONG TRKS > 600, OR<2 WEAK TR.ZMIN>300 IWRT>0 ACCEPTED')
          CIF
          RETURN
        CIF
C
N       ***** LONG TRACKS, >600MEV, MIN(Z)<300.
C
C       CHECK RMIN
        RMIN=10000.
        FOR IPTR=IPTR0,IPTR9,LDTR
          IF IDATA(IPTR+33).GT.LIMHIT
          THEN
            CRV = ADATA(IPTR+25)
            AZV = ADATA(IPTR+31)
            IF ABS(CRV)      .LE.CRVTAG.AND.ABS(AZV).LE.300.
            THEN
              CALL PRTOCI(IPTR,DUM1,RM,DUM2,DUM3)
              IF(RM.LT.RMIN) RMIN=RM
            CIF
          CIF
        CFOR
N       RMIN > 60.
C                                                     >>>>> REJECT <<<<<
        IF RMIN.GT.60.
        THEN
           IF IWRT.EQ.0
           THEN
              INDREJ = 13
              IF(IPRI.GT.0) WRITE(6,4835)
4835  FORMAT(' LONG>600,OR<2 WEAK TR.ZMN>300 IWRT=0 RMIN>60 REJECTED')
           ELSE
              LBWRT = 13
              IF(IPRI.GT.0) WRITE(6,4895)
4895  FORMAT(' LONG>600,OR<2 WEAK TR.ZMN>300 IWRT>0 RMIN>60 ACCEPTED')
           CIF
           RETURN
        CIF
N       LONG TRACKS, >600MEV, MIN(Z)<300.,
C                                                     ***** ACCEPT *****
N       RMIN<60.
        INDREJ = 0
        LBWRT  = 5
        IF(IPRI.GT.0) WRITE(6,4836)
4836  FORMAT(' LONG TR>600 OR 2 WEAK TR., ZMIN<300 RMIN<60  ACCEPTED')
        IWRT = IWRT + 64
        RETURN
      END
      BLOCK DATA
      COMMON /CREDON/LIMHIT,LIMHT1,CRVTAG,CRVNTG
      COMMON /CIPRI/IPRI
      DATA IPRI /0/
      DATA LIMHIT/12/, LIMHT1/20/
      DATA CRVTAG/.00150/, CRVNTG/.00025/
      END
      SUBROUTINE TRGCHK(LBTRCK,LBTRBT)
C
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
C
#include "cdata.for"
C
#include "cheadr.for"
C
      COMMON /CWORK/ ILGE(84),HCLWR(96)
C
N     MASKS FOR EVENT TRIGGER  + HIGH ENERGY
      DATA MKTREV /Z653/, MKT2AC /Z53/, MKEHIG /Z600/
N     MASKS FOR TRIGGER IN 'LATC' BANK
      INTEGER MKTRBT( 7) /1,2,4,8,16,32,64/
N     MASKS FOR HITS IN LAYERS OF JETC
C     INTEGER MKCLBT(16) /1,2,4,8,16,32,64,128,256,512,1024,2048,
C    ,                    Z1000,Z2000,Z4000,Z8000/
N     # OF CELLS FOR TOF COUNTER 1...7
      INTEGER NCTOF ( 7) /2,3,3,3,3,3,3/
N     1. CELL FOR TOF-COUNTER 1...7
      INTEGER ICTOF ( 7) /0,1,2,3,4,5,6/
C
N     INITIALIZE LABEL FOR TRIGGER CHECK
      LBTRCK = 0
C
N     REJECT LUMI-TRIGGER
      IF(LAND(LBTRBT,MKTREV).EQ.0) RETURN
C
N     ACCEPT T1-ACCEPT TRIGGER
      IF(LAND(LBTRBT,MKEHIG).NE.0) LBTRCK = 16
C     PRINT 2991, HHEADR(17),HHEADR(18),HHEADR(19),NREC,LBTRBT,LBTRCK
C2991 FORMAT(' **********************   EVENT:',4I6,1X,Z4,1X,Z4)
C
N     CHECK IF T1-POSTPONE
      IF(LAND(LBTRBT,MKT2AC).EQ.0) RETURN
C
N     INITIALIZE LG-ROWS + CELL BITS
      CALL SETSL(ILGE(1),0,528,0)
C
N     ACCUMULATE E(LG-ROWS)
      REPEAT
        IPALGN = IDATA(IBLN('ALGN'))
        IF(IPALGN.LE.0) XREPEAT
N       CHECK IF ANY LG-ENERGIES
        IF(IDATA(IPALGN).LE. 3) XREPEAT
        IPLG2 = IPALGN*2
        NBARR = HDATA(IPLG2+ 4) - HDATA(IPLG2+ 3)
N       CHECK IF ANY LG-ENERGY IN BARREL
        IF(NBARR.LE.0) XREPEAT
N       SUM ENERGIES OF LG-ROWS
        IP0 = IPLG2 + 7
        IP9 = IP0 + NBARR - 1
        FOR IP=IP0,IP9,2
          NBL  = HDATA(IP)
          IROW = SHFTR(NBL,5)
          ILGE(IROW) = ILGE(IROW) + HDATA(IP+1)
        CFOR
C     PRINT 2003, ILGE
C2003   FORMAT('0EROW:',20I6,/,(6X,20I6))
      UNTIL .TRUE.
C
N     ACCUMULATE CELL BITS OF 3. RING
      IPJETC = IDATA(IBLN('JETC'))
      IF(IPJETC.LE.0) RETURN
C     PRINTOUT
C     I0 = IPJETC*2 + 1
C     I9 = I0 + IDATA(IPJETC)*2 - 1
C     PRINT 2001, I0,I9,IDATA(IPJETC),(HDATA(I1),I1=I0,I9)
C2001 FORMAT('0JETC:',3I6,/,(6X,20I6))
C
N     CHECK IF ANY JETC DATA
      IF(IDATA(IPJETC).LE.50) RETURN
      IPJC2 = IPJETC*2
      NHTR3 = HDATA(IPJC2+99) - HDATA(IPJC2+51)
N     CHECK IF ANY HITS IN R3
      IF(NHTR3.LE.0) RETURN
N     SET UP 1 LABEL/CELL WITH 1 BIT/LAYER
      IP0 = IPJC2 + 100 + HDATA(IPJC2+51)
      IP9 = IP0 + NHTR3 - 1
      IWIR0 =-1
      FOR IP=IP0,IP9,4
        IWIR = HDATA(IP)
        IWIR = SHFTR(IWIR,3)
        HCLL = SHFTR(IWIR,3) - 95
        IF(IWIR.NE.IWIR0) HCLWR(HCLL) = HCLWR(HCLL) + 1
        IWIR0 = IWIR
      CFOR
C     PRINT 2004, HCLWR
C2004 FORMAT('0HCLWR:',12I6,/,(7X,12I6))
C
N     CHECK IF 'LATC' BANK
      IPLATC = IDATA(IBLN('LATC'))
      IF(IPLATC.LE.0) RETURN
N     LOOP OVER ALL TOF COUNTER
      IPLT2 = IPLATC*2
      IP0 = IPLT2 + 6
      IP9 = IP0 + 5
      ITLST =-100
      ITOF0 = 0
      ICLL0 = 0
      NTRCK = 0
      NTREL = 0
      REPEAT
        ITRBT = HDATA(IP0)
        FOR JTOF=1,7
          IF LAND(MKTRBT(JTOF),ITRBT) .NE. 0
          THEN
N           TOF COUNTER #
            ITOF = ITOF0 + JTOF
C
N           CHECK IF TRACK IN JETC
C
C
N           LABEL FOR OVERLAPPING CELLS
            JCTOF = (ICTOF(JTOF) + ICLL0)*2 - 1
N           INITIALIZE LABEL FOR TRACK CAND.
            LBTRK = 0
C
N           CHECK IF 2 ADJACENT TOF
            IF ITOF-ITLST.LE.2
            THEN
N             CHECK IF 2 DIFFERENT TRACKS
              HCLL9 = NCTOF(JTOF)*2 + JCTOF - 1
              HCLL1 =-99
              MTRK = 0
C     PRINT 2005, ITOF,JTOF,JCTOF,ITLST,HCLL0,HCLL9
              FOR ICLL=HCLL0,HCLL9
                IF HCLWR(ICLL).GE.6
                THEN
                  HDCLL = ICLL - HCLL1
                  IF HDCLL.EQ.1
                  THEN
                    HCLL1 = -99
                  ELSE
                    IF HDCLL.NE.3 .OR. TBIT(ICLL,31)
                    THEN
                      MTRK = MTRK + 1
                      HCLL1 = ICLL
                    CIF
                  CIF
                CIF
              CFOR
              IF(MTRK.GE.2) LBTRK = 1
            ELSE
              NHIT5 = 0
              NHIT6 = 0
              IF JCTOF.LT.0
              THEN
                NHIT1 = HCLWR(95)
                NHIT2 = HCLWR(96)
                NHIT3 = HCLWR( 1)
                NHIT4 = HCLWR( 2)
                IF(NHIT2.GE.3 .AND. HCLWR(93).GE.6) NHIT2 = 8
                IF(NHIT4.GE.3 .AND. HCLWR( 3).GE.6) NHIT4 = 8
              ELSE
                NHIT1 = HCLWR(JCTOF  )
                NHIT2 = HCLWR(JCTOF+1)
                NHIT3 = HCLWR(JCTOF+2)
                NHIT4 = HCLWR(JCTOF+3)
                IF NCTOF(JTOF).EQ.3
                THEN
                  NHIT5 = HCLWR(JCTOF+4)
                  NHIT6 = HCLWR(JCTOF+5)
                ELSE
                  IF(NHIT2.GE.3 .AND. HCLWR(JCTOF-2).GE.6) NHIT2 = 8
                  IF(NHIT4.GE.3 .AND. HCLWR(JCTOF+4).GE.6) NHIT4 = 8
                CIF
              CIF
C     PRINT 2005,ITOF,JTOF,JCTOF,NHIT1,NHIT2,NHIT3,NHIT4,NHIT5,NHIT6
C2005   FORMAT(' JETC:',20I6)
C
N             CHECK IF ENOUGH HITS FOR TRACK CAND.
              IF(MAX0(NHIT1,NHIT2,NHIT3,NHIT4,NHIT5,NHIT6).GE.6) LBTRK=1
            CIF
C
N           CHECK IF TRACK + COUNT
            IF LBTRK.NE.0
            THEN
              NTRCK = NTRCK + 1
              ITLST = ITOF
              HCLL0 = JCTOF
C     PRINT 2005, NHIT1,NHIT2,NTRCK
C
N             CHECK IF ELECTRON TRACK
N             CORRESP. LG-ROW
              IR = ITOF*2 - 3
              IF ITOF.EQ.1
              THEN
                ILGENG = ILGE(83)+ILGE(  84)+ILGE(   1)+ILGE(   2)
              ELSE
                IF ITOF.EQ.42
                THEN
                  ILGENG = ILGE(82)+ILGE(  83)+ILGE(  84)+ILGE(   1)
                ELSE
                  ILGENG = ILGE(IR)+ILGE(IR+1)+ILGE(IR+2)+ILGE(IR+3)
                CIF
                IF(ILGENG.GT.1000) NTREL = NTREL + 1
C     PRINT 2002,NTREL,ITOF,IR,ILGENG
C2002 FORMAT(6X,20I6)
C
              CIF
            CIF
          CIF
        CFOR
      ICLL0 = ICLL0 + 8
      ITOF0 = ITOF0 + 7
      IP0   = IP0   + 1
      UNTIL IP0.GT.IP9
C
N     CHECK IF ONLY 1 TRACK, NO E
      LB1TCL = 0
      IF NTRCK.EQ.1 .AND. NTREL.EQ.0
      THEN
N       CHECK CLEAN 1. RING
        IPJC2 = IPJETC*2
        NHTR1 = HDATA(IPJC2+27) - HDATA(IPJC2+ 3)
N       CHECK IF ANY HITS IN R3
        IF NHTR1.GT.96 .AND. NHTR1.LT.196
        THEN
N         COUNT HITS/HALF CELL
          CALL SETS(HCLWR(1),0,48,0)
          IP0 = IPJC2 + 100 + HDATA(IPJC2+ 3)
          IP9 = IP0 + NHTR1 - 1
          IWIR0 =-1
          FOR IP=IP0,IP9,4
            IWIR = HDATA(IP)
            IWIR = SHFTR(IWIR,3)
            HCLL = SHFTR(IWIR,3) + 1
            IF(IWIR.NE.IWIR0) HCLWR(HCLL) = HCLWR(HCLL) + 1
            IWIR0 = IWIR
          CFOR
C     PRINT 2004, HCLWR
          MTRK = 0
          HCLL1 =-99
          FOR ICLL=1,48
            IF HCLWR(ICLL).GE.6
            THEN
              HDCLL = ICLL - HCLL1
              IF HDCLL.EQ.1
              THEN
                HCLL1 = -99
              ELSE
                IF HDCLL.NE.3 .OR. TBIT(ICLL,31)
                THEN
                  MTRK = MTRK + 1
                  HCLL1 = ICLL
                CIF
              CIF
            CIF
          CFOR
          IF(MTRK.EQ.2 .OR. MTRK.EQ.3) LB1TCL = 1
        CIF
      CIF
C
N     SET LABEL FOR TRIGGER CHECK
N     2 TRACKS IN GENERAL
      IF(NTRCK .GE.2) LBTRCK = LBTRCK + 2
N     1 TRACK FOR TAGG-EVENTS ONLY
      IF(NTRCK .EQ.1 .AND. LAND(LBTRBT,2).NE.0) LBTRCK = LBTRCK + 1
N     1 TRACK FOR ELECTRON EVENTS ONLY
      IF(NTREL .EQ.1) LBTRCK = LBTRCK + 4
N     1 TRACK +1 TRACK IN CLEAN RING 1
      IF(LB1TCL.EQ.1) LBTRCK = LBTRCK + 8
C     PRINT 2009, NTRCK,NTREL,LB1TCL,LBTRCK
C2009 FORMAT('0TRACKS:',4I6)
      RETURN
      END
C   19/08/82 509161839  MEMBER NAME  REFIT    (JADEGS)      SHELTRAN
      SUBROUTINE REFIT(IPTR,IPJHTL)
C
C        REFIT TRACK ITRK IN 'PATR'-BANK
C        P. STEFFEN                    80/08/19
C  CORRECTION OF UNINITIALIZED ARRAY NHTRNG     03.3.1984   J.OLSSON
C  REMOVAL OF DEBUG PRINT...                    16.9.1985   J.OLSSON
C
      IMPLICIT INTEGER*2 (H)
      LOGICAL DEADCL
C
#include "cdata.for"
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
#include "cjdrch.for"
#include "cdsmax.for"
C
      INTEGER DATE(5), IDAY /0/
      DIMENSION ITRCLL(6), NCNCK(24), NHTRNG(3)
C
N     MASK FOR L/R BIT IN HIT LABEL
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
C
N     MASK FOR TRACKS AT CELL WALL
      INTEGER MKBDCL(3) /Z10,Z20,Z40/
      INTEGER MKDDCL(3) /Z01,Z02,Z04/
C
C     IF(IDATA(IPTR+1).LT. 4) RETURN
C     I0 = IPTR + 1
C     I9 = IPTR + 48
C     PRINT 2001, (IDATA(I1),I1=I0,I9)
C     I0 = IPJHTL*2 + 1
C     I9 = I0 + IDATA(IPJHTL)*2 - 1
C     PRINT 2000, IPJHTL,I0,I9,(HDATA(I1),I1=I0,I9)
C     IPJETC = IDATA(IBLN('JETC'))
C     I0 = IPJETC*2 + 1
C     I9 = I0 + 109
C     PRINT 2000, IPJETC,I0,I9,(HDATA(I1),I1=I0,I9)
C2000 FORMAT('0REFIT:',3I8,/,(20(1X,Z4)))
C2001 FORMAT(1H0,2I3,I8,2(I4,3F6.1,3F6.3),
C    ,     /,14X,I3,4E13.5,F6.2,I3,4E13.5,
C    ,     /,14X,I3,2F8.3,F6.1,I3,10X,6I3,8I6,2X,Z4)
C2002 FORMAT('0FETCH:',2I3,2I5,12F9.5)
C2003 FORMAT('0ROTATION:',12F10.5)
C2004 FORMAT('0CIRC.CENTRE:',2I3, F10.5,2F10.0,F8.1,2F8.1)
 2005 FORMAT('0TRACK:',I6,/,(1X,3I6,4F8.3,I4,F8.3,2I4,F8.3,I6,F8.3))
3001  FORMAT('0NHTRNG',3I6)
C2006 FORMAT(1X,I6,5F8.2,F12.1,5F8.2)
C2007 FORMAT(' FETCH:',I3,9F8.4,F10.5,F6.0)
C2008 FORMAT(' FIT:',2I3,F8.3,F5.0,3E13.5,2F8.3)
C2009 FORMAT(' JHTL:',I8,1X,Z8,3I5)
C2010 FORMAT(' HIT:',I6,12F8.2)
C2011 FORMAT('0ABERR:',10F10.6)
C2012 FORMAT('0ERROR:',10E13.6)
C2014 FORMAT('0FIT-BANK:',5F8.3,5X,5F8.3,5X,F8.5,2F8.1)
C2016 FORMAT('0ITRCLL =',6I8,/,(9X,6F8.3))
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
        LBINIT = 1
        PERFORM INIT
      CIF
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
C
N     RESERVE SPACE IN CWORK
      HPFREE = 1
      HPFRE1 = HPFREE
C
C
N     FETCH HITS, CALCULATE COORDINATES, AND
N     FILL ARRAY IN /CWORK/
      HPCO0  = HPFREE
      LHIT   = 14
      INDFET = 2
      CALL JFETCH(IPTR,IPJHTL,WRK(HPCO0),LHIT,IPRES,INDFET)
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
N     INITIALIZE FIT PARAMETERS IN CWORK
      WRK(HPAR0+ 4) = 0.
      WRK(HPAR0+ 5) = 0.
      WRK(HPAR0+ 6) = 0.
      WRK(HPAR0+ 7) = 1000.
      WRK(HPAR0+ 8) = 0.
      CSTH   = WRK (IPRES+11)
      SNTH   = WRK (IPRES+12)
C
N     ZVERT, THETA
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
C     PRINT 2007, HPCO0,HPCO9,HDLCO,HPFREE,(WRK(I1),I1=HPAR0,HPAR9)
C
C
N     COPY TRACK BANK
      HPTR0 = HPFREE
      CALL MVC(IWRK(HPTR0),0,IDATA(IPTR+1),0,192)
      IWRK(HPTR0+1) = 0
C  SET ARRAY NHTRNG    CORRECTION FROM 29.2.1984   J.OLSSON
      IPCO = HPCO0 - HLDCO
      NHTRNG(1) = 0
      NHTRNG(2) = 0
      NHTRNG(3) = 0
      ILAYOL = -1
      REPEAT
        IPCO = IPCO + HLDCO
        ILAY = IWRK(IPCO   )
        IF ILAY.NE.ILAYOL
        THEN
         JRING = IWRK(IPCO+12)
         NHTRNG(JRING) = NHTRNG(JRING) + 1
         ILAYOL = ILAY
        CIF
      UNTIL IPCO.GE.HPCO9-HLDCO
C
C      PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C      PRINT 3001, (NHTRNG(IR),IR=1,3)
      HPFREE = HPFREE + 48
C
C
C
      REPEAT
C
C
N       1. PARABOLA FIT
N       LAST RING INCLUDED IN FIT
        JRINGL = 3
        PERFORM FPARA0
C
N       RELABEL HITS
        ALBLM1 = 0.6
        ALBLM2 = 3.0
        PERFORM LABEL
C       PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
        REPEAT
N         REFIT PARABOLA
          PERFORM FPARA0
C
N         RELABEL HITS
          PERFORM LABEL
C
N         REPEAT FIT IF >3 NEW GOOD HITS
        UNTIL NHGOOD-NHFIT .LT.4
C       PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N       SET UP FIT-BANK
        IF SIG.LT.1.
        THEN
          PERFORM FITBNK
        CIF
C
N       STOP IF SIG < .10
        IF(SIG.LT..10) XREPEAT
C
N       STOP IF HIGH MOMENTUM
        IF(ABS(PAR1).LT..00030) XREPEAT
C
N       STOP IF NOT ENOUGH HITS IN R1 + R2
        IF(NHTRNG(1)+NHTRNG(2).LE.16) XREPEAT
C
N       CONTINUE + FIT ONLY R1 + R2
        ALBLM1 = 2.0
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 2
        PERFORM FPARA0
        ALBLM1 = 1.0
        PERFORM LABEL
        PERFORM FPARA0
        PERFORM LABEL
        IF SIG.LT..20
        THEN
          PERFORM FITBK1
        CIF
C       IF(ITRK.EQ.17) PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N       STOP IF GOOD FIT
        IF(SIG.LT..20) XREPEAT
C
N       STOP IF NOT LOW MOMENTUM
        IF(ABS(PAR1).LT..00150) XREPEAT
C
N       STOP IF NOT ENOUGH HITS IN R1
        IF(NHTRNG(1).LE.9) XREPEAT
C
N       CONTINUE + FIT IN R1 ONLY
        ALBLM1 = 2.0
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 1
        NHTFIT = NHTRNG(1)
        IF NHTFIT.LE.5
        THEN
          NHTFIT = NHTFIT + NHTRNG(2)
          JRINGL = 2
        CIF
        IF NHTFIT.GT.9
        THEN
          PERFORM FPARA0
          ALBLM1 = 1.0
          PERFORM LABEL
          PERFORM FPARA0
          PERFORM LABEL
          IF SIG.LT..20
          THEN
            PERFORM FITBK1
          CIF
        CIF
C       IF(ITRK.EQ.17) PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
C
      UNTIL .TRUE.
C
C
      HPFREE = HPFRE1
      RETURN
C
N     *************************
N     *      F P A R A 0      *
N     *************************
C
C
N     PARABOLA FIT THROUG ORIGIN
      PROC FPARA0
C
N     GET EQUATIONS
N     WEIGHT ORIGIN AS POINT OF PARABOLA
      S0 = 0.
      S1 = 0.
      S2 = 0.
      S3 = 0.
      S4 = 0.
      S7 = 0.
      S6 = 0.
      S5 = 0.
      IPCO = HPCO0
      REPEAT
       IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
       THEN
          X = WRK(IPCO+3)
          Y = WRK(IPCO+4)
          X2 = X**2
          S1 = S1 + X
          S2 = S2 + X2
          S3 = S3 + X*X2
          S4 = S4 + X2**2
          S5 = S5 + Y*X2
          S6 = S6 + Y*X
          S7 = S7 + Y
          S0 = S0 + 1.
        CIF
      IPCO = IPCO + HLDCO
      UNTIL IPCO.GT.HPCO9
      IF S0.LT.3.5
      THEN
        SIG = 1000.
      ELSE
C
N       SOLVE EQUATIONS FOR PARABOLA FIT
        F1 = 1. / S4
        XX12 = S3*F1
        XX13 = S2*F1
        YY1  = S5*F1
        XX22 = S2 - S3*XX12
        XX23 = S1 - S3*XX13
        YY2  = S6 - S3*YY1
        XX32 = S1 - S2*XX12
        XX33 = S0 - S2*XX13
        YY3  = S7 - S2*YY1
        IF XX22.GT.XX32
        THEN
          XX23 = XX23 / XX22
          YY2  = YY2  / XX22
          PAR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
          PAR2 = YY2 - XX23*PAR3
        ELSE
          XX33 = XX33 / XX32
          YY3  = YY3  / XX32
          PAR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
          PAR2 = YY3 - XX33*PAR3
        CIF
        PAR1 = YY1 - XX12*PAR2 - XX13*PAR3
        DEG = S0 - 3.
        NHFIT = S0 + .1
C
C
N       CALC. CHISQ + SOLVE L/R AMBIGUITY
        CHISQ = 0.
        DCHIM1 = 0.
        IHITM1 = 0
        XST    = 999999.
        XEN    =-999999.
        IPCO = HPCO0
        REPEAT
         IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
         THEN
            X = WRK(IPCO+3)
            IF(X.LT.XST) XST = X
            IF(X.GT.XEN) XEN = X
            Y = WRK(IPCO+4)
            F = (PAR1 *X + PAR2 )*X + PAR3
            DCHI = Y - F
            WRK(IPCO+13) = DCHI
N           SUM FOR RMS
            CHISQ = CHISQ + DCHI**2
N           KEEP BIGGEST RMS
C           IF ABS(DCHI).GE.DCHIM1
C           THEN
C             DCHIM1 = ABS(DCHI)
C             IHITM1 = IPCO
C           CIF
C     PRINT 2006, IPCO,X,Y,F,DCHI,CHISQ
          CIF
        IPCO = IPCO + HLDCO
        UNTIL IPCO.GT.HPCO9
        SIG    =      CHISQ  / DEG
C     PRINT 2008, JRINGL,IWRK(IHEND),SIG,DEG,PAR1,PAR2,PAR3,WGHT0,Y0
C     PRINT 2012, S0,S1,S2,S3,S4,S5,S6,S7
C
      CIF
C
      CPROC
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
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
      YEN  = (PAR1 *XEN + PAR2 )*XEN + PAR3
N     DIRECTION AT START + END POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
      TGEN = PAR1*XEN*2 + PAR2
      DXEN = 1./SQRT(TGEN**2+1.)
      DYEN = DXEN * TGEN
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
C     CURV =-PAR1 * 2.
      CVZW = TGST**2+1.
      CVST =-PAR1 * 2 / (SQRT(CVZW)*CVZW)
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      IWRK(IP+ 1) = ITRK
      IWRK(IP+ 2) = 16
      IWRK(IP+ 3) = IDAY
      IWRK(IP+ 4) =  0
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2) * TGTH + ZVERT
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)*CSTH
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)*CSTH
      WRK (IP+10) = SNTH
      IWRK(IP+11) = 0
      WRK (IP+12) = XEN *CSROT - YEN *SNROT + XT
      WRK (IP+13) = XEN *SNROT + YEN *CSROT + YT
      WRK (IP+14) = SQRT(WRK(IP+12)**2 + WRK(IP+13)**2) * TGTH + ZVERT
      WRK (IP+15) = (DXEN*CSROT - DYEN*SNROT)*CSTH
      WRK (IP+16) = (DXEN*SNROT + DYEN*CSROT)*CSTH
      WRK (IP+17) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
      IF(SIG  .LT.0) PRINT 2021,WRK(IP+1),S0,SIG
 2021 FORMAT(' REFIT(PST): -VE SQRT:',I4,5E13.5)
      WRK (IP+23) = SIG
      IF(SIG  .GT.0) WRK(IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CVST
      IF(SIG11.LT.0) PRINT 2021,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SIG*SIG11
      IF(WRK(IP+26) .GT. 0) WRK(IP+26) = SQRT(WRK(IP+26))*2.
      WRK (IP+27) = CVST
      WRK (IP+28) = CVST
      I0 = IP+ 1
      I9 = IP+48
C     PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
N     *************************
N     *      F I T B K 1      *
N     *************************
C
C
N     CHANGE FIT BANK (1.POINT)
      PROC FITBK1
C
N     START POINT
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
N     DIRECTION AT START POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
C     CURV =-PAR1 * 2.
      CVZW = TGST**2+1.
      CVST =-PAR1 * 2 / (SQRT(CVZW)*CVZW)
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV,
C    ,            XMIN,YMIN
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2) * TGTH + ZVERT
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)*CSTH
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)*CSTH
      WRK (IP+10) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
      IF(SIG  .LT.0) PRINT 2022,WRK(IP+1),S0,SIG
 2022 FORMAT(' REFIT(PST): -VE SQRT(1):',I4,5E13.5)
      WRK (IP+23) = SIG
      IF(SIG  .GT.0) WRK(IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CVST
      IF(SIG11.LT.0) PRINT 2022,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SIG*SIG11
      IF(WRK(IP+26) .GT. 0) WRK(IP+26) = SQRT(WRK(IP+26))*2.
      WRK (IP+27) = CVST
C     I0 = IP+ 1
C     I9 = IP+48
C     PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
C
N     *************************
N     *      L A B E L        *
N     *************************
C
C
N     LABEL USED HITS
      PROC LABEL
C
N       PRESET LAST HIT POINTER
        NHGOOD = 0
        IWL = -999
        FOR IP = HPCO0,HPCO9,HLDCO
          IW0 = IWRK(IP)
          X   = WRK(IP+3)
          Y   = WRK(IP+4)
          F   = (PAR1*X + PAR2)*X + PAR3
          DF  = F - Y
N         SELECT CLOSEST HIT
          LBGOOD = 4
          IF(ABS(DF).LT.ALBLM2) LBGOOD = 1
          IF(ABS(DF).LT.ALBLM1) LBGOOD = 0
          IWRK(IP+ 10) = LBGOOD
          IF(LBGOOD.EQ.0) NHGOOD = NHGOOD + 1
          WRK (IP+13) = DF
C
N         CHECK IF 2 HITS FROM SAME WIRE
          IF IWL.EQ.IW0
          THEN
N           SELECT CLOSEST HIT
            IF ABS(DFL).LT.ABS(DF)
            THEN
              IF(IWRK(IP +10).EQ.0) NHGOOD = NHGOOD - 1
              IWRK(IP +10) = 16
            ELSE
              IF(IWRK(IPL+10).EQ.0) NHGOOD = NHGOOD - 1
              IWRK(IPL+10) = 16
            CIF
          CIF
N         STORE LAST POINTERS + DF
          IWL = IW0
          IPL = IP
          DFL = DF
        CFOR
C
      CPROC
C
C
N     *************************
N     *      I N I T          *
N     *************************
C
C
N     INITIALIZE CONSTANTS
      PROC INIT
C
        IQHEAD = IBLN('HEAD')
C
        CALL DAY2(DATE)
        IDAY = DATE(1)*1000 + DATE(2)
C
      CPROC
C
      END
C   16/08/82 509161841  MEMBER NAME  REFITV   (JADEGS)      SHELTRAN
      SUBROUTINE REFITV(IPTR,IPJHTL,ERRFAC)
C
C        REFIT TRACK ITRK IN 'PATR'-BANK USING ORIGIN
C                   ONLY INTERMEDIATE VALUES STORED
C                   FOR POSITION + DIRECTION AT 1. AND LAST HIT
C                   THIS ROUTINE IS ONLY USED WITH SUBSEQUENT ZRFIT
C                   USE REFITV IF ONLY R-PHI-FIT WANTED
C        P. STEFFEN                    22/08/80
C  CORRECTION OF UNINITIALIZED ARRAY NHTRNG     29.2.1984   J.OLSSON
C  REMOVAL OF DEBUG PRINT...                    16.9.1985   J.OLSSON
C
      IMPLICIT INTEGER*2 (H)
      LOGICAL DEADCL
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
C
      INTEGER DATE(5), IDAY /0/
      DIMENSION ITRCLL(6), NCNCK(24), NHTRNG(3)
C
N     JET-CHAMBER AND VERTEX RESOLUTION
      DATA RESJ0 /.200/, RESV0 /.300/
C
N     MASK FOR L/R BIT IN HIT LABEL
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
C
N     MASK FOR TRACKS AT CELL WALL
      INTEGER MKBDCL(3) /Z10,Z20,Z40/
      INTEGER MKDDCL(3) /Z01,Z02,Z04/
C
C     IF(IDATA(IPTR+1).LT. 4) RETURN
C     I0 = IPTR + 1
C     I9 = IPTR + 48
C     PRINT 2001, (IDATA(I1),I1=I0,I9)
C     I0 = IPJHTL*2 + 1
C     I9 = I0 + IDATA(IPJHTL)*2 - 1
C     PRINT 2000, IPJHTL,I0,I9,(HDATA(I1),I1=I0,I9)
C     IPJETC = IDATA(IBLN('JETC'))
C     I0 = IPJETC*2 + 1
C     I9 = I0 + 109
C     PRINT 2000, IPJETC,I0,I9,(HDATA(I1),I1=I0,I9)
C2000 FORMAT('0REFIT:',3I8,/,(20(1X,Z4)))
C2001 FORMAT(1H0,2I3,I8,2(I4,3F6.1,3F6.3),
C    ,     /,14X,I3,4E13.5,F6.2,I3,4E13.5,
C    ,     /,14X,I3,2F8.3,F6.1,I3,10X,6I3,8I6,2X,Z4)
C2002 FORMAT('0FETCH:',2I3,2I5,12F9.5)
C2003 FORMAT('0ROTATION:',12F10.5)
C2004 FORMAT('0CIRC.CENTRE:',2I3, F10.5,2F10.0,F8.1,2F8.1)
2005  FORMAT('0TRACK:',I6,/,(1X,3I6,4F8.3,I4,F8.3,2I4,F8.3,I6,F8.2))
3001  FORMAT('0NHTRNG',3I6)
C2006 FORMAT(1X,I6,5F8.3,F12.1,5F8.3)
C2007 FORMAT(' FETCH:',I3,9F8.4,F10.5,F6.0)
C2008 FORMAT(' FIT:',2I3,2F8.3,F5.0,3E12.5,F6.3,F6.3)
C2009 FORMAT(' JHTL:',I8,1X,Z8,3I5)
C2010 FORMAT(' HIT:',I6,12F8.2)
C2011 FORMAT('0ABERR:',10F10.6)
C2012 FORMAT('0ERROR:',10E13.6)
C2014 FORMAT('0FIT-BANK:',5F8.3,5X,5F8.3,5X,F8.5,2F8.1)
C2016 FORMAT('0ITRCLL =',6I8,/,(9X,6F8.3))
C2107 FORMAT(' SIGLM:',10F8.3)
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
        LBINIT = 1
        PERFORM INIT
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
      HPFRE1 = HPFREE
C
N     GET X-Y-VERTEX AND DETERMINE ERROR
      IPV    = ICALIB(10)
      XO     = ACALIB(IPV+ 1)
      YO     = ACALIB(IPV+ 3)
C     I0 = IPV + 1
C     I9 = IPV + 6
C     PRINT 2029, XO,YO,(ACALIB(I1),I1=I0,I9)
      PTRANS = ABS(0.0299792458*BKGAUS/ADATA(IPTR+25)) * .001
      RESV   = RESV0**2 + RESMS / PTRANS**2
      WGHT0  = RESJ0**2 / RESV
      F1     = ERRFAC
      IF(F1 .LT. .10) F1 = .10
      WGHT0  = WGHT0 / F1**2
C     PRINT 2029, XO,YO,WGHT0,F1,RESV,RESMS,PTRANS
C2029 FORMAT(' VERTEX',9E13.5)
C     PRINT 2011,ABERR
C
C
N     FETCH HITS, CALCULATE COORDINATES, AND
N     FILL ARRAY IN /CWORK/
      HPCO0  = HPFREE
      LHIT   = 14
      INDFET = 3
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
      X0     = WRK (IPRES+ 9)
      Y0     = WRK (IPRES+10)
      XOR    =- XT*CSROT -  YT*SNROT
      YOR    =  XT*SNROT -  YT*CSROT
C     PRINT 2003, CSROT,SNROT,XX,YY,XT,YT,X0,Y0,XO,YO,XOR,YOR,WGHT0
C
N     INITIALIZE FIT PARAMETERS IN CWORK
      WRK(HPAR0+ 4) = 0.
      WRK(HPAR0+ 5) = 0.
      WRK(HPAR0+ 6) = 0.
      WRK(HPAR0+ 7) = 1000.
      WRK(HPAR0+ 8) = 0.
      CSTH   = WRK (IPRES+11)
      SNTH   = WRK (IPRES+12)
C
N     ZVERT, THETA
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
C     PRINT 2007, HPCO0,HPCO9,HDLCO,HPFREE,(WRK(I1),I1=HPAR0,HPAR9)
C
C
N     COPY TRACK BANK
      HPTR0 = HPFREE
      CALL MVC(IWRK(HPTR0),0,IDATA(IPTR+1),0,192)
      HPFREE = HPFREE + 48
      IWRK(HPTR0+1) = 0
C  SET ARRAY NHTRNG    CORRECTION FROM 29.2.1984   J.OLSSON
      IPCO = HPCO0 - HLDCO
      NHTRNG(1) = 0
      NHTRNG(2) = 0
      NHTRNG(3) = 0
      ILAYOL = -1
      REPEAT
        IPCO = IPCO + HLDCO
        ILAY = IWRK(IPCO   )
        IF ILAY.NE.ILAYOL
        THEN
         JRING = IWRK(IPCO+12)
         NHTRNG(JRING) = NHTRNG(JRING) + 1
         ILAYOL = ILAY
        CIF
      UNTIL IPCO.GE.HPCO9-HLDCO
C
C      PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C      PRINT 3001, (NHTRNG(IR),IR=1,3)
C
C
N     1. PARABOLA FIT
N     LAST RING INCLUDED IN FIT
      JRINGL = 3
      PERFORM FPARA0
C
N     RELABEL HITS
      ALBLM1 = 0.6
      ALBLM2 = 3.0
      PERFORM LABEL
C     PRINT 2005, NHFIT,(WRK(I),I=HPCO0,HPCO9)
C

      REPEAT
N       REFIT PARABOLA
        PERFORM FPARA0
C
N       RELABEL HITS
        PERFORM LABEL
C
N       REPEAT FIT IF >3 NEW GOOD HITS
      UNTIL NHGOOD-NHFIT .LT.4
C
C     PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N     SET UP FIT-BANK
      IF SIG.LT.1.
      THEN
        PERFORM FITBNK
      CIF
C
N     CHECK IF BAD FIT AND LOW MOMENTUM
      IF ABS(PAR1).GT..00030 .AND. NHTRNG(1)+NHTRNG(2).GT.16
      THEN
        ALBLM1 = 1.5
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 2
        PERFORM FPARA0
        ALBLM1 = 0.6
        PERFORM LABEL
        PERFORM FPARA0
        PERFORM LABEL
        IF SIG.LT..10
        THEN
          PERFORM FITBK1
          IWRK(IP+ 4) = 32
        CIF
      CIF
      IF ABS(PAR1).GT..00150 .AND. NHTRNG(1)+NHTRNG(2).GT.9
      THEN
        ALBLM1 = 1.5
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 1
        NHTFIT = NHTRNG(1)
        IF NHTFIT.LE.5
        THEN
          NHTFIT = NHTFIT + NHTRNG(2)
          JRINGL = 2
        CIF
        IF NHTFIT.GT.9
        THEN
          PERFORM FPARA0
          ALBLM1 = 0.6
          PERFORM LABEL
          PERFORM FPARA0
          PERFORM LABEL
          IF SIG.LT..10
          THEN
            PERFORM FITBK1
            IWRK(IP+ 4) = 48
          CIF
        CIF
      CIF
C
C     PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
      HPFREE = HPFRE1
      RETURN
C
N     *************************
N     *      F P A R A 0      *
N     *************************
C
C
N     PARABOLA FIT THROUG ORIGIN
      PROC FPARA0
C
N     GET EQUATIONS
N     WEIGHT ORIGIN AS POINT OF PARABOLA
      S0 = WGHT0
      S1 = X0*WGHT0
      S2 = S1*X0
      S3 = S2*X0
      S4 = S3*X0
      S7 = Y0 * WGHT0
      S6 = S7*X0
      S5 = S6*X0
      IPCO = HPCO0
      REPEAT
       IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
       THEN
          X = WRK(IPCO+3)
          Y = WRK(IPCO+4)
          X2 = X**2
          S1 = S1 + X
          S2 = S2 + X2
          S3 = S3 + X*X2
          S4 = S4 + X2**2
          S5 = S5 + Y*X2
          S6 = S6 + Y*X
          S7 = S7 + Y
          S0 = S0 + 1.
        CIF
      IPCO = IPCO + HLDCO
      UNTIL IPCO.GT.HPCO9
      IF S0.LT.2.5
      THEN
        SIG = 1000.
      ELSE
C
N       SOLVE EQUATIONS FOR PARABOLA FIT
        F1 = 1. / S4
        XX12 = S3*F1
        XX13 = S2*F1
        YY1  = S5*F1
        XX22 = S2 - S3*XX12
        XX23 = S1 - S3*XX13
        YY2  = S6 - S3*YY1
        XX32 = S1 - S2*XX12
        XX33 = S0 - S2*XX13
        YY3  = S7 - S2*YY1
        IF XX22.GT.XX32
        THEN
          XX23 = XX23 / XX22
          YY2  = YY2  / XX22
          PAR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
          PAR2 = YY2 - XX23*PAR3
        ELSE
          XX33 = XX33 / XX32
          YY3  = YY3  / XX32
          PAR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
          PAR2 = YY3 - XX33*PAR3
        CIF
        PAR1 = YY1 - XX12*PAR2 - XX13*PAR3
        DEG   = S0 - WGHT0 - 2.
        NHFIT = S0 - WGHT0 + .1
C
C
N       CALC. CHISQ + SOLVE L/R AMBIGUITY
        CHISQ = 0.
        DCHIM1 = 0.
        IHITM1 = 0
        XST    = 999999.
        XEN    =-999999.
        IPCO = HPCO0
        REPEAT
         IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
         THEN
            X = WRK(IPCO+3)
            IF(X.LT.XST) XST = X
            IF(X.GT.XEN) XEN = X
            Y = WRK(IPCO+4)
            F = (PAR1 *X + PAR2 )*X + PAR3
            DCHI = Y - F
            WRK(IPCO+13) = DCHI
N           SUM FOR RMS
            CHISQ = CHISQ + DCHI**2
N           KEEP BIGGEST RMS
C           IF ABS(DCHI).GE.DCHIM1
C           THEN
C             DCHIM1 = ABS(DCHI)
C             IHITM1 = IPCO
C           CIF
C     PRINT 2006, IPCO,X,Y,F,DCHI,CHISQ
          CIF
        IPCO = IPCO + HLDCO
        UNTIL IPCO.GT.HPCO9
        SIG    =      CHISQ  / DEG
C     PRINT 2008, ITRK,NHFIT,XST,SIG,DEG,PAR1,PAR2,PAR3,WGHT0,Y0
C
N       SET LIMIT FOR SIGMA
        SIGLM = TRELLM(16)**2
      CIF
C
      CPROC
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
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
      YEN  = (PAR1 *XEN + PAR2 )*XEN + PAR3
N     DIRECTION AT START + END POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
      TGEN = PAR1*XEN*2 + PAR2
      DXEN = 1./SQRT(TGEN**2+1.)
      DYEN = DXEN * TGEN
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
C     CURV =-PAR1 * 2.
      CVZW = TGST**2+1.
      CVST =-PAR1 * 2 / (SQRT(CVZW)*CVZW)
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      IWRK(IP+ 1) = ITRK
      IWRK(IP+ 2) = 32
      IWRK(IP+ 3) = IDAY
      IWRK(IP+ 4) = 16
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2) * TGTH + ZVERT
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)*CSTH
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)*CSTH
      WRK (IP+10) = SNTH
      IWRK(IP+11) = 0
      WRK (IP+12) = XEN *CSROT - YEN *SNROT + XT
      WRK (IP+13) = XEN *SNROT + YEN *CSROT + YT
      WRK (IP+14) = SQRT(WRK(IP+12)**2 + WRK(IP+13)**2) * TGTH + ZVERT
      WRK (IP+15) = (DXEN*CSROT - DYEN*SNROT)*CSTH
      WRK (IP+16) = (DXEN*SNROT + DYEN*CSROT)*CSTH
      WRK (IP+17) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
C     IF(SIG  .LT.0) PRINT 2021,WRK(IP+1),S0,SIG
C2021 FORMAT(' -VE SQRT:',I4,5E13.5)
      WRK (IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CVST
C     IF(SIG11.LT.0) PRINT 2021,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SQRT(SIG*SIG11) * 2.
      WRK (IP+27) = CVST
      WRK (IP+28) = CVST
C     I0 = IP+ 1
C     I9 = IP+48
C     PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
N     *************************
N     *      F I T B K 1      *
N     *************************
C
C
N     CHANGE FIT BANK (1.POINT)
      PROC FITBK1
C
N     START POINT
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
N     DIRECTION AT START POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
C     CURV =-PAR1 * 2.
      CVZW = TGST**2+1.
      CVST =-PAR1 * 2 / (SQRT(CVZW)*CVZW)
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV,
C    ,            XMIN,YMIN
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2) * TGTH + ZVERT
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)
      WRK (IP+10) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
C     IF(SIG  .LT.0) PRINT 2022,WRK(IP+1),S0,SIG
C2022 FORMAT(' -VE SQRT(1):',I4,5E13.5)
      WRK (IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CVST
C     IF(SIG11.LT.0) PRINT 2022,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SQRT(SIG*SIG11) * 2.
      WRK (IP+27) = CVST
C     I0 = IP+ 1
C     I9 = IP+48
C     PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
C
N     *************************
N     *      L A B E L        *
N     *************************
C
C
N     LABEL USED HITS
      PROC LABEL
C
N       PRESET LAST HIT POINTER
        IWL = -999
C
N       PRESET LAST HIT POINTER
        IWL = -999
        NHGOOD = 0
        FOR IP = HPCO0,HPCO9,HLDCO
          IW0 = IWRK(IP)
          X   = WRK(IP+3)
          Y   = WRK(IP+4)
          F   = (PAR1*X + PAR2)*X + PAR3
          DF  = F - Y
N         SELECT CLOSEST HIT
          LBGOOD = 4
          IF(ABS(DF).LT.ALBLM2) LBGOOD = 1
          IF(ABS(DF).LT.ALBLM1) LBGOOD = 0
          IWRK(IP+ 10) = LBGOOD
          IF(LBGOOD.EQ.0) NHGOOD = NHGOOD + 1
          WRK (IP+13) = DF
C
N         CHECK IF 2 HITS FROM SAME WIRE
          IF IWL.EQ.IW0
          THEN
N           SELECT CLOSEST HIT
            IF ABS(DFL).LT.ABS(DF)
            THEN
              IF(IWRK(IP +10).EQ.0) NHGOOD = NHGOOD - 1
              IWRK(IP +10) = 16
            ELSE
              IF(IWRK(IPL+10).EQ.0) NHGOOD = NHGOOD - 1
              IWRK(IPL+10) = 16
            CIF
          CIF
N         STORE LAST POINTERS + DF
          IWL = IW0
          IPL = IP
          DFL = DF
        CFOR
C
      CPROC
C
C
N     *************************
N     *      I N I T          *
N     *************************
C
C
N     INITIALIZE CONSTANTS
      PROC INIT
C
        IQJETC = IBLN('JETC')
        IQHEAD = IBLN('HEAD')
      PRINT 2999
 2999 FORMAT(///,'0***************************************',
     ,         /,' **                                   **',
     ,         /,' **        WHO IS USING THIS          **',
     ,         /,' **                                   **',
     ,         /,' **     SUBROUTINE   REFITV  ???      **',
     ,         /,' **    ==========================     **',
     ,         /,' **                                   **',
     ,         /,' **    PLEASE CONTACT  P. STEFFEN     **',
     ,         /,' **                                   **',
     ,         /,' **                                   **',
     ,         /,' ***************************************',////)
C
        CALL DAY2(DATE)
        IDAY = DATE(1)*1000 + DATE(2)
C
N       MULT. SCATTERING CONSTANTS
        RESMS = .020**2/2. * .16 * (1. + ALOG10(.16) / 9) * 155.45**2
      CPROC
C
      END
      SUBROUTINE REFITV(IPTR,IPJHTL,ERRFAC)
C
C        REFIT TRACK ITRK IN 'PATR'-BANK USING ORIGIN
C        P. STEFFEN                    22/08/80
C
      IMPLICIT INTEGER*2 (H)
      LOGICAL DEADCL
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
C
      INTEGER DATE(5), IDAY /0/
      DIMENSION ITRCLL(6), NCNCK(24), NHTRNG(3)
C
N     JET-CHAMBER AND VERTEX RESOLUTION
      DATA RESJ0 /.200/, RESV0 /.300/
C
N     MASK FOR L/R BIT IN HIT LABEL
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
C
N     MASK FOR TRACKS AT CELL WALL
      INTEGER MKBDCL(3) /Z10,Z20,Z40/
      INTEGER MKDDCL(3) /Z01,Z02,Z04/
C
C     IF(IDATA(IPTR+1).LT. 4) RETURN
C     I0 = IPTR + 1
C     I9 = IPTR + 48
C     PRINT 2001, (IDATA(I1),I1=I0,I9)
C     I0 = IPJHTL*2 + 1
C     I9 = I0 + IDATA(IPJHTL)*2 - 1
C     PRINT 2000, IPJHTL,I0,I9,(HDATA(I1),I1=I0,I9)
C     IPJETC = IDATA(IBLN('JETC'))
C     I0 = IPJETC*2 + 1
C     I9 = I0 + 109
C     PRINT 2000, IPJETC,I0,I9,(HDATA(I1),I1=I0,I9)
C2000 FORMAT('0REFIT:',3I8,/,(20(1X,Z4)))
C2001 FORMAT(1H0,2I3,I8,2(I4,3F6.1,3F6.3),
C    ,     /,14X,I3,4E13.5,F6.2,I3,4E13.5,
C    ,     /,14X,I3,2F8.3,F6.1,I3,10X,6I3,8I6,2X,Z4)
C2002 FORMAT('0FETCH:',2I3,2I5,12F9.5)
C2003 FORMAT('0ROTATION:',12F10.5)
C2004 FORMAT('0CIRC.CENTRE:',2I3, F10.5,2F10.0,F8.1,2F8.1)
C2005 FORMAT('0TRACK:',I6,/,(1X,3I6,4F8.1,I4,F6.2,2I4,F8.3,I6,F8.1))
C2006 FORMAT(1X,I6,5F8.2,F12.1,5F8.2)
C2007 FORMAT(' FETCH:',I3,9F8.4,F10.5,F6.0)
C2008 FORMAT(' FIT:',2I3,F8.2,F5.0,F10.6,F7.3,F5.1,F6.3,F5.1)
C2009 FORMAT(' JHTL:',I8,1X,Z8,3I5)
C2010 FORMAT(' HIT:',I6,12F8.2)
C2011 FORMAT('0ABERR:',10F10.6)
C2012 FORMAT('0ERROR:',10E13.6)
C2014 FORMAT('0FIT-BANK:',5F8.3,5X,5F8.3,5X,F8.5,2F8.1)
C2016 FORMAT('0ITRCLL =',6I8,/,(9X,6F8.3))
C2107 FORMAT(' SIGLM:',10F8.3)
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
        LBINIT = 1
        PERFORM INIT
      CIF
C
N     RESERVE SPACE IN CWORK
      HPFREE = 1
      HPFRE1 = HPFREE
      HPCO0  = HPFREE
      HLDCO  = 14
      HPFREE = HLDCO*100 + HPCO0
      HPCO9 = HPFREE - 1
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
C
N     COPY TRACK BANK
      HPTR0 = HPFREE
      CALL MVC(IWRK(HPTR0),0,IDATA(IPTR+1),0,192)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
N     CENTRE OF CIRCLE (USED FOR ANGULAR CORRECTION)
      IF IDATA(IPTR+18).EQ.1
      THEN
N       CIRCLE PARAMETERS
        ALFA  = ADATA(IPTR+21)
        CRV   = ADATA(IPTR+19)
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        RAD   =  1./CRV + ADATA(IPTR+20)
        XCIRC = COS(ALFA) * RAD
        YCIRC = SIN(ALFA) * RAD
        CHARGE = SIGN(1.,ADATA(IPTR+25))
      ELSE
N       PARABOLA PARAMETERS
        CRV   = ADATA(IPTR+22)*2.
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        ALFA  = ADATA(IPTR+19)
        XCIRC =-SIN(ALFA)/CRV + ADATA(IPTR+20)
        YCIRC = COS(ALFA)/CRV + ADATA(IPTR+21)
        CHARGE =-SIGN(1.,ADATA(IPTR+22))
      CIF
C
N     ZVERT, THETA + DIR. COSINES
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTHI = SQRT(TGTH**2 + 1.)
      CSTH  = 1. / CSTHI
      SNTH  = CSTH * TGTH
C     PRINT 2004,ITRK,IDATA(IPTR+18),ALFA,XCIRC,YCIRC,ZVERT,TGTH,CSTHI
C
N     GET X-Y-VERTEX AND DETERMINE ERROR
      IPV    = ICALIB(10)
      XO     = ACALIB(IPV+ 1)
      YO     = ACALIB(IPV+ 3)
C     I0 = IPV + 1
C     I9 = IPV + 6
C     PRINT 2029, XO,YO,(ACALIB(I1),I1=I0,I9)
      PTRANS = ABS(0.0299792458*BKGAUS/ADATA(IPTR+25)) * .001
      RESV   = RESV0**2 + RESMS / PTRANS**2
      WGHT0  = RESJ0**2 / RESV
      F1     = ERRFAC
      IF(F1 .LT. .10) F1 = .10
      WGHT0  = WGHT0 / F1**2
C     PRINT 2029, XO,YO,WGHT0,F1,RESV,RESMS,PTRANS
C2029 FORMAT(' VERTEX',9E13.5)
C     PRINT 2011,ABERR
C
N     ROTATION ANGLE (USING LAST POINT OF TRACK)
      XT    = (ADATA(IPTR+12) + XO) * .5
      YT    = (ADATA(IPTR+13) + YO) * .5
      XX    =  ADATA(IPTR+12) - XO
      YY    =  ADATA(IPTR+13) - YO
      RR    = SQRT(XX**2+YY**2)
      CSROT = XX / RR
      SNROT = YY / RR
      XOT = XO - XT
      YOT = YO - YT
      X0   = XOT*CSROT + YOT*SNROT
      Y0   =-XOT*SNROT + YOT*CSROT
      XOR  =- XT*CSROT -  YT*SNROT
      YOR  =  XT*SNROT -  YT*CSROT
C     PRINT 2003, CSROT,SNROT,XX,YY,XT,YT,X0,Y0,XO,YO,WGHT0
C
N     FILL CELL ARRAY
      PERFORM SELCLL
C
N     LOOP OVER ALL CELLS + FETCH HITS
      KCLL = 0
      NHIT = 0
      IPCO = HPCO0
C
N     LOOP OVER RINGS
      JRING = 0
N     TRACKS AT CELL WALLS
      LBCELL = 0
      REPEAT
      JRING = JRING + 1
        NHTRNG(JRING) = 0
        NHRNG = 0
        NCLL = 0
        REPEAT
        NCLL = NCLL + 1
        KCLL = KCLL + 1
          JCELL = ITRCLL(KCLL)
          IF JCELL.NE.0
          THEN
            PERFORM FETCH
            NHRNG = NHRNG + JHIT
          CIF
        UNTIL NCLL.EQ.2
N       SET LABEL FOR TRACK AT CELL BOUND.
        IF(JCELL.NE.0) LBCELL = LOR(MKBDCL(JRING),LBCELL)
      UNTIL KCLL.EQ.6
      HPCO9 = IPCO - 1
C     PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N     1. PARABOLA FIT
N     LAST RING INCLUDED IN FIT
      JRINGL = 3
      PERFORM FPARA0
C
N     RELABEL HITS
      ALBLM1 = 0.6
      ALBLM2 = 3.0
      PERFORM LABEL
C     PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N     REFIT PARABOLA
      PERFORM FPARA0
C
N     RELABEL HITS
      PERFORM LABEL
C     PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N     SET UP FIT-BANK
      IF SIG.LT.1.
      THEN
        PERFORM FITBNK
      CIF
C
N     CHECK IF BAD FIT AND LOW MOMENTUM
      IF ABS(PAR1).GT..00030 .AND. NHTRNG(1)+NHTRNG(2).GT.16
      THEN
        ALBLM1 = 1.5
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 2
        PERFORM FPARA0
        ALBLM1 = 0.6
        PERFORM LABEL
        PERFORM FPARA0
        PERFORM LABEL
        IF SIG.LT..10
        THEN
          PERFORM FITBK1
          IWRK(IP+ 4) = 32
        CIF
      CIF
      IF ABS(PAR1).GT..00150 .AND. NHTRNG(1)+NHTRNG(2).GT.9
      THEN
        ALBLM1 = 1.5
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 1
        NHTFIT = NHTRNG(1)
        IF NHTFIT.LE.5
        THEN
          NHTFIT = NHTFIT + NHTRNG(2)
          JRINGL = 2
        CIF
        IF NHTFIT.GT.9
        THEN
          PERFORM FPARA0
          ALBLM1 = 0.6
          PERFORM LABEL
          PERFORM FPARA0
          PERFORM LABEL
          IF SIG.LT..10
          THEN
            PERFORM FITBK1
            IWRK(IP+ 4) = 48
          CIF
        CIF
      CIF
C
      HPFREE = HPFRE1
      RETURN
C
N     *************************
N     *      F P A R A 0      *
N     *************************
C
C
N     PARABOLA FIT THROUG ORIGIN
      PROC FPARA0
C
N     GET EQUATIONS
N     WEIGHT ORIGIN AS POINT OF PARABOLA
      S0 = WGHT0
      S1 = X0*WGHT0
      S2 = S1*X0
      S3 = S2*X0
      S4 = S3*X0
      S7 = Y0 * WGHT0
      S6 = S7*X0
      S5 = S6*X0
      IPCO = HPCO0
      REPEAT
       IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
       THEN
          X = WRK(IPCO+3)
          Y = WRK(IPCO+4)
          X2 = X**2
          S1 = S1 + X
          S2 = S2 + X2
          S3 = S3 + X*X2
          S4 = S4 + X2**2
          S5 = S5 + Y*X2
          S6 = S6 + Y*X
          S7 = S7 + Y
          S0 = S0 + 1.
        CIF
      IPCO = IPCO + HLDCO
      UNTIL IPCO.GT.HPCO9
      IF S0.LT.2.5
      THEN
        SIG = 1000.
      ELSE
C
N       SOLVE EQUATIONS FOR PARABOLA FIT
        F1 = 1. / S4
        XX12 = S3*F1
        XX13 = S2*F1
        YY1  = S5*F1
        XX22 = S2 - S3*XX12
        XX23 = S1 - S3*XX13
        YY2  = S6 - S3*YY1
        XX32 = S1 - S2*XX12
        XX33 = S0 - S2*XX13
        YY3  = S7 - S2*YY1
        IF XX22.GT.XX32
        THEN
          XX23 = XX23 / XX22
          YY2  = YY2  / XX22
          PAR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
          PAR2 = YY2 - XX23*PAR3
        ELSE
          XX33 = XX33 / XX32
          YY3  = YY3  / XX32
          PAR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
          PAR2 = YY3 - XX33*PAR3
        CIF
        PAR1 = YY1 - XX12*PAR2 - XX13*PAR3
        DEG = S0 - WGHT0 - 2.
C
C
N       CALC. CHISQ + SOLVE L/R AMBIGUITY
        CHISQ = 0.
        DCHIM1 = 0.
        IHITM1 = 0
        XST = 999999.
        XEN =-999999.
        IPCO = HPCO0
        REPEAT
         IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
         THEN
            X = WRK(IPCO+3)
            IF(X.LT.XST) XST = X
            IF(X.GT.XEN) XEN = X
            Y = WRK(IPCO+4)
            F = (PAR1 *X + PAR2 )*X + PAR3
            DCHI = Y - F
            WRK(IPCO+13) = DCHI
N           SUM FOR RMS
            CHISQ = CHISQ + DCHI**2
N           KEEP BIGGEST RMS
C           IF ABS(DCHI).GE.DCHIM1
C           THEN
C             DCHIM1 = ABS(DCHI)
C             IHITM1 = IPCO
C           CIF
C     PRINT 2006, IPCO,X,Y,F,DCHI,CHISQ
          CIF
        IPCO = IPCO + HLDCO
        UNTIL IPCO.GT.HPCO9
        SIG    =      CHISQ  / DEG
C     PRINT 2008, JRINGL,IWRK(IHEND),SIG,DEG,PAR1,PAR2,PAR3,WGHT0,Y0
C
N       SET LIMIT FOR SIGMA
        SIGLM = TRELLM(16)**2
      CIF
C
      CPROC
C
N     *************************
N     *      S E L C L L      *
N     *************************
C
C
N     SELECT CELLS CONTAINING TRACK
      PROC SELCLL
C
        FOR I=1,6
          ITRCLL(I) = 0
        CFOR
        IPC0 = IPTR + 34
        IPC9 = IPC0 +  5
        ICELL = 0
        FOR IPC = IPC0,IPC9
          JCELL = IDATA(IPC)
          IF JCELL.GT. 0 .AND. JCELL.LE.96
          THEN
            JRING = 1
            IF(JCELL.GT.24) JRING = 2
            IF(JCELL.GT.48) JRING = 3
            JPC = JRING*2 - 1
            IF ITRCLL(JPC).EQ.0
            THEN
              ITRCLL(JPC) = JCELL
            ELSE
              IF(ITRCLL(JPC).NE.JCELL) ITRCLL(JPC+1) = JCELL
            CIF
            ICELL = JCELL
            IRING = JRING
          CIF
        CFOR
C
C     PRINT 2016, ITRCLL
      CPROC
C
N     *************************
N     *      F E T C H        *
N     *************************
C
C
N     FETCH HITS IN CELL
      PROC FETCH
C
N       DIR. OF SENSEW. + DRIFTSP.
        IF JRING.NE.3
        THEN
          IC1 = JCELL
          IF(IC1.GT.24) IC1 = IC1 - 24
          CSROT0 = DIRWR1(IC1,1)
          SNROT0 = DIRWR1(IC1,2)
        ELSE
          IC1 = JCELL - 48
          CSROT0 = DIRWR3(IC1,1)
          SNROT0 = DIRWR3(IC1,2)
        CIF
        DRICS  = TRMATC(JCELL,2)
        DRISN  = TRMATS(JCELL,2)
        DRITG  = DRISN/DRICS
        DRISNF = DRISN * .05
C
N       LOAD RADIUS AND WIRE SPACING
        R0 = FSENSW(JRING)
        DR = RINCR (JRING)
C
N       ANGLE OF TRACK IN RING
        R1   = DR*7.5 + R0
        DX   = R1 * CSROT0 - XCIRC
        DY   = R1 * SNROT0 - YCIRC
        RR   = SQRT(DX**2 + DY**2) * CHARGE
        CSB  = DX / RR
        SNB  = DY / RR
        TGB  = CSB/SNB
C
N       SET DRIFT SPACE BIN
        DSBIN1 = DRIVEL(JCELL,1)
        IF(NRUN.GT.100) DS0 = T0FIX(JRING)*DSBIN1*64.
        IF(NRUN.LE.100) DS0 = DSBIN1*.5
N       ANGLE(TRACK,DRIFT DIRECT.)
        TANBET = ABS((TGB-DRITG)/(TGB*DRITG+1.))
C     PRINT 2007, JCELL,CSROT0,SNROT0,DRICS,DRISN,CSB,SNB,CHARGE,TANBET,
C    ,            DSBIN1,DS0
N       CORRECTION CONSTANTS FOR JCELL
C
        IPJCOR = ICALIB(5) + JCELL
        CCST01 = ACALIB(IPJCOR    ) * TANBET
        CCST02 = ACALIB(IPJCOR+ 96) * TANBET
        CCST11 = ACALIB(IPJCOR+192)
        CCST12 = ACALIB(IPJCOR+288)
        CCST21 = ACALIB(IPJCOR+384)
        CCST22 = ACALIB(IPJCOR+480)
        CCST51 = ACALIB(IPJCOR+576) * 10.
        CCST52 = ACALIB(IPJCOR+672) / 121.15
        CCST61 = ACALIB(IPJCOR+768) * 10.
        CCST62 = ACALIB(IPJCOR+864) / 121.15
C     PRINT 2002, JRING,JCELL,IP,IPJCOR,CCST01,CCST02,CCST11,CCST12,
C    ,            CCST21,CCST22,CCST51,CCST52,CCST61,CCST62
N       COUNTER FOR NUMBER OF HITS FOUND
        JHIT = 0
        NHIT   = 0
        NHGOOD = 0
N       PRESET LAST LAYER
        ILAYL =-99
N       LOOP OVER ALL HITS OF CELL
        IPCO = IPCO - HLDCO
        IPJETC = IDATA(IQJETC)*2
        IP0    = IPJETC + 100
        IPCLL  = IPJETC + 2 + JCELL
        IP     = HDATA(IPCLL  ) + IP0
        IP9    = HDATA(IPCLL+1) + IP0
        IPHL   = IPJHTL + 2 + HDATA(IPCLL)/4
C     PRINT 2002, JRING,JCELL,IP,IP9,TGB,SNB,CSB,DRISN,DRICS
        WHILE IP.LT.IP9
C
N         CHECK TRACK # OF HIT LABEL
          LB   = IDATA(IPHL)
          ITR1 = LAND(SHFTR(LB,17),127)
          ITR2 = LAND(SHFTR(LB, 1),127)
C     PRINT 2009, IPHL,LB,ITR1,ITR2,ITRK
          IF ITR1.EQ.ITRK .OR. ITR2.EQ.ITRK
          THEN
C
N           L/R FROM HIT LABEL
            LBLR = 0
            IF(ITR1.EQ.ITRK) LBLR = LAND(LB,MKLRT1)
            IF(ITR2.EQ.ITRK) LBLR = LAND(LB,MKLRT2)
            LBSIDE =-1
            IF(LBLR.NE.0) LBSIDE = 1
            LBLR = LBSIDE
C
            IWIR = HDATA(IP)
            IWIR = SHFTR(IWIR,3)
N           LAYER NUMBER WITHIN RING 3
            ILAY = LAND(IWIR,15)
N           AMPLITUDES
            IAMPL = HDATA(IP+1)
            IAMPR = HDATA(IP+2)
N           DRIFT SPACE
            DS =(HDATA(IP+3)) * DSBIN1
C     DATA NPRHT /0/
C     NPRHT = NPRHT + 1
C     IF(NPRHT.LE.25) PRINT 2019, IWIR,ILAY,JCELL,HDATA(IP+3),DS,DSBIN1
C2019 FORMAT(' HIT ',4I6,F6.1)
            X1   = ILAY * DR + R0
            Z1   = X1*TGTH + ZVERT
N           CORRECTION FOR TOF + PROPAG. ALONG WIRE
            DDS = (1222.9-ABS(Z1))*ABERR(1) + ABERR(6)*R1*CSTHI
            DSC = DS - DDS + DS0
            Y1   = SWDEPL
            IF(LAND(ILAY,1).NE.0) Y1 =-Y1
            Y1   = (7-ILAY)*(CCST52*Z1+CCST51) - CCST62*Z1-CCST61 + Y1
            X    = X1*CSROT0 - Y1*SNROT0
            Y    = X1*SNROT0 + Y1*CSROT0
            IF DS.LE.DRC
            THEN
              IF DS.LT.4.0
              THEN
                IF DS.GT.DSD1
                THEN
                  DSC = (DSD1-DSD0)*DRV0 + (DS-DSD1)*DRV1
                ELSE
                  DSC = (DS-DSD0)*DRV0
                CIF
                IF(DSC.LT.0.1) DSC = 0.1
              CIF
              DXR  = DSC * CSB
              DYR  = DSC * SNB
              DXL =-DXR
              DYL =-DYR
            ELSE
C
N             EDGE WIRE FIELD DISTORTION
              IF ILAY.LT. 3
              THEN
                DILAY =-(ILAY- 3)**2
                DSCL  = (DILAY*CCST11 + 1.) * DSC
                DSCR  = (DILAY*CCST12 + 1.) * DSC
              ELSE
              IF ILAY.GT.12
              THEN
                DILAY =-(ILAY-12)**2
                DSCL  = (DILAY*CCST21 + 1.) * DSC
                DSCR  = (DILAY*CCST22 + 1.) * DSC
              ELSE
                DSCL = DSC
                DSCR = DSC
              CIF
              CIF
C
N             FIELD DISTORTIONS AT LARGE DRIFT TIMES
              IF DSC.GT.ABERR(7)
              THEN
                DWIR  = ILAY - 7.5
                DWIRC = DSC*DRISNF
                DWIRL = DWIR + DWIRC
                DWIRR = DWIR - DWIRC
                DSCL  = (DSCL-ABERR(7))*DWIRL*CCST01 + DSCL
                DSCR  =-(DSCR-ABERR(7))*DWIRR*CCST02 + DSCR
              CIF
              DXR  = (DSCR-DRC)*DRISN + DRC*CSB
              DYR  = (DSCR-DRC)*DRICS + DRC*SNB
              DXL  =-(DSCL-DRC)*DRISN - DRC*CSB
              DYL  =-(DSCL-DRC)*DRICS - DRC*SNB
            CIF
C     PRINT 2010, ILAY,DS,DSC,DSCL,DSCR,XL,XR,X,Y,DXL,DXR,DYL,DYR
            XL   = DXL + X - XT
            YL   = DYL + Y - YT
            XXL  = XL*CSROT + YL*SNROT
            YYL  =-XL*SNROT + YL*CSROT
            XR   = DXR + X - XT
            YR   = DYR + Y - YT
            XXR  = XR*CSROT + YR*SNROT
            YYR  =-XR*SNROT + YR*CSROT
C
N           CALCULATE Z COORDINATE
            IF IAMPR.LE.0.OR.IAMPL.LE.0
            THEN
              ZZ     = 0.
              LZGOOD = 16
            ELSE
              ZZ = IAMPR + IAMPL
              ZZ = FLOAT(IAMPR-IAMPL) * ZAL*.5 / ZZ
              LZGOOD = 0
              IF(ABS(ZZ).GT.1250.) LZGOOD = 16
            CIF
N           SET ARRAY
C     PRINT 2010, ILAY,DS,XXL,YYL,X1,Z1,XXR,YYR,Y1
C
N           CHECK IF LEFT + RIGHT SOLUTION POSSIBLE
            NLRSOL = 1
            IF(DS.LT.2.0) NLRSOL = 2
C
N           LOOP OVER LEFT +/OR RIGHT SOLUTION
            ILRSOL = 0
            REPEAT
            ILRSOL = ILRSOL + 1
C
N             SELECT SIDE
              IF NLRSOL.EQ.1 .AND. LBSIDE.LT.0  .OR.
     ?           NLRSOL.EQ.2 .AND. ILRSOL.EQ.1
              THEN
N               LEFT SIDE
                LBSIDE =-1
                XX  = XXL
                YY  = YYL
              ELSE
N               RIGHT SIDE
                LBSIDE = 1
                XX  = XXR
                YY  = YYR
              CIF
C
N             HIT QUALITY:
              LBGOOD = 0
              IF(LBSIDE.NE.LBLR) LBGOOD = 1
N             NEW LAYER?
              IF ILAY.NE.ILAYL .OR. LBGDL.LE.1.AND.LBGOOD.LE.1
              THEN
                LBREG = 1
N               INCREASE HIT COUNTER
                JHIT = JHIT + 1
                IPCO = IPCO + HLDCO
              ELSE
N               2 HITS IN SAME LAYER, SELECT CLOSEST
                LBREG = 0
                IF(LBGOOD.LT.IWRK(IPCO+10)) LBREG = 1
              CIF
N             REGISTER NEW HIT?
              IF LBREG.NE.0
              THEN
                NHIT   = NHIT   + 1
                IF(LBGOOD.LE.1) NHGOOD = NHGOOD + 1
                IWRK(IPCO   ) = ILAY
                IWRK(IPCO+ 1) = IP
                IWRK(IPCO+ 2) = LBSIDE
                WRK (IPCO+ 3) = XX
                WRK (IPCO+ 4) = YY
                WRK (IPCO+ 5) = ZZ
                WRK (IPCO+ 6) = XX - X0
                IWRK(IPCO+ 7) = LZGOOD
                WRK (IPCO+ 8) = DS
                IWRK(IPCO+ 9) = JCELL
                IWRK(IPCO+10) = LBGOOD
                WRK (IPCO+11) = TGB
                IWRK(IPCO+12) = JRING
                WRK (IPCO+13) = 0.
                ILAYL = ILAY
                LBGDL = LBGOOD
                NHTRNG(JRING) = NHTRNG(JRING) + 1
              CIF
C
            UNTIL ILRSOL.GE.NLRSOL
C
          CIF
C
        IPHL = IPHL + 1
        IP   = IP   + 4
        CWHILE
N       SET IPCO TO 1. FREE LOCATION
        IPCO = IPCO + HLDCO
C
N     MASK FOR TRACKS AT CELL WALL + IN DEAD CELLS
C
N       SET LABEL FOR DEAD CELL
        IF NHIT.LE.2
        THEN
          IF DEADCL(JCELL,NRUN)
          THEN
            LBCELL = LOR(LBCELL,MKDDCL(JRING))
            JHIT = 16
            NHIT = 16
C     PRINT 2019, JCELL,JRING,NRUN,LBCELL
          CIF
        CIF
C
      CPROC
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
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
      YEN  = (PAR1 *XEN + PAR2 )*XEN + PAR3
N     DIRECTION AT START + END POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
      TGEN = PAR1*XEN*2 + PAR2
      DXEN = 1./SQRT(TGEN**2+1.)
      DYEN = DXEN * TGEN
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
      CURV =-PAR1 * 2.
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      IWRK(IP+ 1) = ITRK
      IWRK(IP+ 2) = 32
      IWRK(IP+ 3) = IDAY
      IWRK(IP+ 4) = 16
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2) * TGTH + ZVERT
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)*CSTH
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)*CSTH
      WRK (IP+10) = SNTH
      IWRK(IP+11) = 0
      WRK (IP+12) = XEN *CSROT - YEN *SNROT + XT
      WRK (IP+13) = XEN *SNROT + YEN *CSROT + YT
      WRK (IP+14) = SQRT(WRK(IP+12)**2 + WRK(IP+13)**2) * TGTH + ZVERT
      WRK (IP+15) = (DXEN*CSROT - DYEN*SNROT)*CSTH
      WRK (IP+16) = (DXEN*SNROT + DYEN*CSROT)*CSTH
      WRK (IP+17) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
C     IF(SIG  .LT.0) PRINT 2021,WRK(IP+1),S0,SIG
C2021 FORMAT(' -VE SQRT:',I4,5E13.5)
      WRK (IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CURV
C     IF(SIG11.LT.0) PRINT 2021,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SQRT(SIG*SIG11) * 2.
      WRK (IP+27) = CURV
      WRK (IP+28) = CURV
C     I0 = IP+ 1
C     I9 = IP+48
C     PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
N     *************************
N     *      F I T B K 1      *
N     *************************
C
C
N     CHANGE FIT BANK (1.POINT)
      PROC FITBK1
C
N     START POINT
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
N     DIRECTION AT START POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
      CURV =-PAR1 * 2.
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV,
C    ,            XMIN,YMIN
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2) * TGTH + ZVERT
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)*CSTH
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)*CSTH
      WRK (IP+10) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
C     IF(SIG  .LT.0) PRINT 2022,WRK(IP+1),S0,SIG
C2022 FORMAT(' -VE SQRT(1):',I4,5E13.5)
      WRK (IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CURV
C     IF(SIG11.LT.0) PRINT 2022,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SQRT(SIG*SIG11) * 2.
      WRK (IP+27) = CURV
C     I0 = IP+ 1
C     I9 = IP+48
C     PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
C
N     *************************
N     *      L A B E L        *
N     *************************
C
C
N     LABEL USED HITS
      PROC LABEL
C
N       PRESET LAST HIT POINTER
        IWL = -999
        FOR IP = HPCO0,HPCO9,HLDCO
          IW0 = IWRK(IP)
          X   = WRK(IP+3)
          Y   = WRK(IP+4)
          F   = (PAR1*X + PAR2)*X + PAR3
          DF  = F - Y
N         SELECT CLOSEST HIT
          LBGOOD = 4
          IF(ABS(DF).LT.ALBLM2) LBGOOD = 1
          IF(ABS(DF).LT.ALBLM1) LBGOOD = 0
          IWRK(IP+ 10) = LBGOOD
          WRK (IP+13) = DF
C
N         CHECK IF 2 HITS FROM SAME WIRE
          IF IWL.EQ.IW0
          THEN
N           SELECT CLOSEST HIT
            IF ABS(DFL).LT.ABS(DF)
            THEN
              IWRK(IP +10) = 16
            ELSE
              IWRK(IPL+10) = 16
            CIF
          CIF
N         STORE LAST POINTERS + DF
          IWL = IW0
          IPL = IP
          DFL = DF
        CFOR
C
      CPROC
C
C
N     *************************
N     *      I N I T          *
N     *************************
C
C
N     INITIALIZE CONSTANTS
      PROC INIT
C
        IQJETC = IBLN('JETC')
        IQHEAD = IBLN('HEAD')
C
        CALL DAY2(DATE)
        IDAY = DATE(1)*1000 + DATE(2)
C
N       MULT. SCATTERING CONSTANTS
        RESMS = .020**2/2. * .16 * (1. + ALOG10(.16) / 9) * 155.45**2
C
N       RADIUS AROUND WIRE FOR CORR. OF DRIFTSPACE
        DRC = RINCR(1)*.5 * DRICOS
C       CONST. FOR VAR. OF DRIFT VEL.
        IPHEAD = IDATA(IQHEAD)*2
        NRUN = HDATA(IPHEAD+10)
        IF NRUN.LE.100
        THEN
          DSD0   = .0
          DSD1   = 5.0
          DSD2   = 5.0
          DRV0   = 1.0
          DRV1   = 1.0
        ELSE
          DSD0   =-.63
          DSD1   = 1.8
          DSD2   = 4.0
          DRV0   = 0.8
          DRV1   = (DSD2 - (DSD1-DSD0)*DRV0) / (DSD2-DSD1)
        CIF
      CPROC
C
      END
      SUBROUTINE REFIT(IPTR,IPJHTL)
C
C        REFIT TRACK ITRK IN 'PATR'-BANK
C        P. STEFFEN                    80/08/19
C
      IMPLICIT INTEGER*2 (H)
      LOGICAL DEADCL
C
#include "cdata.for"
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
#include "cjdrch.for"
#include "cdsmax.for"
C
      INTEGER DATE(5), IDAY /0/
      DIMENSION ITRCLL(6), NCNCK(24), NHTRNG(3)
C
N     MASK FOR L/R BIT IN HIT LABEL
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
C
N     MASK FOR TRACKS AT CELL WALL
      INTEGER MKBDCL(3) /Z10,Z20,Z40/
      INTEGER MKDDCL(3) /Z01,Z02,Z04/
C
C     IF(IDATA(IPTR+1).LT. 4) RETURN
C     I0 = IPTR + 1
C     I9 = IPTR + 48
C     PRINT 2001, (IDATA(I1),I1=I0,I9)
C     I0 = IPJHTL*2 + 1
C     I9 = I0 + IDATA(IPJHTL)*2 - 1
C     PRINT 2000, IPJHTL,I0,I9,(HDATA(I1),I1=I0,I9)
C     IPJETC = IDATA(IBLN('JETC'))
C     I0 = IPJETC*2 + 1
C     I9 = I0 + 109
C     PRINT 2000, IPJETC,I0,I9,(HDATA(I1),I1=I0,I9)
C2000 FORMAT('0REFIT:',3I8,/,(20(1X,Z4)))
C2001 FORMAT(1H0,2I3,I8,2(I4,3F6.1,3F6.3),
C    ,     /,14X,I3,4E13.5,F6.2,I3,4E13.5,
C    ,     /,14X,I3,2F8.3,F6.1,I3,10X,6I3,8I6,2X,Z4)
C2002 FORMAT('0FETCH:',2I3,2I5,12F9.5)
C2003 FORMAT('0ROTATION:',12F10.5)
C2004 FORMAT('0CIRC.CENTRE:',2I3, F10.5,2F10.0,F8.1,2F8.1)
C2005 FORMAT('0TRACK:',I6,/,(1X,3I6,4F8.1,I4,F6.2,2I4,F8.3,I6,F8.1))
C2006 FORMAT(1X,I6,5F8.2,F12.1,5F8.2)
C2007 FORMAT(' FETCH:',I3,9F8.4,F10.5,F6.0)
C2008 FORMAT(' FIT:',2I3,F8.2,F5.0,F10.6,F7.3,F5.1,F6.3,F5.1)
C2009 FORMAT(' JHTL:',I8,1X,Z8,3I5)
C2010 FORMAT(' HIT:',I6,12F8.2)
C2011 FORMAT('0ABERR:',10F10.6)
C2012 FORMAT('0ERROR:',10E13.6)
C2014 FORMAT('0FIT-BANK:',5F8.3,5X,5F8.3,5X,F8.5,2F8.1)
C2016 FORMAT('0ITRCLL =',6I8,/,(9X,6F8.3))
C
N     INITIALIZATION
      DATA LBINIT /0/
      IF LBINIT .EQ. 0
      THEN
        LBINIT = 1
        PERFORM INIT
      CIF
C
N     RESERVE SPACE IN CWORK
      HPFREE = 1
      HPFRE1 = HPFREE
      HPCO0  = HPFREE
      HLDCO  = 14
      HPFREE = HLDCO*100 + HPCO0
      HPCO9 = HPFREE - 1
C
N     GET RUN #
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
C
N     COPY TRACK BANK
      HPTR0 = HPFREE
      CALL MVC(IWRK(HPTR0),0,IDATA(IPTR+1),0,192)
C
N     TRACK #
      ITRK = IDATA(IPTR+1)
C
N     CENTRE OF CIRCLE (USED FOR ANGULAR CORRECTION)
      IF IDATA(IPTR+18).EQ.1
      THEN
N       CIRCLE PARAMETERS
        ALFA  = ADATA(IPTR+21)
        CRV   = ADATA(IPTR+19)
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        RAD   =  1./CRV + ADATA(IPTR+20)
        XCIRC = COS(ALFA) * RAD
        YCIRC = SIN(ALFA) * RAD
        CHARGE = SIGN(1.,ADATA(IPTR+25))
      ELSE
N       PARABOLA PARAMETERS
        CRV   = ADATA(IPTR+22)*2.
        IF(ABS(CRV).LT.1.E-8) CRV = SIGN(1.E-8,CRV)
        ALFA  = ADATA(IPTR+19)
        XCIRC =-SIN(ALFA)/CRV + ADATA(IPTR+20)
        YCIRC = COS(ALFA)/CRV + ADATA(IPTR+21)
        CHARGE =-SIGN(1.,ADATA(IPTR+22))
      CIF
C
N     ZVERT, THETA + DIR. COSINES
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      CSTHI = SQRT(TGTH**2 + 1.)
      CSTH  = 1. / CSTHI
      SNTH  = CSTH * TGTH
C     PRINT 2004,ITRK,IDATA(IPTR+18),ALFA,XCIRC,YCIRC,ZVERT,TGTH,CSTHI
C
C     PRINT 2029, XO,YO,WGHT0,F1,RESV,RESMS,PTRANS
C2029 FORMAT(' VERTEX',9E13.5)
C     PRINT 2011,ABERR
C
N     ROTATION ANGLE (USING LAST POINT OF TRACK)
      XT    = (ADATA(IPTR+12) + ADATA(IPTR+5)) * .5
      YT    = (ADATA(IPTR+13) + ADATA(IPTR+6)) * .5
      XX    =  ADATA(IPTR+12) - ADATA(IPTR+5)
      YY    =  ADATA(IPTR+13) - ADATA(IPTR+6)
      RR    = SQRT(XX**2+YY**2)
      CSROT = XX / RR
      SNROT = YY / RR
      XOR  =- XT*CSROT -  YT*SNROT
      YOR  =  XT*SNROT -  YT*CSROT
C     PRINT 2003, CSROT,SNROT,XX,YY,XT,YT
C
N     FILL CELL ARRAY
      PERFORM SELCLL
C
N     LOOP OVER ALL CELLS + FETCH HITS
      KCLL = 0
      NHIT = 0
      IPCO = HPCO0
C
N     LOOP OVER RINGS
      JRING = 0
N     TRACKS AT CELL WALLS
      LBCELL = 0
      REPEAT
      JRING = JRING + 1
        NHTRNG(JRING) = 0
        NHRNG = 0
        NCLL = 0
        REPEAT
        NCLL = NCLL + 1
        KCLL = KCLL + 1
          JCELL = ITRCLL(KCLL)
          IF JCELL.NE.0
          THEN
            PERFORM FETCH
            NHRNG = NHRNG + JHIT
          CIF
        UNTIL NCLL.EQ.2
N       SET LABEL FOR TRACK AT CELL BOUND.
        IF(JCELL.NE.0) LBCELL = LOR(MKBDCL(JRING),LBCELL)
      UNTIL KCLL.EQ.6
      HPCO9 = IPCO - 1
C     PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
C
      REPEAT
C
C
C
C
N       1. PARABOLA FIT
N       LAST RING INCLUDED IN FIT
        JRINGL = 3
        PERFORM FPARA0
C
N       RELABEL HITS
        ALBLM1 = 0.6
        ALBLM2 = 3.0
        PERFORM LABEL
C       PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N       REFIT PARABOLA
        PERFORM FPARA0
C
N       RELABEL HITS
        PERFORM LABEL
C       IF(ITRK.EQ.17) PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N       SET UP FIT-BANK
        IF SIG.LT.1.
        THEN
          PERFORM FITBNK
        CIF
C
N       STOP IF SIG < .10
        IF(SIG.LT..10) XREPEAT
C
N       STOP IF HIGH MOMENTUM
        IF(ABS(PAR1).LT..00030) XREPEAT
C
N       STOP IF NOT ENOUGH HITS IN R1 + R2
        IF(NHTRNG(1)+NHTRNG(2).LE.16) XREPEAT
C
N       CONTINUE + FIT ONLY R1 + R2
        ALBLM1 = 2.0
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 2
        PERFORM FPARA0
        ALBLM1 = 1.0
        PERFORM LABEL
        PERFORM FPARA0
        PERFORM LABEL
        IF SIG.LT..20
        THEN
          PERFORM FITBK1
        CIF
C       IF(ITRK.EQ.17) PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
N       STOP IF GOOD FIT
        IF(SIG.LT..20) XREPEAT
C
N       STOP IF NOT LOW MOMENTUM
        IF(ABS(PAR1).LT..00150) XREPEAT
C
N       STOP IF NOT ENOUGH HITS IN R1
        IF(NHTRNG(1).LE.9) XREPEAT
C
N       CONTINUE + FIT IN R1 ONLY
        ALBLM1 = 2.0
        ALBLM2 = 3.0
        PERFORM LABEL
        JRINGL = 1
        NHTFIT = NHTRNG(1)
        IF NHTFIT.LE.5
        THEN
          NHTFIT = NHTFIT + NHTRNG(2)
          JRINGL = 2
        CIF
        IF NHTFIT.GT.9
        THEN
          PERFORM FPARA0
          ALBLM1 = 1.0
          PERFORM LABEL
          PERFORM FPARA0
          PERFORM LABEL
          IF SIG.LT..20
          THEN
            PERFORM FITBK1
          CIF
        CIF
C       IF(ITRK.EQ.17) PRINT 2005, LBCELL,(WRK(I),I=HPCO0,HPCO9)
C
C
      UNTIL .TRUE.
C
C
      HPFREE = HPFRE1
      RETURN
C
N     *************************
N     *      F P A R A 0      *
N     *************************
C
C
N     PARABOLA FIT THROUG ORIGIN
      PROC FPARA0
C
N     GET EQUATIONS
N     WEIGHT ORIGIN AS POINT OF PARABOLA
      S0 = 0.
      S1 = 0.
      S2 = 0.
      S3 = 0.
      S4 = 0.
      S7 = 0.
      S6 = 0.
      S5 = 0.
      IPCO = HPCO0
      REPEAT
       IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
       THEN
          X = WRK(IPCO+3)
          Y = WRK(IPCO+4)
          X2 = X**2
          S1 = S1 + X
          S2 = S2 + X2
          S3 = S3 + X*X2
          S4 = S4 + X2**2
          S5 = S5 + Y*X2
          S6 = S6 + Y*X
          S7 = S7 + Y
          S0 = S0 + 1.
        CIF
      IPCO = IPCO + HLDCO
      UNTIL IPCO.GT.HPCO9
      IF S0.LT.3.5
      THEN
        SIG = 1000.
      ELSE
C
N       SOLVE EQUATIONS FOR PARABOLA FIT
        F1 = 1. / S4
        XX12 = S3*F1
        XX13 = S2*F1
        YY1  = S5*F1
        XX22 = S2 - S3*XX12
        XX23 = S1 - S3*XX13
        YY2  = S6 - S3*YY1
        XX32 = S1 - S2*XX12
        XX33 = S0 - S2*XX13
        YY3  = S7 - S2*YY1
        IF XX22.GT.XX32
        THEN
          XX23 = XX23 / XX22
          YY2  = YY2  / XX22
          PAR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
          PAR2 = YY2 - XX23*PAR3
        ELSE
          XX33 = XX33 / XX32
          YY3  = YY3  / XX32
          PAR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
          PAR2 = YY3 - XX33*PAR3
        CIF
        PAR1 = YY1 - XX12*PAR2 - XX13*PAR3
        DEG = S0 - 3.
C
C
N       CALC. CHISQ + SOLVE L/R AMBIGUITY
        CHISQ = 0.
        DCHIM1 = 0.
        IHITM1 = 0
        IHSTRT = 0
        IPCO = HPCO0
        REPEAT
         IF IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
         THEN
            IF(IHSTRT.EQ.0) IHSTRT = IPCO
            IHEND = IPCO
            X = WRK(IPCO+3)
            Y = WRK(IPCO+4)
            F = (PAR1 *X + PAR2 )*X + PAR3
            DCHI = Y - F
            WRK(IPCO+13) = DCHI
N           SUM FOR RMS
            CHISQ = CHISQ + DCHI**2
N           KEEP BIGGEST RMS
C           IF ABS(DCHI).GE.DCHIM1
C           THEN
C             DCHIM1 = ABS(DCHI)
C             IHITM1 = IPCO
C           CIF
C     PRINT 2006, IPCO,X,Y,F,DCHI,CHISQ
          CIF
        IPCO = IPCO + HLDCO
        UNTIL IPCO.GT.HPCO9
        SIG    =      CHISQ  / DEG
C     PRINT 2008, JRINGL,IWRK(IHEND),SIG,DEG,PAR1,PAR2,PAR3,WGHT0,Y0
C     PRINT 2012, S0,S1,S2,S3,S4,S5,S6,S7
C
      CIF
C
      CPROC
C
N     *************************
N     *      S E L C L L      *
N     *************************
C
C
N     SELECT CELLS CONTAINING TRACK
      PROC SELCLL
C
        FOR I=1,6
          ITRCLL(I) = 0
        CFOR
        IPC0 = IPTR + 34
        IPC9 = IPC0 +  5
        ICELL = 0
        FOR IPC = IPC0,IPC9
          JCELL = IDATA(IPC)
          IF JCELL.GT. 0 .AND. JCELL.LE.96
          THEN
            JRING = 1
            IF(JCELL.GT.24) JRING = 2
            IF(JCELL.GT.48) JRING = 3
            JPC = JRING*2 - 1
            IF ITRCLL(JPC).EQ.0
            THEN
              ITRCLL(JPC) = JCELL
            ELSE
              IF(ITRCLL(JPC).NE.JCELL) ITRCLL(JPC+1) = JCELL
            CIF
            ICELL = JCELL
            IRING = JRING
          CIF
        CFOR
C
C     PRINT 2016, ITRCLL
      CPROC
C
N     *************************
N     *      F E T C H        *
N     *************************
C
C
N     FETCH HITS IN CELL
      PROC FETCH
C
N       DIR. OF SENSEW. + DRIFTSP.
        IF JRING.NE.3
        THEN
          IC1 = JCELL
          IF(IC1.GT.24) IC1 = IC1 - 24
          CSROT0 = DIRWR1(IC1,1)
          SNROT0 = DIRWR1(IC1,2)
        ELSE
          IC1 = JCELL - 48
          CSROT0 = DIRWR3(IC1,1)
          SNROT0 = DIRWR3(IC1,2)
        CIF
        DRICS  = TRMATC(JCELL,2)
        DRISN  = TRMATS(JCELL,2)
        DRITG  = DRISN/DRICS
        DRISNF = DRISN * .05
C
N       LOAD RADIUS AND WIRE SPACING
        R0 = FSENSW(JRING)
        DR = RINCR (JRING)
C
N       ANGLE OF TRACK IN RING
        R1   = DR*7.5 + R0
        DX   = R1 * CSROT0 - XCIRC
        DY   = R1 * SNROT0 - YCIRC
        RR   = SQRT(DX**2 + DY**2) * CHARGE
        CSB  = DX / RR
        SNB  = DY / RR
        TGB  = CSB/SNB
C
N       SET DRIFT SPACE BIN
        DSBIN1 = DRIVEL(JCELL,1)
        IF(NRUN.GT.100) DS0 = T0FIX(JRING)*DSBIN1*64.
        IF(NRUN.LE.100) DS0 = DSBIN1*.5
N       ANGLE(TRACK,DRIFT DIRECT.)
        TANBET = ABS((TGB-DRITG)/(TGB*DRITG+1.))
C     PRINT 2007, JCELL,CSROT0,SNROT0,DRICS,DRISN,CSB,SNB,CHARGE,TANBET,
C    ,            DSBIN1,DS0
N       CORRECTION CONSTANTS FOR JCELL
C
        IPJCOR = ICALIB(5) + JCELL
        CCST01 = ACALIB(IPJCOR    ) * TANBET
        CCST02 = ACALIB(IPJCOR+ 96) * TANBET
        CCST11 = ACALIB(IPJCOR+192)
        CCST12 = ACALIB(IPJCOR+288)
        CCST21 = ACALIB(IPJCOR+384)
        CCST22 = ACALIB(IPJCOR+480)
        CCST51 = ACALIB(IPJCOR+576) * 10.
        CCST52 = ACALIB(IPJCOR+672) / 121.15
        CCST61 = ACALIB(IPJCOR+768) * 10.
        CCST62 = ACALIB(IPJCOR+864) / 121.15
C     PRINT 2002, JRING,JCELL,IP,IPJCOR,CCST01,CCST02,CCST11,CCST12,
C    ,            CCST21,CCST22,CCST51,CCST52,CCST61,CCST62
N       COUNTER FOR NUMBER OF HITS FOUND
        JHIT = 0
        NHIT   = 0
        NHGOOD = 0
N       PRESET LAST LAYER
        ILAYL =-99
N       LOOP OVER ALL HITS OF CELL
        IPCO = IPCO - HLDCO
        IPJETC = IDATA(IQJETC)*2
        IP0    = IPJETC + 100
        IPCLL  = IPJETC + 2 + JCELL
        IP     = HDATA(IPCLL  ) + IP0
        IP9    = HDATA(IPCLL+1) + IP0
        IPHL   = IPJHTL + 2 + HDATA(IPCLL)/4
C     PRINT 2002, JRING,JCELL,IP,IP9,TGB,SNB,CSB,DRISN,DRICS
        WHILE IP.LT.IP9
C
N         CHECK TRACK # OF HIT LABEL
          LB   = IDATA(IPHL)
          ITR1 = LAND(SHFTR(LB,17),127)
          ITR2 = LAND(SHFTR(LB, 1),127)
C     PRINT 2009, IPHL,LB,ITR1,ITR2,ITRK
          IF ITR1.EQ.ITRK .OR. ITR2.EQ.ITRK
          THEN
C
N           L/R FROM HIT LABEL
            LBLR = 0
            IF(ITR1.EQ.ITRK) LBLR = LAND(LB,MKLRT1)
            IF(ITR2.EQ.ITRK) LBLR = LAND(LB,MKLRT2)
            LBSIDE =-1
            IF(LBLR.NE.0) LBSIDE = 1
            LBLR = LBSIDE
C
            IWIR = HDATA(IP)
            IWIR = SHFTR(IWIR,3)
N           LAYER NUMBER WITHIN RING 3
            ILAY = LAND(IWIR,15)
N           AMPLITUDES
            IAMPL = HDATA(IP+1)
            IAMPR = HDATA(IP+2)
N           DRIFT SPACE
            DS =(HDATA(IP+3)) * DSBIN1
C     DATA NPRHT /0/
C     NPRHT = NPRHT + 1
C     IF(NPRHT.LE.25) PRINT 2019, IWIR,ILAY,JCELL,HDATA(IP+3),DS,DSBIN1
C2019 FORMAT(' HIT ',4I6,F6.1)
            X1   = ILAY * DR + R0
            Z1   = X1*TGTH + ZVERT
N           CORRECTION FOR TOF + PROPAG. ALONG WIRE
            DDS = (1222.9-ABS(Z1))*ABERR(1) + ABERR(6)*R1*CSTHI
            DSC = DS - DDS + DS0
            Y1   = SWDEPL
            IF(LAND(ILAY,1).NE.0) Y1 =-Y1
            Y1   = (7-ILAY)*(CCST52*Z1+CCST51) - CCST62*Z1-CCST61 + Y1
            X    = X1*CSROT0 - Y1*SNROT0
            Y    = X1*SNROT0 + Y1*CSROT0
            IF DS.LE.DRC
            THEN
              IF DS.LT.4.0
              THEN
                IF DS.GT.DSD1
                THEN
                  DSC = (DSD1-DSD0)*DRV0 + (DS-DSD1)*DRV1
                ELSE
                  DSC = (DS-DSD0)*DRV0
                CIF
                IF(DSC.LT.0.1) DSC = 0.1
              CIF
              DXR  = DSC * CSB
              DYR  = DSC * SNB
              DXL =-DXR
              DYL =-DYR
            ELSE
C
N             EDGE WIRE FIELD DISTORTION
              IF ILAY.LT. 3
              THEN
                DILAY =-(ILAY- 3)**2
                DSCL  = (DILAY*CCST11 + 1.) * DSC
                DSCR  = (DILAY*CCST12 + 1.) * DSC
              ELSE
              IF ILAY.GT.12
              THEN
                DILAY =-(ILAY-12)**2
                DSCL  = (DILAY*CCST21 + 1.) * DSC
                DSCR  = (DILAY*CCST22 + 1.) * DSC
              ELSE
                DSCL = DSC
                DSCR = DSC
              CIF
              CIF
C
N             FIELD DISTORTIONS AT LARGE DRIFT TIMES
              IF DSC.GT.ABERR(7)
              THEN
                DWIR  = ILAY - 7.5
                DWIRC = DSC*DRISNF
                DWIRL = DWIR + DWIRC
                DWIRR = DWIR - DWIRC
                DSCL  = (DSCL-ABERR(7))*DWIRL*CCST01 + DSCL
                DSCR  =-(DSCR-ABERR(7))*DWIRR*CCST02 + DSCR
              CIF
              DXR  = (DSCR-DRC)*DRISN + DRC*CSB
              DYR  = (DSCR-DRC)*DRICS + DRC*SNB
              DXL  =-(DSCL-DRC)*DRISN - DRC*CSB
              DYL  =-(DSCL-DRC)*DRICS - DRC*SNB
            CIF
C     PRINT 2010, ILAY,DS,DSC,DSCL,DSCR,XL,XR,X,Y,DXL,DXR,DYL,DYR
            XL   = DXL + X - XT
            YL   = DYL + Y - YT
            XXL  = XL*CSROT + YL*SNROT
            YYL  =-XL*SNROT + YL*CSROT
            XR   = DXR + X - XT
            YR   = DYR + Y - YT
            XXR  = XR*CSROT + YR*SNROT
            YYR  =-XR*SNROT + YR*CSROT
C
N           CALCULATE Z COORDINATE
            IF IAMPR.LE.0.OR.IAMPL.LE.0
            THEN
              ZZ     = 0.
              LZGOOD = 16
            ELSE
              ZZ = IAMPR + IAMPL
              ZZ = FLOAT(IAMPR-IAMPL) * ZAL*.5 / ZZ
              LZGOOD = 0
              IF(ABS(ZZ).GT.1250.) LZGOOD = 16
            CIF
N           SET ARRAY
C     PRINT 2010, ILAY,DS,XXL,YYL,X1,Z1,XXR,YYR,Y1
C
N           CHECK IF LEFT + RIGHT SOLUTION POSSIBLE
            NLRSOL = 1
            IF(DS.LT.2.0) NLRSOL = 2
C
N           LOOP OVER LEFT +/OR RIGHT SOLUTION
            ILRSOL = 0
            REPEAT
            ILRSOL = ILRSOL + 1
C
N             SELECT SIDE
              IF NLRSOL.EQ.1 .AND. LBSIDE.LT.0  .OR.
     ?           NLRSOL.EQ.2 .AND. ILRSOL.EQ.1
              THEN
N               LEFT SIDE
                LBSIDE =-1
                XX  = XXL
                YY  = YYL
              ELSE
N               RIGHT SIDE
                LBSIDE = 1
                XX  = XXR
                YY  = YYR
              CIF
C
N             HIT QUALITY:
              LBGOOD = 0
              IF(LBSIDE.NE.LBLR) LBGOOD = 1
N             NEW LAYER?
              IF ILAY.NE.ILAYL .OR. LBGDL.LE.1.AND.LBGOOD.LE.1
              THEN
                LBREG = 1
N               INCREASE HIT COUNTER
                JHIT = JHIT + 1
                IPCO = IPCO + HLDCO
              ELSE
N               2 HITS IN SAME LAYER, SELECT CLOSEST
                LBREG = 0
                IF(LBGOOD.LT.IWRK(IPCO+10)) LBREG = 1
              CIF
N             REGISTER NEW HIT?
              IF LBREG.NE.0
              THEN
                NHIT   = NHIT   + 1
                IF(LBGOOD.LE.1) NHGOOD = NHGOOD + 1
                IWRK(IPCO   ) = ILAY
                IWRK(IPCO+ 1) = IP
                IWRK(IPCO+ 2) = LBSIDE
                WRK (IPCO+ 3) = XX
                WRK (IPCO+ 4) = YY
                WRK (IPCO+ 5) = ZZ
                WRK (IPCO+ 6) = XX
                IWRK(IPCO+ 7) = LZGOOD
                WRK (IPCO+ 8) = DS
                IWRK(IPCO+ 9) = JCELL
                IWRK(IPCO+10) = LBGOOD
                WRK (IPCO+11) = TGB
                IWRK(IPCO+12) = JRING
                WRK (IPCO+13) = 0.
                ILAYL = ILAY
                LBGDL = LBGOOD
                NHTRNG(JRING) = NHTRNG(JRING) + 1
              CIF
C
            UNTIL ILRSOL.GE.NLRSOL
C
          CIF
C
        IPHL = IPHL + 1
        IP   = IP   + 4
        CWHILE
N       SET IPCO TO 1. FREE LOCATION
        IPCO = IPCO + HLDCO
C
N     MASK FOR TRACKS AT CELL WALL + IN DEAD CELLS
C
N       SET LABEL FOR DEAD CELL
        IF NHIT.LE.2
        THEN
          IF DEADCL(JCELL,NRUN)
          THEN
            LBCELL = LOR(LBCELL,MKDDCL(JRING))
            JHIT = 16
            NHIT = 16
C     PRINT 2019, JCELL,JRING,NRUN,LBCELL
          CIF
        CIF
C
      CPROC
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
      XST  = WRK(IHSTRT+ 3)
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
      XEN  = WRK(IHEND + 3)
      YEN  = (PAR1 *XEN + PAR2 )*XEN + PAR3
N     DIRECTION AT START + END POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
      TGEN = PAR1*XEN*2 + PAR2
      DXEN = 1./SQRT(TGEN**2+1.)
      DYEN = DXEN * TGEN
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
      CURV =-PAR1 * 2.
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      IWRK(IP+ 1) = ITRK
      IWRK(IP+ 2) = 16
      IWRK(IP+ 3) = IDAY
      IWRK(IP+ 4) =  0
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2) * TGTH + ZVERT
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)*CSTH
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)*CSTH
      WRK (IP+10) = SNTH
      IWRK(IP+11) = 0
      WRK (IP+12) = XEN *CSROT - YEN *SNROT + XT
      WRK (IP+13) = XEN *SNROT + YEN *CSROT + YT
      WRK (IP+14) = SQRT(WRK(IP+12)**2 + WRK(IP+13)**2) * TGTH + ZVERT
      WRK (IP+15) = (DXEN*CSROT - DYEN*SNROT)*CSTH
      WRK (IP+16) = (DXEN*SNROT + DYEN*CSROT)*CSTH
      WRK (IP+17) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
      IF(SIG  .LT.0) PRINT 2021,WRK(IP+1),S0,SIG
 2021 FORMAT(' REFIT(PST): -VE SQRT:',I4,5E13.5)
      WRK (IP+23) = SIG
      IF(SIG  .GT.0) WRK(IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CURV
      IF(SIG11.LT.0) PRINT 2021,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SIG*SIG11
      IF(WRK(IP+26) .GT. 0) WRK(IP+26) = SQRT(WRK(IP+26))*2.
      WRK (IP+27) = CURV
      WRK (IP+28) = CURV
      I0 = IP+ 1
      I9 = IP+48
C     PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
N     *************************
N     *      F I T B K 1      *
N     *************************
C
C
N     CHANGE FIT BANK (1.POINT)
      PROC FITBK1
C
N     START POINT
      XST  = WRK(IHSTRT+ 3)
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
N     DIRECTION AT START POINT
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
N     MIN. OF PARABOLA
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
C
N     CURVATURE + ERROR
      CURV =-PAR1 * 2.
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
C     PRINT 2012, DET,SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIG
C
C     PRINT 2014, XST,YST,DXST,DYST,TGST,XEN,YEN,DXEN,DYEN,TGEN,CURV,
C    ,            XMIN,YMIN
C
N     FILL FIT-BANK
      IP    = HPTR0 - 1
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2) * TGTH + ZVERT
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)*CSTH
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)*CSTH
      WRK (IP+10) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
      IF(SIG  .LT.0) PRINT 2022,WRK(IP+1),S0,SIG
 2022 FORMAT(' REFIT(PST): -VE SQRT(1):',I4,5E13.5)
      WRK (IP+23) = SIG
      IF(SIG  .GT.0) WRK(IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CURV
      IF(SIG11.LT.0) PRINT 2022,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SIG*SIG11
      IF(WRK(IP+26) .GT. 0) WRK(IP+26) = SQRT(WRK(IP+26))*2.
      WRK (IP+27) = CURV
C     I0 = IP+ 1
C     I9 = IP+48
C     PRINT 2001,(WRK(I1),I1=I0,I9)
      CPROC
C
C
N     *************************
N     *      L A B E L        *
N     *************************
C
C
N     LABEL USED HITS
      PROC LABEL
C
N       PRESET LAST HIT POINTER
        IWL = -999
        FOR IP = HPCO0,HPCO9,HLDCO
          IW0 = IWRK(IP)
          X   = WRK(IP+3)
          Y   = WRK(IP+4)
          F   = (PAR1*X + PAR2)*X + PAR3
          DF  = F - Y
N         SELECT CLOSEST HIT
          LBGOOD = 4
          IF(ABS(DF).LT.ALBLM2) LBGOOD = 1
          IF(ABS(DF).LT.ALBLM1) LBGOOD = 0
          IWRK(IP+ 10) = LBGOOD
          WRK (IP+13) = DF
C
N         CHECK IF 2 HITS FROM SAME WIRE
          IF IWL.EQ.IW0
          THEN
N           SELECT CLOSEST HIT
            IF ABS(DFL).LT.ABS(DF)
            THEN
              IWRK(IP +10) = 16
            ELSE
              IWRK(IPL+10) = 16
            CIF
          CIF
N         STORE LAST POINTERS + DF
          IWL = IW0
          IPL = IP
          DFL = DF
        CFOR
C
      CPROC
C
C
N     *************************
N     *      I N I T          *
N     *************************
C
C
N     INITIALIZE CONSTANTS
      PROC INIT
C
        IQJETC = IBLN('JETC')
        IQHEAD = IBLN('HEAD')
C
        CALL DAY2(DATE)
        IDAY = DATE(1)*1000 + DATE(2)
C
N       MULT. SCATTERING CONSTANTS
        RESMS = .020**2/2. * .16 * (1. + ALOG10(.16) / 9) * 155.45**2
C
N       RADIUS AROUND WIRE FOR CORR. OF DRIFTSPACE
        DRC = RINCR(1)*.5 * DRICOS
C       CONST. FOR VAR. OF DRIFT VEL.
        IPHEAD = IDATA(IQHEAD)*2
        NRUN = HDATA(IPHEAD+10)
        IF NRUN.LE.100
        THEN
          DSD0   = .0
          DSD1   = 5.0
          DSD2   = 5.0
          DRV0   = 1.0
          DRV1   = 1.0
        ELSE
          DSD0   =-.63
          DSD1   = 1.8
          DSD2   = 4.0
          DRV0   = 0.8
          DRV1   = (DSD2 - (DSD1-DSD0)*DRV0) / (DSD2-DSD1)
        CIF
      CPROC
C
      END
