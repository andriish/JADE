C   26/08/87 901131448  MEMBER NAME  DXMCSP   (JADEGS)      FORTRAN77
      SUBROUTINE DXMCSP( NBIN, ISPECT, XBIN )
C----------------------------------------------------------
C  Version of 26/08/87       Last Mod 13/01/89   E Elsen
C  Choose XBIN according to spectrum ISPECT
C  Input:  NBIN   number of bins contained in ISPECT
C          ISPECT differential spectrum (will be integrated)
C  Output  ISPECT integrated spectrum
C          XBIN   bin chosen, interpolated within bins
C  Changes:    remove while expressions     13/1/89  E E
C----------------------------------------------------------
      INTEGER ISPECT(NBIN)
      LOGICAL FIRST / .TRUE. /
      INTEGER IDELMX / 0 /
C
      IF( FIRST ) THEN
        FIRST = .FALSE.
        IBINMX = NBIN/2
        DO 10 I=2,NBIN
          IF( ISPECT(I) .GT. IDELMX ) THEN
             IDELMX = ISPECT(I)
             IBINMX = I
          ENDIF
          ISPECT(I) = ISPECT(I-1) + ISPECT(I)
   10   CONTINUE
      ENDIF
C
      X = RN(DUMMY)
      IBX = X*ISPECT(NBIN)
      IBIN = IBINMX
      NS = 1
      NE = NBIN
  100 CONTINUE
      IF( NS.LT.NE-1 ) THEN
        IF( IBX .LT. ISPECT(IBIN) ) THEN
          NE = IBIN
        ELSE
          NS = IBIN
        ENDIF
        IBIN = (NE+NS)/2
        GO TO 100
      ENDIF
C
      IL = NS
  110 CONTINUE
      IF( IL.GT.1 .AND. ISPECT(IL-1).EQ.ISPECT(NS) ) THEN
        IL = IL - 1
        GO TO 110
      ENDIF
C
      IH = NE
  120 CONTINUE
      IF( IH.LT.NBIN .AND. ISPECT(IH+1).EQ.ISPECT(NE) ) THEN
        IH = IH + 1
        GO TO 120
      ENDIF
C                                           INTERPOLATE
      IF( IH.LT.NBIN ) THEN
        XBIN = FLOAT(IH+1-IL)/(ISPECT(IH+1)-ISPECT(IL))*
     +                (ISPECT(NBIN)*X-ISPECT(IL)) + IL
      ELSE
        XBIN = FLOAT(IH-1-IL)/(ISPECT(IH)-ISPECT(IL-1))*
     +                (ISPECT(NBIN)*X-ISPECT(IL-1)) + IL - 1
      ENDIF
      END
