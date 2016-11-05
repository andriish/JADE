C   08/04/87 704102020  MEMBER NAME  EXPATR   (JADEGS)      FORTRAN
      SUBROUTINE EXPATR( NPPATR, LTRNEW, IER )
C-----------------------------------------------------------
C  Version of 08/04/87     last mod 08/04/87  E Elsen
C  Extend PATR bank at BOS position NPPATR to have
C  track data length LTRNEW if shorter, to make room
C  for the covariance matrix.
C  The new positions are set to zero.
C  IER = 0, if no error occured during expansion of bank
C-----------------------------------------------------------
      COMMON / BCS / IW(1)
C                                           CHECK PATR NAME
      INTEGER NAME / 'PATR' /, LTRMAX / 64 /
      DATA NPRT / 30 /, KPRT / 0 /
C
      IF( NPPATR.LE.0 .OR. IW(NPPATR-3) .NE. NAME ) GOTO 9000
C
      LTR=LTRNEW
      IF(LTR.LE.LTRMAX) GOTO 100
         IF(KPRT.GT.NPRT) GOTO 90
            KPRT=KPRT+1
            WRITE(6,20) LTRNEW,LTRMAX
 20         FORMAT(' *** EXPATR *** REQUESTED TRACK LENGTH',I8,
     +      ' IS SET TO MAXIMUM ALLOWED:',I4)
 90      LTR=LTRMAX
100   CONTINUE
C
      L0 = IW(NPPATR+1)
      L1 = IW(NPPATR+3)
      N  = IW(NPPATR+2)
      IP0 = NPPATR + L0
      IF( L1.GE.LTR ) GO TO 8000
C                                           BANK NEEDS EXPANSION
      LOLD = IW(NPPATR)
      CALL BCHM( NPPATR, N*(LTR-L1), IERR )
      IF( IERR.NE. 0 ) GOTO 9100
      IP0 = NPPATR + L0
C                                           ANYTHING BEHIND LAST TRACK?
      IF( LOLD.LE.L0+N*L1) GO TO 1000
C                                           YES, SO COPY
         L = LOLD-(L0+N*L1)
         DO 900 I=1,L
  900       IW(IP0+N*LTR+L-I+1) = IW(IP0+N*L1+L-I+1)
 1000 CONTINUE
C                                           COPY TRACK BY TRACK
      L11 = L1 + 1
      IP9 = IP0 + N*L1
      NP9 = IP0 + N*LTR
 2000 IF( IP9.LE.IP0 ) GO TO 2100
         IP9 = IP9 - L1
         NP9 = NP9 - LTR
         DO 2050 I=1,L1
 2050       IW(NP9+L1+1-I) = IW(IP9+L1+1-I)
C                                      SET NEW ELEMENTS TO ZERO
         DO 2060 I=L11,LTR
 2060       IW(NP9+I)=0
         GO TO 2000
 2100 CONTINUE
      IW(NPPATR+3) = LTR
 8000 IER = 0
      RETURN
C
 9000 IF(KPRT.GT.NPRT) GOTO 9999
         KPRT=KPRT+1
         IF(NPPATR.GT.0) GOTO 9050
            WRITE(6,9010) IW(NPPATR-3)
 9010       FORMAT(' *** EXPATR *** CALLED WITH NAME  ',A4)
            GOTO 9999
 9050    WRITE(6,9060) NPPATR
 9060    FORMAT(' *** EXPATR *** CALLED WITH NONPOSITIVE ',
     +   'BANK POSITION:',I10)
         GOTO 9999
C
 9100 IF(KPRT.GT.NPRT) GOTO 9999
         KPRT=KPRT+1
         WRITE(6,9110) IERR
 9110    FORMAT(' *** EXPATR ***    ERROR CODE FROM BCHM:',I8)
C
 9999 IER=1
      RETURN
      END
