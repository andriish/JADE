C   05/08/86 609171231  MEMBER NAME  FADCAX   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE FADCAX(NCELL,LINEAR,LDOS,LSEP)
C-----------------------------------------------------------------------
C
C    AUTHOR:   R. RAMCKE     23/10/84 :  DRAWS COORDINATES FOR FADC
C                                        DATA ON SCREEN
C       MOD:   J. HAGEMANN   23/07/85 :  FOR DIFFENCIATED PULSES
C       MOD:   G. ECKERLIN   05/08/86 :  NEW MEMBER FOR JETC DATA
C       MOD:   G. ECKERLIN   18/08/86 :  USER KOORDINATES USED NOW
C       MOD:   G. ECKERLIN   20/08/86 :  WIRE RANGE SELECTION SUPPORTED
C  LAST MOD:   G. ECKERLIN    4/09/86 :  DESPLAY DETAILS IMPLEMENTED
C
C------------------------------------------------------------
C
C
      IMPLICIT INTEGER*2 (H)
C
      character*2 HTX, HCELL, HDIFF,HLIN, HZETM, HZETP, HMW, HDATSV
      LOGICAL LINEAR, LSEP, LDOS
C
      COMMON /CWORK1/ IAMP(300,2), ICWIR, IPED(2,16), NWIRNG(2), IT0
     +                ,ISTWIR(16), IENWIR(16), NP0WIR(16)
     +                ,ISPRE2(8), TPREV(8), ISPRE1(8)
C
C
#include "cgraph.for"
C
      COMMON / CORDER / KWR(17), KSIDE, KSTART, KEND, KTIMB, KEV
      COMMON / CKORD  / RX1, RX2,
     +                  RY1, RY2
      COMMON / CHEADR / HEAD(108)
C
      DIMENSION KANAL(10), KAN(3), IAD(5)
      DIMENSION HDATSV(22), HMW(24), HTX(2), HCELL(2), HDIFF(11)
      DIMENSION HZETM(2), HZETP(2), HLIN(5)
C
      EQUIVALENCE ( HDATSV(1), IDATSV(1) )
C
      DATA KANAL / 48, 49, 50, 51, 52, 53, 54, 55, 56, 57 /
      
      DATA HTX   / 'DS', 'N ' /
      DATA HCELL / 'Ce', 'll' /
      DATA HDIFF / 'di','ff','er','en','ci','at','ed',' p','ul',
     &             'se','s ' /
      DATA HLIN  / 'no','n ','li','ne','ar' /
      DATA HZETM / ' -',' Z' /
      DATA HZETP / ' +',' Z' /
C
C-----------------  C O D E  -------------------------------------------
C
CX    CALL TRMOUT(80,' SUBROUTINE FADCAX^')
C
      KEV    = 1
      DO 2 I = 1,16
         KWR(I) = 1
    2 CONTINUE
      KWR(17) = NWIRNG(2) - NWIRNG(1) + 1
      KSIDE  = 3
      KSTART = 0
      KEND   = 256
      KTIMB  = 50
C
      NTIMB = KTIMB
      IF(KTIMB .EQ. 0) NTIMB = 10
C
      RY1 =  100.
      RY2 = 2950.
      IF( LSEP ) GOTO 10
         RX1 =  600.
         RX2 = 4000.
         GOTO 13
   10 CONTINUE
      LS = 1
      RX1 = 300.
      RX2 = 2100.
      GOTO 13
   12 LS  = 2
      RX1 = 2200.
      RX2 = 4000.
C
   13 IF( LSEP .AND. LS .EQ. 2 ) GOTO 50
C                                      WRITE DSN ON PICTURE
      CALL CHRSIZ(4)
      HMW(1) = HTX(1)
      HMW(2) = HTX(2)
C
      DO 14 I = 1, 22
         HMW(I+2) = HDATSV(I)
   14 CONTINUE
C
      CALL MOVEA(RX1 - 50., RY2 + 200.)
      CALL EOUTST(48, HMW)
C                                      WRITE RUN NUMBER ON PICTURE
      IRUN = HEAD(18)
      CALL INTADE(IRUN, IAD, 5)
      CALL MOVEA(RX1 - 50., RY2 + 50.)
      CALL HLABEL(5,IAD)
C                                      WRITE EVENT NUMBER ON PICTURE
      IEVT = HEAD(19)
      CALL INTADE(IEVT, IAD, 5)
      CALL SEELOC(IXAC, IYAC)
      RXAC = IXAC
      RYAC = IYAC
      CALL MOVEA(RXAC + 30., RYAC)
      CALL HLABEL(5, IAD)
C                                      WRITE CURRENT EVENT NUMBER
      CALL INTADE(ICREC, IAD, 5)
      CALL SEELOC(IXAC, IYAC)
      RXAC = IXAC
      RYAC = IYAC
      CALL MOVEA(RXAC + 30., RYAC)
      CALL HLABEL(5, IAD)
C                                      WRITE CELL NUMBER ON PICTURE
      CALL INTADE(NCELL, IAD, 2)
      CALL MOVEA(RX1 + 1800., RY2 + 200.)
      CALL EOUTST(4, HCELL)
      CALL SEELOC(IXAC, IYAC)
      RXAC = IXAC
      RYAC = IYAC
      CALL MOVEA(RXAC + 50., RYAC)
      CALL HLABEL(2, IAD)
C                                   WRITE 'NON LINEAR PULSES'IF
C                                   CDTL 52 IS NOT SET
      IF ( LINEAR ) GOTO 70
        CALL MOVEA( RX1 + 2150., RY2 + 200. )
        CALL EOUTST( 10,HLIN )
   70 CONTINUE
C                                      WRITE 'DIFFERENTIATED PULSES' ON
C                                      PICTURE IF CDTL51 IS SET
      IF( .NOT. LDOS ) GO TO 50
         CALL MOVEA( RX1 + 2500., RY2 + 200.)
         CALL EOUTST( 22, HDIFF )
C
   50 CALL NLINE(RX1, RY1, RX1, RY2, 0)
C
      IYREL = INT(RY2 - RY1)/KWR(17)
      NLIM  = KWR(17)
C
      DO 60 IW = 1, NLIM
          RYK = INT(RY1) + IYREL*(IW - 1)
C         WRITE (6,9000) IW
 9000     FORMAT(' CALL LINE :',I4)
          CALL NLINE(RX1, RYK, RX2, RYK, 0)
   60 CONTINUE
C
      IF( .NOT. LSEP ) GO TO 75
         CALL CHRSIZ(4)
         CALL MOVEA( RX1 + 800., RY2 + 40.)
         IF( LS .EQ. 1 ) CALL EOUTST( 4, HZETM )
         IF( LS .EQ. 2 ) CALL EOUTST( 4, HZETP )
C
   75 IF( .NOT. LDOS ) GO TO 100
         DO 80 IW = 1, NLIM
            RYK = INT(RY1) + IYREL*(IW - 1) + (IYREL - 50)*24/64
            CALL NLINE( RX1, RYK, RX2, RYK, 12 )
   80    CONTINUE
C
  100 ISCRCH = RX2 - RX1
      KRANGE = KEND- KSTART
      RYUP = RY2
      IF(KTIMB .EQ. 0) RYUP = RY1
      NB = KRANGE/NTIMB
      T0 = RX1 + ISCRCH*IT0/(85 * KRANGE)
      CALL NLINE(T0, RYUP, T0, RY1 - 20.,47)
      DO 200 JB = 1, NB
          RXWID = INT(RX1) + ISCRCH*NTIMB*JB/KRANGE
C         WRITE (6,9002) JB,RXWID,RYUP,RY1
 9002     FORMAT(' CALL LINE :',I5,3F10.3)
          CALL NLINE(RXWID, RYUP, RXWID, RY1 - 20., 38)
          ICHNR = KSTART + NTIMB*JB
          CALL MOVEA(RXWID - 40., RY1 - 50.)
          CALL CHRSIZ(4)
          IF(NTIMB .EQ. 1 .AND. ((ICHNR/2)*2 - ICHNR) .NE. 0) GOTO 200
          IF(ICHNR .GE. 10) GOTO 150
             KAN(1) = KANAL(1)
             KAN(2) = KANAL(1)
             KAN(3) = KANAL(ICHNR + 1)
             GOTO 170
  150     CONTINUE
          IF (ICHNR.GE.100) GOTO 160
            KAN(1) = KANAL(1)
            KAN(2) = KANAL(ICHNR/10 + 1)
            KAN(3) = KANAL(ICHNR -(ICHNR/10)*10 + 1)
            GOTO 170
  160     CONTINUE
          KAN(1) = KANAL(ICHNR/100 + 1)
          IDUM = (ICHNR - (ICHNR/100)*100)/10
          KAN(2) = KANAL(IDUM + 1)
          KAN(3) = KANAL(ICHNR -IDUM*10 -(ICHNR/100)*100 + 1)
  170     CALL HLABEL(3, KAN)
  200 CONTINUE
C
      IF( LSEP .AND. LS .EQ. 1 ) GOTO 12
C
      RETURN
      END
C
      SUBROUTINE INTADE( IN, IA, N )
C------------------------------------------------------------
C     VERSION 30/10/84
C     CONVERTS INTEGER INTO ADE-CODE
C------------------------------------------------------------
      DIMENSION IA(N)
      IZ = 10**(N-1)
      K = IN
      DO 10 J = 1, N
         IA(J) = K/IZ + 48
         K     = K - (K/IZ)*IZ
         IZ    = IZ/10
   10 CONTINUE
C
      IF( N .EQ. 1) GO TO 30
C                           MERGE ZEROES AT BEGIN
      NEND = N - 1
      DO 20 J = 1, NEND
         IF( IA(J) .GT. 48 ) GO TO 30
             IA(J) = 96
   20 CONTINUE
   30 CONTINUE
      RETURN
      END
C
      SUBROUTINE NLINE(RX1, RY1, RX2, RY2, NDRW)
C--------------------------------------------------------------
C     VERSION 7/3/83
C     DRAWS LINE (DASHES) FROM (RX1, RY1)  TO
C     (RX2, RY2)
C--------------------------------------------------------------
C
C     WRITE (6,9001) RX1,RY1,RX2,RY2
 9001 FORMAT(' RX1,RY1,RX2,RY2 :',4F10.3)
      CALL MOVEA(RX1, RY1)
      IF(NDRW .EQ. 0) CALL DRAWA(RX2, RY2)
      IF(NDRW .GT. 0) CALL DASHA(RX2, RY2, NDRW)
      RETURN
      END
